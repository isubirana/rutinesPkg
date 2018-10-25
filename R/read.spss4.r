#' read.spss4
#' reads SPSS files detecting automatically date variables and transforming them to chron format
#' @param file .sav file
#' @param elimina.miss elimina els missing definits per l'usuari en el fitxer SPSS (default TRUE)
#' @param tolower converteix els noms a minúscules (default TRUE)
#' @param use.value.labels converteix els codis a etiquetes (similar a read.spss de foreign)
#' @param keep.var vector de les variables a llegir (default 'ALL')
#' @param date.format format de les dates (veure 'out.format' de la funció 'chron')
#' @param reencode codi UTF-8, latin1, ...
#' @param ... further arguments passed to `read.spss`
#' @import chron foreign
#' @export


read.spss4<-function(file,elimina.miss=TRUE,tolower=TRUE,use.value.labels=FALSE,keep.var="ALL",date.format="d-mon-Y",reencode=NA,...){

  varlist<-spss_varlist(file)  # retorna una matriu amb el diccionari de variables.
  if (!is.na(reencode)){
    Encoding(varlist)<-reencode
  }

  ######## LLEGEIX LA BASE DE DADES ########

  dades<-try(read.spss(file,use.value.labels=use.value.labels,to.data.frame=FALSE,trim.factor.names=FALSE,reencode=reencode,...),silent=TRUE)
  if (is.character(dades)){
    cmd=paste("\"",file.path(getwd(),"spssfix.exe"),"\" ",file," ",temp.file<-file.path(tempdir(),"xxx.sav"),sep="")
    cat("-Program 'fixspss' executed to read variable names containing strange characters\n")
    shell(cmd,mustWork=FALSE)
    cat("\n\n")
    dades<-read.spss(temp.file,use.value.labels=FALSE,to.data.frame=FALSE,trim.factor.names=FALSE,reencode=reencode,...)
  }
  short.names<-names(dades)

  # per NO convertir les variables car?cter (format AXX) en factors, cal protegir la/es variables en q?esti? amb la funci? I().
  formats<-varlist[,"printfmt"]
  index.char<-grep("^A",formats)
  if (length(index.char)>0){
    aux<-paste("dades[[\"",names(dades),"\"]]",sep="")
    aux[index.char]<-paste("I(",aux[index.char],")",sep="")
    eval(parse(text=paste("dades<-data.frame(",paste(aux,collapse=","),")",sep="")))
  } else dades<-data.frame(dades)

  # elim s?n les variables que s'han d'eliminar ja que estan duplicades per culpa que alguna variable cadena t? un format molt llarg (A>250).
  if (!is.null(attr(varlist,"elim"))){
    elim<-attr(varlist,"elim")
    dades<-dades[,-elim,drop=FALSE]
  }

  keep.var.index <- if (length(keep.var)==1 && keep.var=="ALL") 1:ncol(dades) else which(tolower(varlist[,"longname"])%in%tolower(keep.var))

  keep.var.orig<-keep.var
  keep.var<-if (length(keep.var.index)) varlist[keep.var.index,"longname"] else character(0)

  if (keep.var.orig!="ALL" || length(keep.var.orig)>1){
    var.missing<-keep.var.orig[!tolower(keep.var.orig)%in%tolower(varlist[,"longname"])]
    if (length(var.missing)==length(keep.var.orig)) stop(paste("None of variables specified in 'keep.var' found in dataset",basename(file),"'",sep=""))
    if (length(var.missing)<length(keep.var.orig) & length(var.missing)) warning("Variables '",paste(var.missing,collapse="','"),"' are not found in '",basename(file),"'")
  }

  varlist<-varlist[keep.var.index,]

  dades<-dades[,keep.var.index]


  #### ASSIGNA ATRIBUTS A LES VARIABLES #######

  for (i in 1:ncol(dades)){
    attr(dades[,i],"vari.label")<-varlist[i,"varlabel"]
    attr(dades[,i],"formats.arreglats")<-FALSE
    miss.values<-as.double(varlist[i,3:6])       ## els missings es posen a punt per a ser arreglats amb la funci? arregla.miss.values
    if (miss.values[1]==0) attr(dades[,i],"miss.values")=NULL
    else{
      attr(dades[,i],"miss.values.arreglat")=FALSE       # 0 missings
      if (miss.values[1]==1) attr(dades[,i],"miss.values")=miss.values[2]   # 1 missing
      if (miss.values[1]==2) attr(dades[,i],"miss.values")<-miss.values[2:3]  # 2 missings.
      if (miss.values[1]==3) attr(dades[,i],"miss.values")<-miss.values[2:4] #  3 missings.
      if (miss.values[1]==-2) attr(dades[,i],"miss.values")=paste(miss.values[2:3],collapse=":")    # [a:b] ?s missing
      if (miss.values[1]==-3) attr(dades[,i],"miss.values")=c(paste(miss.values[2:3],collapse=":"),miss.values[4])   # el conjunt {[a:b],c} es missing
    }
    attr(dades[,i],"format.SPSS")<-varlist[i,"printfmt"]
  }


  ######## AFEGEIX ATRIBUTES AL DATA.FRAME (DADES) ######

  # els que te per defecte (en fer read.spss) son:
  # -. class: DATA.FRAME
  # -. names: noms curts (els hi posem despr?s els llargs) de les variables
  # -. row.names: noms (per defecte consecutius, abans de fer cap filtre) dels registres o files
  # -. variable.labels: etiquetes de les variables.

  attributes(dades)$file.origen=file  # de quin fitxer prov?.
  attributes(dades)$import.origen<-"SPSS"   # possibilitats: SPSS,ACCESS,TXT,R
  attributes(dades)$short.names<-structure(short.names,names=names(dades))   # noms curts que llegeix read.spss (o altres programes com SAS?)


  ######## ARREGLA FORMATS DATA (CONVERTEIX LES DATES DE SPSS A FORMAT CHRON com "dd/mm/aaaa" ? "dd/mm/aaaa h:m:s") #########

  formats<-varlist[,"printfmt"]
  formats<-gsub("[0-9.]+","",formats)    # formats tipus ("A","F","DATE" o "DATETIME").

  for (i in which(formats=="DATE" | formats=="EDATE")){     # formats dd/mm/yyyy
    antic<-dades[,i]
    dades[,i]<-ifelse(dades[,i]==1,NA,dades[,i]) ## read.spss llegeix els missings de les variables data com un '1'.
    aux<-format(ISOdate(1582,10,14)+dades[,i],"%d/%m/%Y")
    dades[,i]<-chron(dates=aux,format=c(dates="d/m/y"),out.format=c(dates=date.format))
    # recuperem les etiquetes de valor
    aux<-attr(antic,"value.labels")
    if (!is.null(aux)){
      aux<-format(ISOdate(1582,10,14)+aux,"%d/%m/%Y")
      aux<-chron(aux,format=c(dates="d/m/y"),out.format=c(dates=date.format))
      attr(dades[,i],"value.labels")<-aux
      attr(dades[,i],"value.labels")<-structure(aux,names=names(attr(antic,"value.labels")))
    }
    # tornem a posar els altres atributs perduts en aquest proc?s
    attr(dades[,i],"vari.label")<-attr(antic,"vari.label")
    attr(dades[,i],"format.SPSS")<-"DATE11"
  }
  for (i in which(formats=="DATETIME")){    # formats dd/mm/yyy hh:mm:ss
    antic<-dades[,i]
    aux<-format(ISOdate(1582,10,14,0,0,0)+dades[,i],"%d/%m/%Y %H:%M:%S")
    aux<-strsplit(aux," ")
    dates<-unlist(lapply(aux,function(x) x[1]))
    times<-unlist(lapply(aux,function(x) x[2]))
    dades[,i]<-chron(dates=dates,times=times,format=c(dates="d/m/y",times="h:m:s"),out.format=c(dates=date.format,times="h:m:s"))
    # recuperem les etiquetes de valor
    aux<-attr(antic,"value.labels")
    if (!is.null(aux)){
      aux<-format(ISOdate(1582,10,14,0,0,0)+as.double(aux),"%d/%m/%Y %H:%M:%S")
      aux<-strsplit(aux," ")
      dates<-unlist(lapply(aux,function(x) x[1]))
      times<-unlist(lapply(aux,function(x) x[2]))
      aux<-chron(dates,times,format=c(dates="d/m/y",times="h:m:s"),out.format=c(dates=date.format,times="h:m:s"))
      attr(dades[,i],"value.labels")<-aux
      attr(dades[,i],"value.labels")<-structure(aux,names=names(attr(antic,"value.labels")))
    }
    # tornem a posar els altres atributs perduts en aquest proc?s
    attr(dades[,i],"vari.label")<-attr(antic,"vari.label")
    attr(dades[,i],"format.SPSS")<-"DATETIME23"
  }

  ######### ELIMINA MISSINGS #########

  if (elimina.miss){
    temp<-arregla.miss.values(dades)
    dades<-as.data.frame(temp)
    names(dades)

  } ## OBSERVACI?: una base de dades tindr? missings (per defecte arrossegats de l'SPSS) quan alguna de les seves variables tingui l'atribut miss.values no null

  names(dades)<-varlist[,"longname"]  # posa el nom "aut?ntic", encara que tingui m?s de 8 car?cters i tal com est? en SPSS en quan a min?scules o maj?scules.
  if (tolower) names(dades)<-tolower(names(dades))  # si volem passar els noms a min?scula.

  return(dades)

}



############ COMENTARIS ##############

# aquesta funci? es basa en read.spss(foreign) que, entre altres coses, fa el seg?ent.
# -. les variables format data ("DATE, DATETIME,...") els tracta com a num?rica (segons des de 14/10/1580)
# -. les variables format num?ric (FX.X) les tracta com a num?riques.
# -. les variables format cadena (AX) les tracta com a factor. A m?s, els valors "" NO els considera NA; per arreglar aix?, si es vol, s'ha
#     d'aplicar la funci? arregla formats.

# el format de les dates de les variables DATE estan en chron day-mon-year.
