#' export.SPSS
#' EXPORTA TAULA (DATA.FRAME O MATRIX) DE R A SPSS
#' @param taula base de dades
#' @param file.dict nom del fitxer diccionari
#' @param file.save nom de la base de dades guardada en SPSS
#' @param var.keep vector amb el nom de les variables a guardar ('ALL' per guardar-les totes)
#' @param file.runsyntax on està l'executable de SPSS (ó PSPP) per a guardar el fitxer .sav
#' @param file.dades nom del fitxer on es guarden les dades en format txt (default NULL - temporal)
#' @param run.spss executa l'SPSS (ó PSPP) per a generar el .sav (default TRUE)
#' @param dec caràcter decimal

#' @export
export.SPSS<-function(taula,file.dict=NULL,file.save=NULL,var.keep="ALL",file.runsyntax="C:\\Archivos de programa\\SPSS\\runsyntx.exe",file.dades=NULL,run.spss=TRUE,dec="."){


  # file.dict: arxiu de sintaxis de SPSS on hi ha les etiquetes i missings de les variables.
  # file.save: nom en que es vol guardar la base de dades
  # var.keep: nom de les variables que es volen guardar (cadena) separades per un espai en blanc.
  # taula: matrix o data.frame que es vol exportar a SPSS, o llista del tipus data.frame amb formats arreglats (return import.ACCESS).
  # file.spss: arxiu on esta el programa de SPSS que permet llegir sintaxis en un fitxer (runsyntx.exe). ?s una aplicaci? que executa les comandes de SPSS
  # file.dades: arxiu .txt on s'escriuen les dades que posteriorment llegeix l'SPSS. Per defecte ho posa en un arxiu temporal
  # run.spss: TRUE executa i obra el spss, FALSE: noms mostra per pantalla la sintaxis.
  ## si la taula es una matriu la convertim a data.frame
  if (class(taula)!="data.frame") taula<-as.data.frame(taula)


  ## canviem els noms perque siguin compatibles amb SPSS.
  noms<-names(taula)
  noms<-gsub(" ","_",noms)
  noms<-gsub("\\(","_",noms)
  noms<-gsub("\\)","_",noms)
  noms<-gsub("\\.","_",noms)
  names(taula)<-noms

  ## mira variable per variable si t? format.SPSS; si no en te li posa a traves de la funcio arregla.format noms de les variables que no en tenen.
  formats<-unlist(lapply(taula,function(x){res<-attr(x,"format.SPSS"); if (is.null(res)) return("FORMAT.NULL") else return(res)}))

  sense.format<-grep("FORMAT.NULL",formats)
  if (length(sense.format)>0) taula[,sense.format]<-arregla.formats(taula[,sense.format])

  # ara tots tindran format
  formats<-unlist(lapply(taula,function(x) attr(x,"format.SPSS")))

  ## transformacio de les variables data a cadenes que pugui entendre l'SPSS.
  taula.spss<-taula
  for (i in grep("DATE",formats)){
    taula.spss[,i]<-as.character(taula.spss[,i])
    taula.spss[,i]<-gsub("\\)","",taula.spss[,i])
    taula.spss[,i]<-gsub("\\(","",taula.spss[,i])
  }

  # Si hi ha alguna variable amb algun tabulador el substitueix per espais en blanc.
  f.tab<-function(x){
    if (any(is.character(x) | is.factor(x))){
      attr.antic<-attributes(x)
      x<-gsub("\t"," ",as.character(x))
      attr.antic$class<-"character"
      attributes(x)<-attr.antic
      return(x)
    } else return(x)
  }
  taula.spss<-as.data.frame(lapply(taula.spss, f.tab))

  ## escriu la taula en *.txt.
  if (is.null(file.dades)) file.dades<-paste(tempdir(),"dades.txt",sep="\\")
  write.table(taula.spss, file = file.dades, append = FALSE, quote = FALSE, sep = "\t",
	       	  eol = "\n", na = "", dec = dec, row.names = FALSE, col.names = TRUE)

  ## escriu la sintaxis per llegir la taula de ".txt" en spss
  formats.table<-cbind(noms,formats)
  aux=""
  for (k in 1:nrow(formats.table))
    aux<-paste(aux,"\n\t",paste(formats.table[k,],collapse="\t"),sep="")

  sintaxis<-
  ## IMPORTANT: aixo NO ho hem de justificar, perque si ho fem dona error en el runstintax de l'SPSS.
  paste("
GET DATA  /TYPE = TXT
 /FILE = '",file.dades,"'
 /DELCASE = LINE
 /QUALIFIER = ''
 /DELIMITERS = \"\\t\"
 /ARRANGEMENT = DELIMITED
 /FIRSTCASE = 2
 /IMPORTCASE = ALL
 /VARIABLES =",
aux,"
.
CACHE.
EXECUTE.","\n\n",sep="")

  write(sintaxis,file.sintaxis<-file.path(tempdir(),"sintaxis.sps"))

  ## etiquetem les variables.
  if (!is.null(file.dict)){
    for (i in 1:length(file.dict)){
      file.append(file.sintaxis,file.dict[i])
      write("\n\n",file.xxx<-file.path(tempdir(),"xxx.sps"))
      file.append(file.sintaxis,file.xxx)
    }
  } else{
    dictionary.intern(taula,file.xxx<-file.path(tempdir(),"xxx.sps"))
    file.append(file.sintaxis,file.xxx)
  }


  ## on es vol guardar l'arxiu
  if (!is.null(file.save) && length(file.save)>0){
    sintaxi.save<-
    paste(
"SAVE OUTFILE='",file.save,"'
 /KEEP ",var.keep,"
 /COMPRESSED.\n\n",sep="")

    write(sintaxi.save,tempfile.save<-file.path(tempdir(),"sintaxi.save.sps"))
    file.append(file.sintaxis,tempfile.save)

    write(paste("GET FILE='",file.save,"'.",sep=""),file.xxx<-file.path(tempdir(),"xxx.sps"))
    file.append(file.sintaxis,file.xxx)

  }

  ## eliminem les variables que no existeixen en els missings
  sintaxis<-scan(file.sintaxis,what="character",sep="@",quote=NULL)
  index.miss<-grep("MISSING VALUES ",sintaxis)
  if (length(index.miss)>0){
    index.miss.elim<-!lapply(strsplit(sintaxis[index.miss]," "),function(x) x[3])%in%noms
    index.miss.elim<-index.miss[index.miss.elim]
    sintaxis<-sintaxis[-index.miss.elim]
    write(sintaxis,file.sintaxis)
  }


  ## obre l'arxiu de sintaxi en una pantalla de R ##
 file.show(file.sintaxis)


  if (run.spss){

    ## obre l'arxiu de sintaxi ##
   # shell.exec(file.sintaxis)

    ## executa els comandaments de spss.
    cmd=paste("\"",file.runsyntax,"\" ",file.sintaxis,sep="")
    #shell(cmd,mustWork=FALSE)
system(cmd)
  }


}


# Exemple:
#taula=dades
#file.dict=NULL
#file.save="C:\\xxx.sav"
#var.keep="ALL"
#export.SPSS(taula=taula,file.dict=file.dict,var.keep=var.keep,file.save=file.save)


