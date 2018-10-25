#' export.ACCESS
#' FUNCIO QUE EXPORTA UNA TAULA (data.frame) A UNA BASE DE DADES EXISTENT! DE ACCESS. A MES, POSA LES VARIABLES DATA EN FORMAT DATA!!!
#' TAMBE (OPCIONAL) CREA UNA TAULA AMB EL DICCIONARI (VARI LABELS, VALUE LABELS) DE LES VARIABLES DE TAULA.
#' @param taula data.frame amb les dades per a exportar a ACCESS
#' @param file.mdb character, especifica el path i el filename de la base de dades EXISTENT! de ACCESS on va a parar 'taula'
#' @param table.name nom de la taula en el ACCESS
#' @param table.dict NULL - no crea cap diccionari, !NULL - crea una taula que sera el diccionari de 'taula' amb el nom especificat com a caracter.
#' @param fix.formats arreglar formats (default TRUE)
#' @note PER A MILLORAR: DE MOMENT NO ARREGLA LES VARIABLES EN FORMAT DATA AMB HORES:MINUTS:SEGONS (EN ACCESS QUEDA COM UN REAL PROCEDENT DEL FORMAT DE R).

#' @export
export.ACCESS<-function(taula,file.mdb,table.name,table.dict=NULL,fix.formats=TRUE){

#taula<-dades
#file.mdb<-"C:/isaac/bd1.mdb"
#table.name<-"parto"
#table.dict=NULL
#fix.formats=TRUE


  if (length(grep("\\.",names(taula)))){
    names(taula)<-gsub("\\.","_",names(taula))
    warning("S'han substituir els punts per \"_\" en els noms de les variables")
  }

  rename.file<-function(filename,exfilenames){
    k<-1
    orig.filename<-filename
    print(filename)
    print(exfilenames)
    while(filename%in%exfilenames){
      filename<-paste(orig.filename,k,sep="_")
      k<-k+1
    }
    if (k>1) warning("La taula '",orig.filename,"' ja existeix. S'ha creat la taula '",filename,"'")
    return(filename)
  }

  if (ncol(taula)>255) stop("Hi ha mes de 255 variables a la taula")

  if (fix.formats) taula<-arregla.formats(taula) # per assegurar que tingui atributs de formats.

  convert.date.ACCESS<-function(var.date){   # funcio que converteix les variables data a cadena en un format que li agrada a ACCESS (dd/mm/yyyy)
    vari.label<-attr(var.date,"vari.label")
    value.labels<-attr(var.date,"value.labels")
    dies<-as.integer(days(var.date))
    mesos<-as.integer(months(var.date))
    anys<-as.integer(as.character(years(var.date)))
    var.date<-apply(cbind(dies,mesos,anys),1,paste,collapse="/")
    var.date<-ifelse(var.date=="NA/NA/NA",NA,var.date)
    attr(var.date,"vari.label")<-vari.label
    attr(var.date,"value.labels")<-value.labels
    return(var.date)
  }

  index.data<-NULL
  for (i in 1:ncol(taula)){
    if (!is.null(attr(taula[,i],"format.SPSS")) && attr(taula[,i],"format.SPSS")=="DATE11"){
      taula[,i]<-convert.date.ACCESS(taula[,i])
      index.data<-c(index.data,i)
    }
  }

  canal<-odbcConnectAccess(file.mdb)

  noms.taules<-sqlTables(canal)

  table.name<-rename.file(table.name,noms.taules[,"TABLE_NAME"])   # reanomena la taula si la taula ja exiteix afegint '_1' al final.

  exist<-TRUE;  k<-1
  while(exist & k<1000){
    taula.temp1<-paste(sample(letters,10),collapse="")
    exist<-taula.temp1%in%noms.taules
  }
  if (k==1000) stop("taula.temp1 ja existeix")
  exist<-TRUE;  k<-1
  while(exist & k<1000){
    taula.temp2<-paste(sample(letters,10),collapse="")
    exist<-taula.temp2%in%noms.taules
  }
  if (k==1000) stop("taula.temp2 ja existeix")

  sqlSave(canal, taula, tablename = taula.temp1, rownames = FALSE)

  if (length(index.data)>0){
    query<-paste(taula.temp1,names(taula),sep=".")
    query[index.data]<-paste("IIf([",taula.temp1,"]![",names(taula)[index.data],"] Is Null,Null,CDate([",taula.temp1,"]![",names(taula)[index.data],"])) AS d_",names(taula)[index.data],sep="")
    query<-paste(query,collapse=", ")
    query<-paste("SELECT ",query," INTO ",taula.temp2," FROM ",taula.temp1,";",sep="")
    sqlQuery(canal,query)
    #query<-paste("[",paste(taula.temp2,names(taula),sep="]!["),"]",sep="")
    query<-names(taula)
    #query[index.data]<-paste(taula.temp2,".d_",names(taula)[index.data]," AS ",names(taula)[index.data],sep="")
    query[index.data]<-paste("d_",names(taula)[index.data]," AS ",names(taula)[index.data],sep="")
    query<-paste(query,collapse=", ")
    query<-paste("SELECT ",query," INTO ",table.name," FROM ",taula.temp2,";",sep="")
    invisible(sqlQuery(canal,query))
    #query<-paste("SELECT * INTO ",table.name," FROM ",taula.temp2,";",sep="")
    #invisible(sqlQuery(canal,query))
    sqlDrop(channel=canal, sqtable=taula.temp2)
  } else {
    query<-paste("SELECT * INTO ",table.name," FROM ",taula.temp1,";",sep="")
    invisible(sqlQuery(canal,query))
  }

  sqlDrop(channel=canal, sqtable=taula.temp1)

  if (!is.null(table.dict)){
    table.dict<-rename.file(table.dict,noms.taules[,"TABLE_NAME"])   # reanomena la taula diccionari si ja exiteix afegint '_1' al final.
    value.labels<-NULL
    for (i in 1:ncol(taula)){
      temp<-attr(taula[,i],"value.labels")
      if (!is.null(temp)){
        temp<-sort(temp)
        temp<-paste(paste(names(temp),"=",temp,sep=""),collapse=";")
      } else {
        temp<-""
      }
      value.labels<-c(value.labels,temp)
    }
    vari.label<-as.character(lapply(taula,function(x) attr(x,"vari.label")))
    dict<-data.frame(varname=names(taula),label=vari.label,valuelabels=value.labels,table=table.name)

    sqlSave(canal, dict, tablename = table.dict, rownames = FALSE)
  }

  odbcClose(canal)

}


# Exemple
#
#source("U:\\ULEC\\Software\\R cran project\\rutines\\fix2.r")
#source("U:\\ULEC\\Software\\R cran project\\rutines\\read.spss4.r")
#dades<-read.spss4("U:\\ULEC\\Productes ULEC\\REGICOR\\Regicor\\TRABAJO\\salida_CASO.sav")
#
#export.ACCESS(dades[,1:10],"C:\\Documents and Settings\\isubirana\\Escritorio\\TRANS00.mdb","ALIM","dict")
#
#source("U:\\ULEC\\Software\\R cran project\\rutines\\import.ACCESS2.r")
#import.ACCESS2(file.mbd="C:\\Documents and Settings\\isubirana\\Escritorio\\TRANS00.mdb",
#  nom.taula="holaquetal",
#  nom.variables = list("ALL"),
#  nom.dicc = "dict_hola",
#  file.spss = "",
#  var.dicc = c("varname","label","valuelabels","table"),
#  noms.taules = "hola",
#  fix.formats = TRUE)
