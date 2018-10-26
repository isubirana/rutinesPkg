#' import.ACCESS2
#' IMPORTA UNA TAULA DE ACCESS I INCORPORA DICCIONARI DE VARIABLES I FA UNA EXPORTACIO A SPSS
#' @param file.mbd nom del fitxer access
#' @param nom.taula vector amb el nom de les taules a llegir
#' @param nom.variables llista de vectors amb els noms de les variables a llegir de cada taula
#' @param nom.dicc nom de la taula diccionari
#' @param file.spss nom del fitxer spss si es vol guardar en SPSS
#' @param var.dicc vector amb els noms de les variables del diccioanri amb l'ordre noom, etiqueta variable, etiqueta valors, taula
#' @param noms.taules vector amb el nom de les taules com es guardaran a R
#' @param fix.formats arregla els formats de les variables (default TRUE)
#' @import RODBC chron
#' @export

import.ACCESS2<-function(file.mbd,nom.taula,nom.variables=list("ALL"),nom.dicc="",file.spss="",var.dicc="",noms.taules=gsub(" ","_",nom.taula),fix.formats=TRUE){

  # nom.taula<-import.ACCESS(file=file.mbd,nom.taula=nom.taula,nom.variables=nom.variables,nom.taula.R=noms.taules)
  taules<-import.ACCESS(file=file.mbd,nom.taula=nom.taula,nom.variables=nom.variables,nom.taula.R=noms.taules)


  if (nom.dicc!=""){ # si existeix una taula de diccionari etiqueta la/es taula/es
    # nom.dict<-import.ACCESS(file=file.mbd,nom.taula=nom.dicc)
    # assign("dict",get(nom.dict))
    dict<-import.ACCESS(file=file.mbd,nom.taula=nom.dicc)
    names(dict)<-tolower(names(dict))
    dict<-arregla.formats(dict)
  }

  # for (i in 1:length(nom.taula)){
  for (i in 1:length(taules)){

    # assign("taula",get(nom.taula[i]))
    taula <- taules[[i]]
    names(taula)<-tolower(names(taula))
    nom.taula.access<-attr(taula,"table.origen")

    if (nom.dicc!=""){ # va etiquetant les taules

      dict2<-dict
      names(dict2)<-tolower(names(dict2))
      var.dicc<-tolower(var.dicc)

      if (length(var.dicc)==4)
        dict2<-subset(dict,tolower(dict[,var.dicc[4]])==tolower(nom.taula.access))  # els noms de les taules no son case-sensitive.

      dict2[,var.dicc[1]]<-tolower(as.character(dict2[,var.dicc[1]]))

      comuns<-names(taula)[names(taula)%in%dict2[,var.dicc[1]]]
      comuns<-as.character(comuns)
      dict2<-subset(dict2,dict2[,var.dicc[1]]%in%comuns)

      noms.table<-table(dict2[,var.dicc[1]])
      noms.repes<-names(noms.table)[noms.table>1]
      dict2.repe<-subset(dict2,dict2[,var.dicc[1]]%in%noms.repes)
      dict2.repe<-dict2.repe[order(dict2.repe[,var.dicc[1]]),]
      if (length(noms.repes)>0){
        cat("Advertencia: les variables",paste(noms.repes,collapse=","),"no s'han etiquetat perque estan repetits\n")
        #print(dict2.repe)
        #fix2(dict2.repe)
        cat("\n")
        dict2<-subset(dict2,!dict2[,var.dicc[1]]%in%noms.repes)
        comuns<-names(taula)[names(taula)%in%dict2[,var.dicc[1]]]
        comuns<-as.character(comuns)
      }

      dict2[,var.dicc[2]]<-gsub("\"","",as.character(dict2[,var.dicc[2]]))

      for (j in which(names(taula)%in%comuns)){
        index<-which(dict2[,var.dicc[1]]==names(taula)[j])
        attr(taula[,j],"vari.label")<-dict2[index,var.dicc[2]]
        value.labels<-as.character(dict2[index,var.dicc[3]])
        if (!is.na(value.labels) & value.labels!=""){
          value.labels<-gsub(" *= *","=",value.labels)
          value.labels<-gsub(" *; *",";",value.labels)
          value.labels<-gsub(";",",\"",value.labels)
          value.labels<-gsub("=","\"=",value.labels)
          value.labels<-paste("value.labels=c(\"",value.labels,")",sep="")
          print(dict2[index,var.dicc[1]])
          eval(parse(text=value.labels))
          attr(taula[,j],"value.labels")<-value.labels
        }
      }
    }

    if (fix.formats)
      taula<-arregla.formats(taula,force=TRUE)

    if (file.spss!=""){ # si es vol exportar a SPSS file.spss ha d'estar especificat
      export.SPSS(taula,file.save=ifelse(file.spss=="",choose.files(".sav",paste(nom.taula.access,"Guardar com")),file.spss[i]))
    }

    assign(noms.taules[i], taula, env=.GlobalEnv)

  }

  return(noms.taules)

}




