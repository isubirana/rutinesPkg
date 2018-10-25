#' import.ACCESS
#' for internal use only. Use import.ACCESS2
#' @export


import.ACCESS<-function(file,nom.taula,nom.variables=list("*"),nom.taula.R=nom.taula){  # per defecte ho fa interactivament

  # file.mbd: path i nom de la base de dades de ACCESS on estan les taules
  # nom.taula: vector amb els noms de les taules a seleccionar
  # nom.variables: llista de vectors caracter especificant les variables a seleccionar per a cada taula (si es vol totes les variables posar list("*")
  # nom.taula.R: vector de la mateixa llargada que nom.taula amb els noms assignats a R. Per defecte posa els mateixos noms que a ACCESS
  #     per? si no s?n comptatibles amb R caldr? modificarlos (espais en blanc, ')', '(', etc... )


  # arregla els noms de les taules perqu? siguin compatibles amb R
  nom.taula.R=gsub(" ","_",nom.taula.R)
  nom.taula.R=gsub("\\)","_",nom.taula.R)
  nom.taula.R=gsub("\\(","_",nom.taula.R)

  canal <- odbcConnectAccess(file)

  for (i in 1:length(nom.taula)){

    nom.variables[[i]]<-ifelse(nom.variables[[i]]=="ALL","*",nom.variables[[i]])
    query<-paste("select ",paste(paste("[",nom.taula[i],"].[",nom.variables[[i]],"]",sep=""),collapse=", ")," from [",nom.taula[i],"];",sep="")
    query<-sub("\\.\\[\\*\\]",".*",query)

    cat("\n--- Llegint les variables: ----\n")
    cat("    ",paste(nom.variables[[i]],collapse=", "))
    cat("\n     de la taula",nom.taula[i],"\n\n")

    taula<-sqlQuery(canal,query)

    if (!is.character(taula)){ ## @@@ arreglar quan la consulta a ACCESS doni error (taula ser? un character).......

      ## arreglem els formats data convertint a chron

      apply.formats<-function(var){

        formats=NULL
        attr(var,"formats.arreglats")<-FALSE

        ## FORMATS VARIABLE BUIDA ##
        if (is.null(formats) && sum(is.na(var))==nrow(taula)){
          formats="F8.2"
          attr(var,"formats.arreglats")<-TRUE
        }

        ## FORMATS DATA ##
        if (is.null(formats) && !is.null(class(var)) && sum(class(var)%in%c("POSIXt","POSIXct"))>0){ # definits com a data en ACCESS
          aux<-strsplit(as.character(var)," ")
          f<-function(x){
            if (any(is.na(x))) return(NA)
            if (!any(is.na(x)) & length(x)==1) return(chron(dates=x,format=c(dates="y-m-d"),out.format=c(dates="d-mon-Y")))
            if (!any(is.na(x)) & length(x)==2){
              dates<-unlist(strsplit(x," "))[1]
              times<-unlist(strsplit(x," "))[2]
              return(chron(dates,times,format=c(dates="y-m-d",times="h:m:s"),out.format=c(dates="d-mon-Y",times="h:m:s")))
            }
          }
          aux<-unlist(lapply(aux,f))
          if (any(aux-trunc(aux),na.rm=TRUE)>0){
            var<-chron(aux,format=c(dates="d-mon-Y",times="h:m:s"))
            formats<-"DATETIME23"
          }else{
            var<-chron(aux,format=c(dates="d-mon-Y"))
            formats<-"DATE11"
          }
          attr(var,"formats.arreglats")<-TRUE
        }

        ## FORMATS ENTER ##
        if (is.null(formats) && length(class(var))==1 && class(var)=="integer"){
          if (sum(is.na(var))==nrow(taula)) formats="F8.2"    ## buida
          else formats=paste("F",max(nchar(as.character(var)),na.rm=TRUE),".0",sep="")
          attr(var,"formats.arreglats")<-TRUE
        }

        ## FORMAT NUM?RIC AMB DECIMALS ##
        if (is.null(formats) && length(class(var))==1 && class(var)=="numeric" && is.double(var)){
          if (sum(is.na(var))==nrow(taula)) formats="F8.2"    ## buida
          else{
            num_dec<-max(nchar(as.character(abs(var-trunc(var))))-2,na.rm=TRUE)
            num_dec<-ifelse(num_dec>16,16,num_dec)
            num_dec<-ifelse(num_dec<0,0,num_dec)
            num_ent<-max(nchar(as.character(trunc(var))),na.rm=TRUE)
            formats<-paste("F",num_dec+num_ent+2,".",num_dec,sep="")
          }
          attr(var,"formats.arreglats")<-TRUE
        }

        ## ALTRES FORMATS: FORMAT CADENA (EN R COM A FACTOR) ##
        if (is.null(formats))
          formats=paste("A",max(nchar(as.character(var)),na.rm=TRUE),sep="")

        attr(var,"format.SPSS")<-formats

        return(var)

      }

      taula<-as.data.frame(lapply(taula,apply.formats))

      # afegim els atributs no est?ndards (els estandards s?n class, names i rownames)
      attributes(taula)$file.origen=file
      attributes(taula)$table.origen=nom.taula[i]
      attributes(taula)$import.origen<-"ACCESS"


      assign(nom.taula.R[i], taula, env=.GlobalEnv)

    } else cat("\n\n>Error: No ha pogut llegir la taula '",nom.taula[i],"' Motiu:",paste(taula,collapse=" "))  # no arreglar si no pot fer la consulta

  } ## fi bucle per a totes les taules

  odbcClose(canal) # tenco el canal a la base de dades de ACCESS.

  return(nom.taula.R)

}


### COMENTARIS ###

# -. tot i que a ACCESS estigui definit com a text, si una variable es pot llegir com un enter, R li assigna la classe integer.






##  EXEMPLE ##

#file.mdb="U:\\Estudis\\Epidemiologia\\HERMES\\Dades\\TELEFORM\\HERMES.mdb"
#nom.taula=c("NO PARTICIPANTS","RECLUTAMENT","LAB")
#taules<-import.ACCESS(file.mdb=file.mdb,nom.taula=nom.taula)
### per obtenim la primera de les taules importades
# get(taules[1])

#res<-arregla.formats(get(taules[1]))
