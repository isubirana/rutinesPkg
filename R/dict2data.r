#'dict2data
#' incorpora un fitxer .sps de diccionari de variables ('file.dict') al data.frame 'dades' ###
#' i retorna dades etiquetat (vari.labels i value.labels) ########
#'@param file.dict nom del fitxer del diccionari
#'@param dades dades

#'@export
dict2data<-function(file.dict,dades){


  dict<-scan(file=file.dict,what="character",sep="\n")
  dict<-tolower(dict)
  names(dades)<-tolower(names(dades))

  # variable labels #

  begin<-grep("vari labels",dict,ignore.case=TRUE)

  if (length(begin)>0){
    for (j in begin){
      i=j+1
      linia<-dict[i]
      repeat{
        aux<-unlist(strsplit(linia,"\t"))
        aux<-aux[aux!=""]
        nom.var=trim(aux[1])
        vari.label=trim(aux[2])
        vari.label=gsub("\"","",vari.label)
        if (nom.var%in%names(dades)){
          attr(dades[,nom.var],"vari.label")=vari.label
        }
        if (substr(trim(linia),nchar(trim(linia)),nchar(trim(linia)))==".") break
        i=i+1
        linia<-dict[i]
      }
    }
  }


  # value labels #


  begin<-grep("value labels",dict,ignore.case=TRUE)

  if (length(begin)>0){
    for (j in begin){
      i=j
      linia<-dict[i]
      nom.var<-trim(sub("value labels","",linia))
      if (nom.var%in%names(dades)){
        i=i+1
        linia<-dict[i]
        value.label.sint="c("
        repeat{
          aux<-unlist(strsplit(linia,"\t"))
          aux<-aux[aux!=""]
          value=aux[1]
          label=aux[2]
          value.label.sint<-paste(value.label.sint,",","",label,"=",value,sep="")
          if (substr(trim(linia),nchar(trim(linia)),nchar(trim(linia)))=="."){
            value.label.sint<-sub("\"\\.=","\"=",value.label.sint)    # traiem el punt.
            break
          }
          i=i+1
          linia<-dict[i]
        }
        value.label.sint<-paste(value.label.sint,")",sep="")
        value.label.sint<-sub("c\\(,","c(",value.label.sint)
        eval(parse(text=paste("attr(dades[,nom.var],\"value.labels\")=",value.label.sint,sep="")))
        names(attr(dades[,nom.var],"value.labels"))<-gsub("\\.$","",names(attr(dades[,nom.var],"value.labels"))) # traiem el punt que queda al final.
      }
    }
  }


  # missing values #
  #.....

  return(dades)

}



