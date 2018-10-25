#'dict.ACCESS
#'crea un fitxer de diccionari de variables (millor txt) per a importar a ACCESS d'uns fitxers de sps (en el mateix format del que dona la funci? dictionary.r)
#'@param files.sps fitxer sintaxis SPSS
#'@param file.res nom del fitxer de resultats
#'@param nom.taules nom de les taules

#'@export
dict.ACCESS<-function(files.sps,file.res,nom.taules=gsub(".sps$","",basename(files.sps))){

  #### funci? pr?via ####

  dict.ACCESS.aux<-function(file.dict,nom.taula,append,col.names){

    dict<-scan(file=file.dict,what="character",sep="\n",quote="")
    dict<-tolower(dict)

    cat("------------------------------------------------------------------------------",
      "\n","Posant les etiquetes de la taula:",nom.taula,"\n","de l'arxiu de sintaxis:",basename(file.dict),"\n\n")

    begin.value.labels<-grep("^value labels",dict)

    if (length(begin.value.labels)){ # si t? value labels
      value.labels<-matrix(NA,length(begin.value.labels),2)
      k=1
      for (i in begin.value.labels){
        nom.var<-sub("value labels ","",dict[i])
        j=1
        linia<-dict[i+j]
        linia<-unlist(strsplit(linia,"\t",linia))
        linia<-linia[linia!=""]
        linia<-rev(linia)
        if (length(linia)==1) linia=c(linia," ")
        linia<-paste("",sub("\\.$","",trim(linia[1])),"=",trim(linia[2]),sep="")
        resu<-linia
        while(!length(grep("\\.$",linia<-dict[i+j]))){
          j=j+1
          linia<-dict[i+j]
          linia<-unlist(strsplit(linia,"\t",linia))
          linia<-linia[linia!=""]
          linia<-rev(linia)
          linia<-paste("",sub("\\.$","",trim(linia[1])),"=",trim(linia[2]),sep="")
          resu<-paste(resu,"; ",linia,sep="")
        }
        value.labels[k,1]=nom.var
        value.labels[k,2]=resu
        k=k+1
        if (k>50) stop("Hi ha algun error") # massa etiquetes de valor
      }
      value.labels<-data.frame(list(var=value.labels[,1],value.label=value.labels[,2]))
    }

    begin.vari.label<-grep("^vari labels",dict)
    aux1<-aux2<-NULL

    k=1
    i=begin.vari.label
    j=1
    linia<-dict[i+j]
    linia<-unlist(strsplit(linia,"\t",linia))
    linia<-linia[linia!=""]
    aux1<-c(aux1,trim(linia[1]))
    aux2<-c(aux2,gsub("\\.$","",trim(linia[2])))
    while(!length(grep("\\.$",linia<-dict[i+j]))){
      j=j+1
      linia<-dict[i+j]
      linia<-unlist(strsplit(linia,"\t",linia))
      linia<-linia[linia!=""]
      aux1<-c(aux1,trim(linia[1]))
      aux2<-c(aux2,gsub("\\.$","",trim(linia[2])))
      k=k+1
      if (k>5000) stop("hi ha algun error") # massa variables
    }
    vari.labels<-cbind(aux1,aux2)

    vari.labels<-data.frame(list(var=vari.labels[,1],vari.label=vari.labels[,2]))

    if (length(begin.value.labels)) res.dict<-merge(vari.labels,value.labels,all.x=TRUE,by="var",sort=FALSE)
    if (!length(begin.value.labels)) res.dict<-cbind(vari.labels,rep(" ",nrow(vari.labels)))

    res.dict<-cbind(res.dict,rep(nom.taula,nrow(res.dict)))

    colnames(res.dict)<-c("var","label","value.labels","table")

    write.table(res.dict,file.res,sep="\t",row.names=FALSE,col.names=col.names,na="",quote=FALSE,append=append)

  }

  #### fi funci? pr?via ####


  for (i in 1:length(files.sps))
    dict.ACCESS.aux(files.sps[i],nom.taules[i],ifelse(i>1,TRUE,FALSE),ifelse(i>1,FALSE,TRUE))


}


