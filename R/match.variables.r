#' match.variables
#' PROGRAMA QUE BUSCA LES VARIABLES DE 'dades1' A 'dades2'.
#' LES VARIABLES QUE ES DIUEN IGUAL MIRA LA CORRESPOND?NCIA DE VALORS, SEMPRE QUE NO N'HI HAGIN MOLTS DE DIFERENTS (p.e.10).
#' LA RESTA DE VARIABLES LES BUSCA A 'dades2' A TRAV?S D'UNA CORRESPOND?NCIA NO EXACTE (veure funci? 'agrep').
#' DE MOMENT L'OUTPUT ?S UN LLISTAT A LA CONSOLA DE R.
#' @param dades1 primera base de dades
#' @param dades2 segona base de dades
#' @param max.values nombre de valors diferents per sobre del qual es considera contínua i per sota es considera categòrica
#' @param max.dist distancia per a considerar outlier
#' @param file.output nom de l'arxiu on es genera l'informe
#' @export


match.variables<-function(dades1,dades2,max.values=10,max.dist=0.1,file.output="xxx.doc"){

  comuns<-which(names(dades1)%in%names(dades2))
  if (length(comuns)==0) stop("No hi ha cap variable amb el mateix nom")
  var.comunes<-names(dades1)[comuns]

  ### VARIABLES LLIGADES ###

  vari.labels.dades1<-unlist(lapply(dades1,function(x){ res<-attr(x,"vari.label"); if (is.null(res)) return(" ") else return(res)}))
  vari.labels.dades2<-unlist(lapply(dades2,function(x){ res<-attr(x,"vari.label"); if (is.null(res)) return(" ") else return(res)}))

  vari.labels.dades1.comunes<-vari.labels.dades1[var.comunes]
  vari.labels.dades2.comunes<-vari.labels.dades2[var.comunes]


  # per visualitzar el nom de les variables i les vari.label
  vari.labels.comunes<-cbind(var.comunes,vari.labels.dades1.comunes,vari.labels.dades2.comunes)
  colnames(vari.labels.comunes)<-c("Nom variable",paste("Etiqueta en",substitute(dades1)),paste("Etiqueta en",substitute(dades2)))


  # dades1.comu i dades2.comu tenen el mateix nom de les variables i amb el mateix ordre.

  dades1.comu<-dades1[,var.comunes]
  dades2.comu<-dades2[,var.comunes]

  # funci? que retorna les etiquetes i els valors
  labels<-function(x){
    if (mean(is.na(x))==1) return(" ")
    value.labels<-attr(x,"value.labels")
    t<-table(x)
    if (length(t)<=max.values){
      t<-data.frame(list(valors=names(t),freq=as.integer(t)))
      if (!is.null(value.labels)){
        value.labels<-data.frame(list(labels=names(value.labels),valors=as.double(value.labels)))
        res<-merge(t,value.labels,all.x=TRUE,sort=FALSE)
      }
      if (is.null(value.labels)){
        res<-as.data.frame(cbind(t,as.character(t[,1])))
        names(res)<-c("valors","freq","labels")
      }
      res<-res[order(as.double(as.character(res[,1]))),]
      return(res[,c("valors","labels","freq")])
    }else return(" ")
  }

  cat("\n",file=file.output)
  cat("\n-------------------------------------------------------------\n",file=file.output,append=TRUE)
  cat("---------EXECUTANT EL PROGAMA MATCH.VARIABLES-----------------\n",file=file.output,append=TRUE)
  cat("-------------------------------------------------------------\n",file=file.output,append=TRUE)

  cat("\   -. DADES1:\n", as.character(substitute(dades1)),file=file.output,append=TRUE)
  cat("\   -. DADES2:", as.character(substitute(dades2)),"\n",file=file.output,append=TRUE)


  cat("\n\n------ MIRANT CORRESPONDENCIA DE LES ETIQUETES I VALORS -------\n",file=file.output,append=TRUE)
  cat("      DE LES VARIABLES AMB EL MATEIX NOM                         \n\n",file=file.output,append=TRUE)



  value.labels1<-lapply(dades1.comu,labels)
  value.labels2<-lapply(dades2.comu,labels)


  print.corr.labels<-function(nom.var){

    cat("---------",nom.var,": '",vari.labels.dades1[nom.var],"'|'",vari.labels.dades2[nom.var],"'----------\n\n",file=file.output,append=TRUE)

    aux1<-value.labels1[[nom.var]]
    aux2<-value.labels2[[nom.var]]

    if (is.data.frame(aux1) & is.data.frame(aux2)){ # les dues s?n categ?riques (max.values<10)
      res<-merge(aux1,aux2,by="valors",all=TRUE,sort=FALSE, suffixes=c(paste("(",substitute(dades1),")",sep=""),paste("(",substitute(dades2),")",sep="")))
      colnames<-names(res)
      res<-res[order(as.double(as.character(res[,1]))),]
      res<-as.matrix(apply(res,1,as.character))
      res<-ifelse(is.na(res),"*",res)
      res<-t(res)
      colnames(res)<-colnames
      write.table(res,row.names=FALSE,quote=FALSE,append=TRUE,file=file.output,sep="\t")
    } else {
      if (is.data.frame(aux1)) print(aux1) else cat("\nLa variable no est? etiquetada i t? m?s de",max.values,"valors diferents en",substitute(dades1),"\n")
      if (is.data.frame(aux2)) print(aux2) else cat("\nLa variable no est? etiquetada i t? m?s de",max.values,"valors diferents en",substitute(dades2),"\n")
    }

    cat("\n\n",file=file.output,append=TRUE)

  }

  invisible(sapply(var.comunes,print.corr.labels))


  ### VARIABLES DADES1 NO TROBADES A DADES2 #####

  cat("\n\n------ LOCALITZANT VARIABLES DE DADES1 NO TROBADES A DADES2 -------\n",file=file.output,append=TRUE)

  no.lligats.dades1<-names(dades1)[which(!names(dades1)%in%names(dades2))]

  if (length(no.lligats.dades1)>0){

    vari.labels.dades1.no.lligats<-vari.labels.dades1[no.lligats.dades1]
    vari.labels.dades1.no.lligats<-cbind(no.lligats.dades1,vari.labels.dades1.no.lligats)
    colnames(vari.labels.dades1.no.lligats)<-c("Nom variable","Etiqueta ")


    llista.names<-names(dades2)[!names(dades2)%in%var.comunes]
    llista.labels<-vari.labels.dades2[llista.names]
    cerca.no.lligats<-function(x){
      cat("\n---",x,":",vari.labels.dades1[x],"--------\n\n",file=file.output,append=TRUE)
      index.names<-index.labels<-index.names2<-index.labels2<-numeric(0)
      index.names<-agrep(x,as.character(llista.names),ignore.case=TRUE,max.distance=max.dist)
      index.labels<-agrep(x,as.character(llista.labels),ignore.case=TRUE,max.distance=max.dist)
      aux<-as.character(vari.labels.dades1[x])
      aux<-gsub(" ","",aux)
      if (aux!="") index.names2<-agrep(aux,as.character(llista.names),ignore.case=TRUE,max.distance=max.dist)
      aux<-as.character(vari.labels.dades1[x])
      aux<-gsub(" ","",aux)
      if (aux!="") index.labels2<-agrep(aux,as.character(llista.labels),ignore.case=TRUE,max.distance=max.dist)
      if (length(index.names)>0 || length(index.labels)>0 || length(index.names2)>0 || length(index.labels2)>0){
        index<-unique(c(index.names,index.labels,index.names2,index.labels2))
        write.table(data.frame(list(nom=llista.names[index],label=llista.labels[index])),row.names=FALSE,append=TRUE,file=file.output,sep="\t")
        cat("\n\n",file=file.output,append=TRUE)
      } else cat("No s'ha trobat cap correspond?ncia\n\n\n",file=file.output,append=TRUE)
    }
    invisible(sapply(no.lligats.dades1,cerca.no.lligats))

  } else cat("\n        TOTES LES VARIABLES DE DADES 1 S'HAN POGUT LLIGAR PEL NOM!!!     \n",file=file.output,append=TRUE)

  file.show(file.output)
  shell.exec(file.output)

  return(var.comunes)

}



