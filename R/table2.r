#' table2
#' funcion que modifica el \code{table} que substitueix els valors de les variables per l'etiqueta i el valor; per tant surten les etiquetes a la taula.
#' @param ... vector o vectors pel qual es farà la taula de freqüències
#' @param max.values nombre màxim de categories (default Inf - vol dir que no hi ha límit)
#' @param margin 1 proporció per files, 2 proporció per columnes, 3 per capes, etc. (NULL no calcula proporcions sinó freq absolutes)
#' @param digits nombre de decimals
#' @param print printa per la consola (default TRUE)
#' @note
#' els arguments de table2 han de ser vectors (o variables d'un data.frame) de la mateixa longitud.
#' @export


table2<-function(...,max.values=Inf,margin=NULL,digits=NULL,print=TRUE){

  args<-list(...)

  # funcion que substitueix els valors per les etiquetes (si en tenen)
  labels<-function(temp){
    value.labels<-attr(temp,"value.labels")
    if (!is.null(value.labels)){
      value.labels.num<-gsub(" ","0",format(value.labels))
      temp<-as.character(temp)
      value.labels<-structure(as.character(value.labels),names=names(value.labels))
      for(i in 1:length(value.labels)){
        # aixo posa p.e ' 1: trueta' .... '10: clinic'  i ho posa en ordre del valor
        temp[temp==value.labels[i]]<-paste(format(value.labels.num[i]),names(value.labels)[i],sep=": ")
      }
      labelled<-paste(format(value.labels.num),names(value.labels),sep=": ")
      no.labelled<-temp[!temp%in%labelled]
      if (length(table(no.labelled))==0)
        uv <- sort(names(table(labelled)))
      else
        uv <- c(sort(names(table(labelled))),sort(names(table(no.labelled))))
      temp<-factor(temp,levels=uv)
    }
    return(temp)
  }

  # expr: te el nom dels arguments (variables) de la funcio table2.
  expr<-match.call()
  expr<-gsub(" ","",expr)
  expr<-sub("x=","",expr)
  expr<-expr[2:(length(args)+1)]

  for (k in 1:length(expr)){
    vari.name<-expr[k]
    vari.label<-attr(args[[k]],"vari.label")
    if (print) cat("\nvariable name:",vari.name,"\n")
    if (print) cat("variable label:",vari.label,"\n")
    n.miss<-sum(is.na(args[[k]]))
    perc.miss<-format2(mean(is.na(args[[k]]))*100)
    if (print) cat("\tnum. of missings:",n.miss,"(",perc.miss,"%)","\n\n")
  }
  if (length(args)>1){
    n.miss<-length(args[[1]])-nrow(na.omit(data.frame(args)))
    perc.miss<-format2((n.miss/length(args[[1]]))*100)
    if (print) cat("Global: num. of missings:",n.miss,"(",perc.miss,"%)","\n\n")
  }

  var<-as.data.frame(lapply(args,labels))

  freq<-table(var,dnn=expr)

  #for (i in 1:length(expr)){
  #  if (class(args[[i]])[1]=="dates") dimnames(freq)[[i]]<-as.character(chron(as.double(dimnames(freq)[[i]]),out.format=c(dates="day-mon-year")))
  #}

  if (length(dim(freq))>1 & any(dim(freq)>max.values)) stop("Dimensio de la taula massa gran (superior a ",max.values,")")

  if (is.null(margin) || margin!=0){ # si margin==0 no posa percentatges!!!

    perc<-format2(prop.table(freq,margin=margin)*100,digits)

    if (length(dim(freq))==1) freq<-structure(paste(freq," (",perc,"%)",sep=""),names=names(freq))
    if (length(dim(freq))==2) for (i in 1:nrow(freq)) for (j in 1:ncol(freq)) freq[i,j]<-paste(freq[i,j]," (",perc[i,j],"%)",sep="")
    if (length(dim(freq))==3) for (i in 1:dim(freq)[1]) for (j in 1:dim(freq)[2]) for (k in 1:dim(freq)[3]) freq[i,j,k]<-paste(freq[i,j,k]," (",perc[i,j,k],"%)",sep="")
    if (length(dim(freq))==4) for (i in 1:dim(freq)[1]) for (j in 1:dim(freq)[2]) for (k in 1:dim(freq)[3]) for (l in 1:dim(freq)[4]) freq[i,j,k,l]<-paste(freq[i,j,k,l]," (",perc[i,j,k,l],"%)",sep="")

  }

  if (length(dim(freq))==1 | is.vector(freq)){
    freq<-cbind(freq)
    if (length(freq)>max.values){  # si hi ha mes de max.values valors fa un 'summary' enlloc d'un 'table'.
      freq<-summary(var)
      boxplot(var,main=expr)
      dimnames(freq)[[2]]<-""
    }
  }

  if (print) cat("\n")

  if (print) print(freq,quote=FALSE)

  return(invisible(freq))

}



#Exemple
#file<-"U:\\Estudis\\Clinic\\RESCATE I\\Dades\\Rescate I.sav"
#rescate<-read.spss4(file,keep.var=c("id","p129","p131","p132","p133","p138","p139","p140"))
#freq<-table2(rescate$p129)


#freq<-table2(rescate$p129,digits=3)
#freq<-table2(rescate$id,max.values=20)
#freq<-with(rescate,table2(p132,p129,margin=2,print=TRUE,max.values=20)) # percentatges per columnes (per columnes suma 100%)


# freq<-with(rescate,table2(p132,p129,p138,margin=c(2,3),print=TRUE,max.values=20))  # percentatges per columnes (per columnes suma 100%)



#RutinesLocals<-"U:/ULEC/Software/R cran project/rutines"
#source(file.path(RutinesLocals,"format2.r"))

#dades<-data.frame(gender=rep(0:1,each=1000))
#attr(dades$gender,"value.labels")<-c("home"=1,"dona"=0)

#table2(dades$gender)

#dades2<-subset2(dades,"gender==1")
#table2(dades2$gender)

#dades$hta<-sample(rep(0:1,each=1000))
#attr(dades$hta,"value.labels")<-c("si"=1,"no"=0)

#with(dades,table2(gender,hta))

#dades2<-subset2(dades,"gender==1")
#with(dades2,table2(gender,hta))


#dades$gender[78]<-7
#table2(dades$gender)

#dades2<-subset2(dades,"gender==1")
#table2(dades2$gender)

#xxx<-trunc(rnorm(100,10,2))
#attr(xxx,"value.labels")<-c("mitjana"=10)
#table2(xxx)

