#' prepare
#' prepare data: labels variables from vari.label attribut, and convert factors those variables with value.labels atributes.
#' @param dat dades
#' @param expand codifica valors sense etiquetes de valor (per exemple 1=diab i 2=no diabétic i també apareixen 9's, aleshores el 9 serà una nova categoria, si no el 9 passarà a ser NA).
#' @export


prepare<-function(dat,expand=FALSE){
  ans<-as.data.frame(lapply(1:ncol(dat), function(i) prepare.i(dat[,i],names(dat)[i],expand=expand)))
  names(ans)<-names(dat)
  ans
}


prepare.i<-function(x,var.name,expand){

  value.lab<-attr(x,"value.labels")
  if (!is.null(value.lab) && length(value.lab)==0)
    value.lab<-NULL

  if (!is.null(value.lab))
    names(value.lab)<-trim(names(value.lab))

  vari.lab<-attr(x,"vari.label")
  if (is.null(vari.lab))
    vari.lab<-attr(x,"label")

  if (!is.null(vari.lab))
    if (length(vari.lab)==0 || vari.lab=='')
      vari.lab<-NULL

  tt<-table(x)
  tt<-names(tt)

  if (expand & !is.null(value.lab)){
    tt<-tt[!tt%in%as.character(value.lab)]
    if (length(tt)>0)
      if (is.numeric(value.lab))
        tt<-structure(as.numeric(tt),names=tt)
      else
        tt<-structure(tt,names=tt)
      value.lab<-sort(c(value.lab,tt))
  }

  if (!is.null(value.lab))
    x<-factor(x,levels=sort(value.lab),labels=names(sort(value.lab)))

  if (is.null(vari.lab))
    Hmisc::label(x)<-var.name
  else
    Hmisc::label(x)<-vari.lab

  return(x)
}
