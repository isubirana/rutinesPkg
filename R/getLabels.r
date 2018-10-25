#' getLabels
#' recull les etiquetes de variable i de valors d'una base de dades
#' @param data base de dades

#' @export
getLabels<-function(data){
  vari.labels<-unlist(lapply(data,function(x){
        vl<-attr(x,"vari.label")
        if (is.null(vl))
          return('')
        else
          return(vl)
        }))
  value.labels<-unlist(lapply(data,function(x){
        vl<-attr(x,"value.labels")
        if (is.null(vl))
          return(NA)
        else{
          vl<-paste(paste("'",names(vl),"'=",vl,sep=""),collapse="; ")
          return(vl)
        }}))
  ans<-data.frame("name"=names(dades),"vari.label"=vari.labels,"value.label"=value.labels)
  ans
}
