#' order2
#' ORDENA UNA BASE DE DADES (data) SEGONS UNES VARIABLES (vars) SENSE PERDRE ELS ATRIBUTS.
#' @param data dades
#' @param vars vector de les variables que serviran per ordenar les files
#' @param vars.view vector de les variables que es guardaran de la base de dades (default is 'ALL')
#' @param decreasing default is FALSE
#' @note no funciona si es vol ordenar per a més d'una variable
#' @export

order2<-function(data,vars,vars.view="ALL",decreasing=FALSE){

  ordre<-order(data[,vars],decreasing=decreasing)
  autonum<-1:nrow(data)

  if (length(vars.view)==1 && vars.view=="ALL") vars.view<-names(data)

  sorted.data<-merge2(data.frame("aquestaesunavariablemoltllarga"=ordre),
       cbind(data.frame(data.frame("aquestaesunavariablemoltllarga"=autonum)),data[,vars.view]),by.id="aquestaesunavariablemoltllarga",all.x=TRUE,sort=FALSE)
  sorted.data<-remove.vars(sorted.data,"aquestaesunavariablemoltllarga",info=FALSE)

  return(sorted.data)

}
