#' merge2
#' Fa el mateix que la funció 'merge' però no perd les etiquetes
#' @param x primera base de dades
#' @param y segona base de dades
#' @param by.id nom de la variable o vector amb el nom de les variables per ajuntar les dues base de dades (veure argument 'by' de 'merge')
#' @param ... further arguments from 'merge'
#' @export

merge2<-function(x,y,by.id,...){  # t? els mateixos arguments que la funcio de R merge.

  attr.x<-lapply(x,attributes)
  noms.y<-names(y)[!names(y)%in%by.id] # triem en una de les dos taules els atributs de les variables clau.
  if (!length(noms.y)) attr.y<-NULL
  if (length(noms.y)==1){
    attr.y<-attributes(y[,noms.y])
    attr.y<-eval(parse(text=paste("list(",noms.y,"=attr.y)",sep="")))
  }
  if (length(noms.y)>1) attr.y<-lapply(y,attributes)

  vars.repes<-names(y)[names(y)%in%names(x)[-which(names(x)%in%by.id)]] # si hi ha variables repetides, a part de les variables clau, les llista
  if (length(vars.repes)) cat("\n> Advertencia: les variables",paste(vars.repes,collapse=", "),"estan a les dos bases de dades\n\n")

  fusio<-merge(x,y,by=by.id,...)

  # recupera els atributs.
  if (!all(unlist(lapply(attr.x,function(x) is.null(x)))) | !all(unlist(lapply(attr.y,function(x) is.null(x))))){
    atributs <- c(attr.x,attr.y)
    for (i in 1:length(atributs)){
      index <- which(names(fusio)==names(atributs)[i])
      if (length(index)==1) attributes(fusio[,index])<-atributs[[i]]
    }
  }

  return(fusio)

}
