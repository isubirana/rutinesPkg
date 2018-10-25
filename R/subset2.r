#' subset2
#' ANÀLEG AL SUBSET PER NO PERD ELS ATRIBUTS
#' @param data dades
#' @param keep logical vector per seleccionar les files. O una expressió lógica en format caràcter
#' @param vars vector amb els noms de les variables a seleccionar
#' @note
#' A DIFER?NCIA DEL SUBSET, ES NECESSARI PASSAR-LI UN DATA.FRAME O UNA MATRIU.
#' TAMBE SE LI HI DE POSAR EL KEEP COM UNA EXPRESSI EN FORMAT CADENA, I AIXI NO CAL POSAR EL DATAFRAME.
#' @example \donttest{
#' file<-"diabetes.sav"
#' dades<-read.spss4(file,tolower=TRUE)
#' dades2<-subset2(dades[,"sexo",drop=FALSE],sexo==2)   # hi posem el drop=FALSE perque no ho converteixi en un vector.
#' dades2<-subset2(dades,dades$sexo==2)
#' dades2<-subset2(matrix(1,2,1),c(FALSE,TRUE))  # veiem com tambe es capaç de treballar amb matrius.
#' }
#' @export

subset2<-function(data,keep,vars,...){

  attr<-lapply(data,attributes)

  #data <- if (is.logical(keep)) subset(data,keep,...) else eval(parse(text=paste("subset(data,",keep,",...)",sep="")))
  data <- if (is.logical(keep)) subset(data,keep) else eval(parse(text=paste("subset(data,",keep,",)",sep="")))

  if (!missing(vars)) data<-data[,vars]


  if (length(attr)>0){
    invisible(sapply(1:ncol(data),function(i) attributes(data[,i])<<-attr[[which(names(attr)==names(data)[i])]]))
  }
  return(data)

}




