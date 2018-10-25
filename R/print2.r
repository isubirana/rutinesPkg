#' print2
#' FUNCIO QUE VISUALITZA (a trav?s de la funci? 'fix') UNA MATRIU O UN DATA.FRAME
#' té com a opcio que mostri les etiquetes de valors enlloc dels valors.
#' @param data dades
#' @param # view =1: noms etiquetes, view=2: valor i etiqueta, view!=1 ? 2: noms valor
#' @export

print2<-function(data,view=1){

  if (view%in%(1:2)){
    # substitueix els valors per les etiquetes (si en tenen)
    labels<-function(x){
      value.labels<-attr(x,"value.labels")
      if (!is.null(value.labels)){
        for (i in 1:length(value.labels)){
          if (view==1) x[x==value.labels[i]]<-names(value.labels)[i]
          if (view==2) x[x==value.labels[i]]<-paste(format(value.labels[i]),names(value.labels)[i],sep=": ")
        }
      }
      return(x)
    }
    data<-data.frame(lapply(data,labels))
  }

  if (is.data.frame(data)){
    data2<-as.data.frame(lapply(data,as.character))
    return(data2)
  } else{
    if (is.matrix(data)){
      data2=as.character(data)
      return(data2)
    }else stop("Les dades no son ni data.frame ni matrix")
  }

}
