#' fix2
#' FUNCI? QUE VISUALITZA (a traves de la funcio 'fix') UNA MATRIU O UN DATA.FRAME #############
#' @param data dades
#' @param view =1: noms etiquetes, view=2: valor i etiqueta, view!=1  2: noms valor
#' @param print TRUE: mostra el data.frame per la consola de R, print=FALSE: mostra el data.frame per una finestra (com la funcio 'fix2')
#' @param rows: selecciona els individus i ha de ser el nombre de la fila de l'individu en el data.frame.

#' @export
fix2<-function(data,view=1,print=FALSE,rows=NULL){

  if (!is.null(rows)){
    repe=TRUE
    k<-1
    while(repe==TRUE & k<1000){
      nom.var.temp<-paste(sample(letters,20),collapse="")
      repe<-nom.var.temp%in%names(data)
      k<-1
    }
    if (k==1000) stop("duplicate temp var name")
    temp<-eval(parse(text=paste("data.frame(",nom.var.temp,"=rows)",sep="")))
    eval(parse(text=paste("data$",nom.var.temp,"<-1:nrow(data)",sep="")))
    data<-merge2(temp,data,by.id=nom.var.temp,all.x=TRUE,sort=FALSE)
    data<-remove.vars(data,nom.var.temp,info=FALSE)
  }

  if (view%in%(1:2)){
    # substitueix els valors per les etiquetes (si en tenen)
    labels<-function(x){
      value.labels<-attr(x,"value.labels")
      if (!is.null(value.labels)){
        x<-as.character(x)
        value.labels<-structure(as.character(value.labels),names=names(value.labels))
        for (i in 1:length(value.labels)){
          if (view==1) x[x==value.labels[i]]<-names(value.labels)[i]
          if (view==2) x[x==value.labels[i]]<-paste(format(value.labels[i]),names(value.labels)[i],sep=": ")
        }
      }
      return(x)
    }
    data<-data.frame(lapply(data,labels))
  }

  aux<-try(fix(data),silent=TRUE)

  if (is.character(aux)){
    if (is.data.frame(data)){
      data2<-as.data.frame(lapply(data,as.character))
      if (print) print(data2,quote=FALSE) else fix(data2)
    } else{
      if (is.matrix(data)){
        data2=as.character(data)
        if (print) print(data2,quote=FALSE) else fix(data2)
      }else stop("Les dades no son ni data.frame ni matrix")
    }
  }

  return(invisible(data))


}


# fix2(rescate[,1:3],1,print=TRUE,rows=1:10)
