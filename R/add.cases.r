#### FUNCIO QUE AFEGEIX CASOS ####

# funcio que afegeix els casos de y sota dels de x.
# per a l'opcio all=1 i all=2 manen les etiquetes i formats de x
# per a l'opcio all=3 manen les etiquetes de x per a les variables comunes i les de y per a la resta. Els formats resultants s?n
#    coherents amb la fusio (realitza un arregla.formats)

add.cases<-function(x,y,font=NULL,all=3,show.warnings=TRUE){

  # all=1: posa les variables comunes de x i de y
  # all=2: posa totes les variables de x
  # all=3: posa totes les variables de x i totes les variables de y.

  comunes<-intersect(names(x),names(y))

  if (!is.null(font)){
    eval(parse(text=paste("x$",font,"<-0",sep="")))
    eval(parse(text=paste("y$",font,"<-1",sep="")))
  }

  if (all==1){ # totes les variables comunes de x i y.
    if (length(comunes)==0){
      if (show.warnings) cat("\n\n>Advertencia: no hi ha cap variable comuna\n\n")
      return(NULL)
    }
    if (length(comunes)>0){
      return(rbind(x[,comunes],y[,comunes]))
    }
  }
  if (all==2){  # totes les variables de x
    falten<-names(x)[!names(x)%in%comunes]
    if (length(falten)){
      if (show.warnings) cat("\n\n>Advertencia: les seguents variables quedaran buides\n  ",paste(falten,collapse=", "),"\n\n")
      for (i in 1:length(falten)) eval(parse(text=paste("y$'",falten[i],"'<-NA",sep="")))
    }
    return(rbind(x,y[,names(x)]))
  }
  if (all==3){ # totes les variables de y, x
    attr.x<-lapply(x,attributes)
    attr.y<-lapply(y,attributes)
    falten.y<-names(x)[!names(x)%in%names(y)]
    falten.x<-names(y)[!names(y)%in%names(x)]
    if (length(falten.y)){
      if (show.warnings) cat("\n\n>Advertencia: les seguents variables quedaran buides per a y\n  ",paste(falten.y,collapse=", "),"\n\n")
      for (i in 1:length(falten.y)) eval(parse(text=paste("y$'",falten.y[i],"'<-NA",sep="")))
      y<-as.data.frame(y)
    }
    if (length(falten.x)){
      if (show.warnings) cat("\n\n>Advertencia: les seguents variables quedaran buides per a x\n  ",paste(falten.x,collapse=", "),"\n\n")
      for (i in 1:length(falten.x)) eval(parse(text=paste("x$'",falten.x[i],"'<-NA",sep="")))
      x<-as.data.frame(x)
    }
    fusio<-rbind(
      as.data.frame(lapply(x[,names(x)],function(temp) I(as.character(temp)))),
      as.data.frame(lapply(y[,names(x)],function(temp) I(as.character(temp))))
    )
    fusio<-arregla.formats(fusio,force=TRUE)
    names(fusio)<-names(x)
    if (length(attr.x)){
      for (i in 1:length(attr.x)){
        attributes(fusio[,names(attr.x)[i]])<-attr.x[[i]]
      }
    }
    if (length(attr.y)){
      for (i in 1:length(attr.y)){ # queden els atributs de les variables de x.
        if (!names(attr.y)[i]%in%names(attr.x)) attributes(fusio[,names(attr.y)[i]])<-attr.y[[i]]
      }
    }
    return(fusio)
  }

}

