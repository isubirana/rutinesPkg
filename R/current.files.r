#'current files

#'@export
current.files<-function(){

  files<-unlist(sapply(ls(env=.GlobalEnv),function(x) attr(get(x),"file.origen")))
  aux<-cbind(as.character(files),names(files))
  aux<-paste(aux[,1],aux[,2],sep=" | data.frame:")
  sel<-menu(aux,graphics=TRUE,"Quin arxiu vols obrir?")
  if (sel==0) stop("no s'ha escollit cap arxiu")
  file.sel=files[sel]
  shell.exec(file.sel)

}


