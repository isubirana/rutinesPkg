#'cut3
#'fa quantils amb el mateix mètode que ho fa el SPSS
#'@param x variable numèrica
#'@param g nombre de quantils (amb la mateixa n)


#'@export
cut3<-function(x,g){
  x<-na.omit(x)
  if (g>length(unique(x))) stop("Hi ha mes grups que dades diferents")
  perc<-rank(x,ties.method="average")/length(x)
  cum.perc<-cumsum(table(perc))
  breaks.perc<-sapply((1:g)/g,function(x) max(names(cum.perc)[which(names(cum.perc)<=x)]))
  cut(perc,c(0,breaks.perc))
}
