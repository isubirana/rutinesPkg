#'convert.dates
#'crea una variable de tipus data (chron) a partir del dia, mes i any (integers)
#'@param day dies (vector integer)
#'@param month mes (vector integer)
#'@param year any (vector integer)
#'@import chron

#'@export
convert.dates<-function(day,month,year){
  ans  <-  apply(cbind(day,month,year),1,paste,collapse="-")
  ans  <-  chron(ans,format="d-m-Y",out.format="d-mon-Y")
  return(ans)
}
