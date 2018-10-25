#'deltaAUC.surv
#'funció que calcula la diferència de AUC (C'index) entre dos models predictius tenint en compte la supervivència
#'@param dat dades
#'@param ref predits segons el model 'vell'
#'@param new predits segons el model 'nou'
#'@param times nom de la variable temps fins a event o censura
#'@param outcome nom de la variable indicadora de censura

#'@export
deltaAUC.surv<-function(dat,ref,new,times,outcome){
  temps<-dat[,times]
  cens<-dat[,outcome]
  res <- rcorrp.cens(ref,new, Surv(temps,as.integer(cens==1)), method=2)
  tst <- abs(res[1]/res[2])
  pvalue <- (1-pnorm(tst))*2
  ifelse(pvalue<0.001,"<0.001",round(pvalue,3))
}
