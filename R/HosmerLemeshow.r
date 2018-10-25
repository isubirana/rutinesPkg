#'HosmeLemeshow
#'Calcula el test de HosmerLemeshow per a logística i per a supervivència

#' @export
HosmerLemeshow<-function(obj,...)
  UseMethod("HosmerLemeshow")

#' HosmlerLemeshow.Surv
#' Realitza el test de Hosmer-Lemeshow a partir d'un vector de supervivència
#' @param obj vector de supervivència creat amb la funció 'Surv'
#' @param risk vector numèric amb el risc predit
#' @param nq nombre de grups
#' @param years anys a calcular el risc observat
#' @param breaks punts de tall si 'nq' no s'especifica
#' @param digits nombre de decimals
#' @param adj aplica l'ajust segons mètode Nam (default FALSE)
#' @param ... further arguments

#' @export
HosmerLemeshow.Surv<-function(obj, risk, nq, years, breaks,digits=2, adj=FALSE,...){

  if (!inherits(obj,"Surv"))
    stop("obj must be of class 'Surv'")

  y<-obj
  p<-risk

  keep<-!is.na(y) & !is.na(risk)
  y<-y[keep,]
  p<-p[keep]

  time.max<-years*365.25

  if (missing(nq) && missing(breaks))
    stop("'nq' or 'years' must be specified")

  if (!missing(nq) && !missing(breaks))
    stop("only one of the two argument,'nq' or 'years', must be specified")

  if (!missing(nq))
    pcut<-cut2(p*100,g=nq,digits=digits)
 #pcut<-cut3(p,g=nq)

  if (!missing(breaks))
    pcut<-cut(p,c(-Inf,breaks,Inf))

  nq<-length(levels(pcut))

  n<-table(pcut)

  ss<-summary(survfit(y~pcut))
  p.obs<-sapply(levels(ss$strata),function(xx) {
    tt.i <- ss$time[ss$strata==xx]
    ss.i <- ss$surv[ss$strata==xx]
    ss.i <- ss.i[tt.i<=time.max]
    ans<-1-ss.i[length(ss.i)]
    if (length(ans)==0) ans<-0
    ans
  })

  names(p.obs)<-sub("^pcut=","",names(p.obs))

  if (length(n)>length(p.obs))
    warning("some categories have no event")

  temp<-rep(0,nq)
  names(temp)<-levels(pcut)
  temp[names(p.obs)]<-p.obs
  p.obs<-temp

  p.exp<-tapply(p,pcut,mean)
  n<-n[names(p.obs)]
  p.exp<-p.exp[names(p.obs)]

  obs <- cbind(n*p.obs,n*(1-p.obs))
  exp <- cbind(n*p.exp,n*(1-p.exp))

  if (adj)
    chisq <- sum(n*(p.obs-p.exp)^2/((p.exp+1/n)*(1-p.exp+1/n)))
  else
    chisq <- sum((n*(p.obs-p.exp)^2)/(p.exp*(1-p.exp)))
  df <- length(p.obs)-1
  pvalue <- 1-pchisq(chisq,df)

  res<-list(obs=obs,exp=exp,chisq=chisq,df=df,pvalue=pvalue)

  class(res) <- "HosmerLemeshow"

  res

}


#' HosmlerLemeshow.coxph
#' Realitza el test de Hosmer-Lemeshow a partir d'un model de Cox
#' @param obj model de cox
#' @param nq nombre de grups
#' @param years anys a calcular el risc observat
#' @param ... further arguments

#' @export
HosmerLemeshow.coxph<-function(obj, nq, years,...){

  if (!inherits(obj,"coxph"))
    stop("obj must be of class 'coxph'")

  fit<-obj
  time.max<-years*365.25

  baseh<-basehaz(fit,centered=TRUE)
  baseh<-subset(baseh,time<=time.max)
  baseh.time.max<-baseh[nrow(baseh),"hazard"]
  lp<-predict(fit,type="lp")
  lp <- lp[!is.na(lp)]
  haz.time.max<-baseh.time.max*exp(lp)
  prob.time.max<-1-exp(-haz.time.max)

  p<-prob.time.max
  pcut<-cut3(p,g=nq)

  n<-table(pcut)

  ss<-summary(survfit(fit$y~pcut))
  p.obs<-sapply(levels(ss$strata),function(xx) {
    tt.i <- ss$time[ss$strata==xx]
    ss.i <- ss$surv[ss$strata==xx]
    ss.i <- ss.i[tt.i<=time.max]
    ans<-1-ss.i[length(ss.i)]
    if (length(ans)==0) ans<-0
    ans
  })


  if (length(n)>length(p.obs))
    warning("some categories have no event and have been removed")

  p.exp<-tapply(p,pcut,mean)
  n<-n[sub("^pcut=","",names(p.obs))]
  p.exp<-p.exp[sub("^pcut=","",names(p.obs))]

  obs <- cbind(n*p.obs,n*(1-p.obs))
  exp <- cbind(n*p.exp,n*(1-p.exp))


  chisq <- sum((n*(p.obs-p.exp)^2)/(p.exp*(1-p.exp)))
  df <- length(p.obs)-2
  pvalue <- 1-pchisq(chisq,df)

  res<-list(obs=obs,exp=exp,chisq=chisq,df=df,pvalue=pvalue)

  class(res) <- "HosmerLemeshow"

  res

}


#' HosmlerLemeshow.glm
#' Realitza el test de Hosmer-Lemeshow a partir d'un model logístic
#' @param obj model logístic
#' @param ... altres arguments de HosmerLemeshow.Surv (com nq, years,...)

#' @export
HosmerLemeshow.glm<-function(obj,...){

  if (!inherits(obj,"glm"))
    stop("obj must be of class 'glm'")

  if (family(obj)$family!="binomial" | family(obj)$link!="logit")
    stop("fit must be logistic regression")

  p=obj$fitted.values

  y=obj$y

  ans <- HosmerLemeshow(p,y,...)

  ans

}


#' HosmlerLemeshow.glm
#' Realitza el test de Hosmer-Lemeshow "clàssic"
#' @param obj risk estimat o variable predictora
#' @param y variable binària (0/1)
#' @param nq nombre de grups (per defecte 10)

#' @export
HosmerLemeshow.default<-function(obj,y,nq=10){

  p<-obj

  if (any(p<0 | p>1)) stop("p must be between 0 and 1")
  if (any(y%nin%c(0,1))) stop("y must be 0 or 1")

  if (any(is.na(y))) stop("y has missing values")
  if (any(is.na(p))) stop("p has missing values")

  pcut<-cut3(p,g=nq)

  obs.yes<-tapply(y,pcut,sum)
  obs.no<-tapply(1-y,pcut,sum)
  exp.yes<-tapply(p,pcut,sum)
  exp.no<-tapply(1-p,pcut,sum)

  obs<-cbind(obs.yes,obs.no)
  exp<-cbind(exp.yes,exp.no)

  chisq<-sum((obs-exp)^2/exp)
  df<-nq-2
  pvalue<-pchisq(chisq,df,lower.tail=FALSE)


  res<-list(obs=obs,exp=exp,chisq=chisq,df=df,pvalue=pvalue)

  class(res)<-"HosmerLemeshow"

  return(res)

}


#' print.HosmerLemeshow
#' mètode genèric per printar el resultat de Hosmer Lemeshow
#' @param hl objecte de classe HosmerLemeshow

#' @export
print.HosmerLemeshow<-function(hl){
  cat("_____Hosmer Lemeshow Test_____\n\n")
  taula<-cbind(format2(hl$obs[,1],3),format2(hl$exp[,1],3),format2(hl$obs[,2],3),format2(hl$exp[,2],3),format2(rowSums(hl$obs),3))
  taula<-cbind(1:nrow(taula),taula)
  taula<-rbind(c("decile","yes observed","yes expected","no observed","no expected","total"),taula)
  colnames(taula)<-rep("",ncol(taula))
  rownames(taula)<-rep("",nrow(taula))
  taula<-apply(taula,2,format,justify="centre")
  print(taula,quote=FALSE)

  cat("\n\n")
  cat("Chi-square =",hl$chisq," ( df =",hl$df,"), pvalue =",format2(hl$pvalue,3),"\n")

}

#' plot.HosmerLemeshow
#' mètode genèric per graficar el resultat de Hosmer Lemeshow
#' @param hl objecte de classe HosmerLemeshow
#' @param perc printa el percentatge o el valor absolut d'esdeveniments
#' @param leg.text text
#' @param fact factor per multiplicar els esdeveniments
#' @param ...  further arguments

#' @export
plot.HosmerLemeshow<-function(hl, perc = FALSE, leg.text=c("Expected","observed"), fact=1, ...){

  if (perc){
    obs<-hl$obs[,1]/rowSums(hl$obs)
    exp<-hl$exp[,1]/rowSums(hl$obs)
  } else {
    obs<-hl$obs[,1]
    exp<-hl$exp[,1]
  }

  barplot(rbind(exp,obs)*fact,beside=TRUE,legend.text=leg.text, args.legend=list(x="topleft",bty="n"),...)

}



