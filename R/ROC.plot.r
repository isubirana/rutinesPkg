#' ROC.plot
#' DIBUIXA UNA CURVA ROC I DONA L'AUC I EL SEU INTERVAL DE CONFIANÇA
#' @param y variable binaria 0/1
#' @param p valors predits
#' @param alpha nivell de significació (default 0.05)
#' @export

ROC.plot<-function(y,p,alpha=0.05){

	y=as.integer(y)
	p=as.double(p)

	#sensibilitat #{>=p i y==1} / # {y==1}
	#especificiatat #{<p i y==0} / # {y==0}

	aux<-rcorr.cens(p,y)
	AUC<-aux["C Index"]
	SE=aux["S.D."]
	AUC.inf<-AUC-qnorm(1-alpha/2)*SE/2
	AUC.sup<-AUC+qnorm(1-alpha/2)*SE/2

  out<-structure(c(AUC,AUC.inf,AUC.sup),names=c("AUC","inf","sup"))

  pred<-prediction(p,y)
  perf<<-performance(pred,"tpr","fpr")

  attr(out,"sens")<-perf@y.values[[1]]
  attr(out,"spec")<-1-perf@x.values[[1]]
  attr(out,"p")<-p
  attr(out,"y")<-y

  class(out)<-"ROC"

  out

}


print.ROC<-function(obj,d=3){
  cat("--- Area ander ROC curve [95%CI] ---- \n")
  AUC<-obj["AUC"]
  #print.default(obj)
  AUC.inf<-obj["inf"]
  AUC.sup<-obj["sup"]
  cat(paste("AUC = ",round(AUC,d)," [",round(AUC.inf,d)," ; ",round(AUC.sup,d),"]",sep=""),"\n\n")
}


plot.ROC<-function(obj,identify=FALSE,plot.text=FALSE,ylab="Sensitivity",xlab="1-Specificity",main="ROC curve",...){
  p<-attr(obj,"p")
  y<-attr(obj,"y")
  AUC<-obj["AUC"]
  AUC.inf<-obj["inf"]
  AUC.sup<-obj["sup"]
  res<-paste("AUC = ",round(AUC,3)," [",round(AUC.inf,3)," ; ",round(AUC.sup,3),"]",sep="")
	pred<-prediction(p,y)
	perf<-performance(pred,"tpr","fpr")
	plot(perf,type="l",ylim=c(0,1),xlim=c(0,1),ylab=ylab,xlab=xlab,main=main,xaxs="i",yaxs="i",...)
	abline(0,1,lty=2)
	if (plot.text) text(0.6,0.25,res)
	sens<-attributes(perf)$y.values[[1]]
	esp<-1-attributes(perf)$x.values[[1]]
  if (identify){
    while (winDialog("yesno","Vols identificar riscos en la curva ROC?")=="YES"){
  		coord<-locator(1)
  		sens.aprox<-coord$y
  		esp.aprox<-1-coord$x
  		dif<-(sens-sens.aprox)^2+(esp-esp.aprox)^2
  		index<-which(dif==min(dif))  #identifca el punt de la corba m?s pr?xima al punt del pla seleccionat.
  		#torna a dibuixar la corba.
  		plot(1-esp,sens,type="l",ylim=c(0,1),xlim=c(0,1),axes=FALSE,ylab="Sensitivity",xlab="1-Specificity",main="ROC curve",xaxs="i",yaxs="i",...)
  		abline(0,1,lty=2)
  		box()
  		axis(1,1-esp[index],labels=round(1-esp[index],3))
  		axis(2,sens[index],labels=round(sens[index],3))
 			if (plot.text) text(0.6,0.25,paste("AUC = ",round(AUC,3)," [",round(AUC.inf,3)," ; ",round(AUC.sup,3),"]",sep=""))
  		abline(h=sens[index],v=1-esp[index],col="red",lty=4)
  		winDialog("ok",paste("El risc associat ?s: ",round(100*p[index],2),"%",sep=""))
  	}
  	plot(1-esp,sens,type="l",ylim=c(0,1),xlim=c(0,1),ylab=ylab,xlab=xlab,main=main,,xaxs="i",yaxs="i")
  	abline(0,1,lty=2)
  	text(0.6,0.25,res)
  }
  #return(res)
}


lines.ROC<-function(obj,...){
  p<-attr(obj,"p")
  y<-attr(obj,"y")
  AUC<-obj["AUC"]
  AUC.inf<-obj["inf"]
  AUC.sup<-obj["sup"]
  res<-paste("AUC = ",round(AUC,3)," [",round(AUC.inf,3)," ; ",round(AUC.sup,3),"]",sep="")
  pred<-prediction(p,y)
  perf<-performance(pred,"tpr","fpr")
  lines(perf@x.values[[1]],perf@y.values[[1]],...)
  return(res)
}
