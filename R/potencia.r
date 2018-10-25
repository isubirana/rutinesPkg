#' potencia
#' calcula la m?nima diferencia detectada per a una potencia donada i un nivell de confian?a donat
#' tamb? dibuixa un plot de la potencia a un nivell de confian?a donat
#' @param SE error est?ndar de la beta
#' @param pot pot?ncia (per defecte al 80%)
#' @param alpha nivell de significaci? (per defecte 5%)
#' @param df graus de llibertat (per estudis amb grand?ries de mostra molt petits es posa la grand?ria de la mostra, si l'estudi ?s gran, es posa Inf).
#' @param digits nombre de decimals que vols que aparegui en el gr?fic
#' @param fact vols multiplicar la variable predictora per algun valor? ?til per si es vol calcular la pot?ncia per al augment de 10 unitats de colesterol total per exemple, es posaria fact=10.
#' @param max.rang l?mits del gr?fic (+/-max.rang*SE)
#' @param OR vols donar els restulats en escala d'Odds Ratio ? Hazard Ratio (exp(beta)) o no (beta).
#' @param plot vols que es dibuixi el gr?fic o nom?s que surtin els resultats
#' @param max.diff.est la m?nima difer?ncia a detectar es buscar? entre 0 i SE*max.diff.est. Per defecte ?s 10, per? si el c?lcul d?na errors de converg?ncia es pot modificar.
#' @examples \donttest{
#' # Exemple 1: Regressi? lineal
#' #  pot?ncia 80%
#' #  alpha: 5%
#' #  SE de la beta: 0.5
#' potencia(0.5,pot=0.8,alpha=0.05,OR=FALSE)
#' # -> la difer?ncia m?nima a detectar amb una pot?ncia del 80% i un nivell de significaci? del 5% (i amb un error est?ndar de la beta), ?s de 1.4
#' # Exemple 2: Odds Ratio
#' # pot?ncia 80%
#' # alpha: 5%
#' # regressi? lineal -> OR=FALSE
#' # SE de la beta (log(OR)): 0.1
#' potencia(0.1,pot=0.8,alpha=0.05,OR=TRUE)
#' #-> l'OR m?s petit que es pot detectar amb una pot?ncia del 80% i un nivell de significaci? del 5% (i amb un error est?ndar de la beta), ?s de 1.32 (si ?s de risc) i de 0.76 (si ?s protector).
#' }



potencia <- function(SE,pot=0.8,alpha=0.05,df=Inf,digits=2,fact=1,max.rang=5,OR=TRUE,plot=TRUE,max.diff.est=10)
{

	SE<-SE*fact   # fact ?s l'escala en qu? es representa la variable predictora cont?nua.
	z<-ifelse(df==Inf,qnorm(1-alpha/2),qt(1-alpha/2,df))
	f <- function(x) return(pnorm(z-x)-pnorm(-z-x)-(1-pot))
	f.pot<-function(x) return(1-pnorm(z-x/SE)+pnorm(-z-x/SE))
	f.pot.OR<-function(x) return(f.pot(log(x)))
	dif.est<-uniroot(f,c(0,max.diff.est))$root
	dif<-dif.est*SE
	if (plot){
		par(mfrow=c(1,1),las=1)
		x.lim<-c(dif-max.rang*SE,dif+max.rang*SE)
		if (OR){
  		curve(f.pot.OR,xlim=exp(c(-x.lim[2],x.lim[2])),xlab=expression(e^beta),ylab="potencia",ylim=c(0,1))
			abline(h=pot,v=1,lty=3)
			lines(c(exp(-dif),exp(-dif)),c(0,pot),lty=2,col="red")
			lines(c(exp(dif),exp(dif)),c(0,pot),lty=2,col="red")
			title(paste("Si ?s protector:",round(exp(-dif),digits=digits)," | si ?s de risc:",round(exp(dif),digits=digits)))
		} else{
  		curve(f.pot,xlim=c(-x.lim[2],x.lim[2]),xlab=expression(beta),ylab="potencia",ylim=c(0,1))
			abline(h=pot,v=0,lty=3)
			lines(c(-dif,-dif),c(0,pot),lty=2,col="red")
			lines(c(dif,dif),c(0,pot),lty=2,col="red")
			title(paste("Si ?s protector:",round(-dif,digits=digits)," | si ?s de risc:",round(dif,digits=digits)))
		}
		legend("bottomright",bty="n",legend=paste("Pot?ncia=",pot,"\nConfian?a=",1-alpha))
	}
	if (fact>1) print(paste("ULL! La variable est? multiplicada per ",fact))
	if (fact<1) print(paste("ULL! La variable est? dividida per ",1/fact))
	return(structure(c(dif,dif.est),names=c("beta detectable","           beta detect.estand.")))

  options(OutDec=".") # tornem a posar punts pels decimals

}

# Nota: En aquest c?lcul de la pot?ncia es fa a posteriori, ?s a dir, es fa servir la mostra per a estimar el valor del SE. El valor del SE engloba, entre altres coses, la grand?ria de la mostra.
#      Com m?s gran ?s la mostra m?s petit ?s el SE i per tant m?s petita ?s la difer?ncia m?nima que es pot detectar per a una mateixa pot?ncia i nivell de significaci?.








