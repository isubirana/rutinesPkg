#' intervals
#' Calcula els intervals de confiança d'un model (lineal, logístic o cox)
#' @param objecte (model lineal, logístic o cox)
#' @param level nivell de confiança (default 0.95)
#' @param ... further argumetns.
#' @export

intervals <- function(object, level = 0.95, ...)
  UseMethod("intervals")

#' intervals.lm
#' Calcula els intervals de confiança d'un model lineal
#' @param objecte model lineal
#' @param level nivell de confiança (default 0.95)
#' @param fun funció per a tranformar els coeficients (per defecte la identitat)
#' @param lab etiqueta per al coeficient (defect 'coef')
#' @export

intervals.lm <- function(x, level=.95, fun=function(beta) beta, lab="coef")     # regressi? lineal a partir de la funci? lm
{
  z<-abs(qnorm((1-level)/2))
  co <- summary(x)$coef
  or <- fun(co[, 1])
  li <- fun(co[, 1]-z * co[, 2])
  ls <- fun(co[, 1]+z * co[, 2])
  p <- 2*(1-pt(abs(co[, 1]/co[, 2]), x$df.residual))
  r <- cbind(or, li, ls, p)
  dimnames(r) <- list(dimnames(co)[[1]], c(lab, paste(level*100,"%",sep=""), "C.I.", "p-value"))
  class(r)<-c("intervals","intervals.lm")
  r
}


#' intervals.glm
#' Calcula els intervals de confiança d'un model logístic (glm)
#' @param objecte model logístic
#' @param level nivell de confiança (default 0.95)
#' @param fun funció per a tranformar els coeficients (per defecte l'exponencial -OR-)
#' @param lab etiqueta per al coeficient (defect 'or')
#' @export

intervals.glm <- function(x, level=.95, fun=exp , lab="or")
{
  z<-abs(qnorm((1-level)/2))

  link<-family(x)$link

  if (link%in%c("logit","log")){
    lab=if (link=="logit") "or" else "rr"
  }
  if (link=="identity"){
    fun=function(beta) beta
    lab="coef"
  }

  co <- summary(x)$coef
  or <- fun(co[, 1])
  li <- fun(co[, 1]-z * co[, 2])
  ls <- fun(co[, 1]+z * co[, 2])
  p <- 2*(1-pnorm(abs(co[, 1]/co[, 2])))
  r <- cbind(or, li, ls, p)
  dimnames(r) <- list(dimnames(co)[[1]], c(lab, paste(level*100,"%",sep=""), "C.I.", "p-value"))
  class(r)<-c("intervals",if (lab=="beta") "intervals.lm" else "intervals.glm")
  r
}

#' intervals.coxph
#' Calcula els intervals de confiança d'un model de cox
#' @param objecte model de cox
#' @param level nivell de confiança (default 0.95)
#' @param fun funció per a tranformar els coeficients (per defecte l'exponencial -HR-)
#' @param lab etiqueta per al coeficient (defect 'hr')
#' @export
intervals.coxph <- function(x, level=.95, fun=exp, lab="hr")
{
  z<-abs(qnorm((1-level)/2))
  co <- coef(x)
  se <- sqrt(diag(vcov(x)))
  or <- fun(co)
  li <- fun(co-z * se)
  ls <- fun(co+z * se)
  p <- 2*(1-pnorm(abs(co/se)))
  r <- cbind(or, li, ls, p)
  dimnames(r) <- list(names(co), c(lab, paste(level*100,"%",sep=""), "C.I.", "p-value"))
  class(r)<-c("intervals","intervals.coxph")
  r
}


#' print.intervals
#' métode per a printar per consola
#' @param n objecte intervals
#' @param len length (default 6)
#' @param d nombre de digits (default 3)
#' @param exclude.intercept logical. incloure intercept (no si el model és cox)
#' @export
print.intervals <- function(n, len = 6, d = 3, exclude.intercept=inherits(n,"intervals.glm"))
{
  dd <- dim(n)
  n[n > 999.99] <- Inf
  a <- n
  a[, 1:3] <- formatC(n[, 1:3], d, len,format="f")
  a[, 4] <- formatC(n[, 4], 3, len,format="f")
  dim(a) <- dd
  if(length(dd) == 1){
      dd<-c(1,dd)
      dim(a)<-dd
      lab<-" "
      }
  else
      lab <- dimnames(n)[[1]]
  if(T){
      mx <- max(nchar(lab)) + 1
      cat(paste(rep(" ",mx),collapse=""),paste("   ",dimnames(n)[[2]]),"\n")
      for(i in (1+exclude.intercept):dd[1]) {
          lab[i] <- paste(c(rep(" ", mx - nchar(lab[i])), lab[i]),collapse = "")
          cat(lab[i], a[i, 1], "(", a[i, 2], "-", a[i, 3], ") ",a[i, 4],"\n")
      }
  }
  else cat(a[1], "(", a[2], "-", a[3], ") ",a[4],"\n")
}
