% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/HosmerLemeshow.r
\name{HosmerLemeshow.Surv}
\alias{HosmerLemeshow.Surv}
\title{HosmlerLemeshow.Surv
Realitza el test de Hosmer-Lemeshow a partir d'un vector de supervivència}
\usage{
\method{HosmerLemeshow}{Surv}(obj, risk, nq, years, breaks, digits = 2,
  adj = FALSE, ...)
}
\arguments{
\item{obj}{vector de supervivència creat amb la funció 'Surv'}

\item{risk}{vector numèric amb el risc predit}

\item{nq}{nombre de grups}

\item{years}{anys a calcular el risc observat}

\item{breaks}{punts de tall si 'nq' no s'especifica}

\item{digits}{nombre de decimals}

\item{adj}{aplica l'ajust segons mètode Nam (default FALSE)}

\item{...}{further arguments}
}
\description{
HosmlerLemeshow.Surv
Realitza el test de Hosmer-Lemeshow a partir d'un vector de supervivència
}
