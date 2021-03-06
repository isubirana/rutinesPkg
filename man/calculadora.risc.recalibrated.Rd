% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/calculadora.risc.recalibrated.r
\name{calculadora.risc.recalibrated}
\alias{calculadora.risc.recalibrated}
\title{calculadora.risc.recalibrated
Calculo del riesgo en enfermedad coronaria con las funciones de Framingham  y la adaptada de REGICOR
En aquesta funció es recalibra l'equació REGICOR tenint en compte la variància de les xbetas.}
\usage{
calculadora.risc.recalibrated(sex, age, coltot, hdl, tas, tad, diab, smoke,
  calibrated = TRUE, age.range = c(35, 74))
}
\arguments{
\item{sex}{1:hombres, 2:mujer ( o otro n?mero diferente de zero)}

\item{age}{edad en a?os}

\item{coltot}{colesterol total en mg/dL}

\item{hdl}{colesterol HDL en mg/dL}

\item{tas}{TAS en mmHg}

\item{tad}{TAD en mmHg}

\item{diab}{diabetes (1=s?, 0=no)}

\item{smoke}{fumador actual o <1a (1=s?, 0=no)}

\item{calibrated}{funci? calibrada (TRUE) o original (FALSE).}
}
\description{
calculadora.risc.recalibrated
Calculo del riesgo en enfermedad coronaria con las funciones de Framingham  y la adaptada de REGICOR
En aquesta funció es recalibra l'equació REGICOR tenint en compte la variància de les xbetas.
}
