#'calculadora.risc.hard
#'Calculo del riesgo en enfermedad coronaria con las funciones de Framingham  y la adaptada de REGICOR
#'@param sex 1:hombres, 2:mujer ( o otro n?mero diferente de zero)
#'@param age edad en a?os
#'@param coltot colesterol total en mg/dL
#'@param hdl colesterol HDL en mg/dL
#'@param tas TAS en mmHg
#'@param tad TAD en mmHg
#'@param diab diabetes (1=s?, 0=no)
#'@param smoke fumador actual o <1a (1=s?, 0=no)
#'@param calibrated funci? calibrada (TRUE) o original (FALSE).
#'@param g_chol.men, g_chol.wom:  sum(betes*mean(x))
#'@param S0.men supervivencia basal en hombres (en la media de las covariables)
#'@param S0.wom: supervivencia basal en mujeres (en la media de las covariables)
#'@examples
#'calculadora.risc.hard(sex=1,
#'                      age=55,
#'                      coltot=154,
#'                      hdl=46,
#'                      tas=110,
#'                      tad=60,
#'                      diab=1,
#'                      smoke=1,
#'                      calibrated=TRUE,age.range=c(35,74))

#*****************************************************************************************************************************************************************
#******                                               Sintaxis preparada por Jaume Marrugat el 02.02.2002, ********
#******************************************************************************************************************************************************************
#*************  Calculo del riesgo en enfermedad coronaria con las funciones de Framingham  y la adaptada de REGICOR              *************
#*************                             Referencias de inter?s:  Marrugat J Rev Esp Cardiol 2003; 56: 253-                                              ************
#*************                                                                 Marrugat J J Epidemiol Comm Health  2003; 57: 634-                              ************
### CALCULA EL RISC A 10 ANYS D'EVENTS HARD (IAM MORTAL O NO MORTAL, EXCLOENT ANGOR!!) ###

#'@export
calculadora.risc.hard<-function(sex,age,coltot,hdl,tas,tad,diab,smoke,calibrated=TRUE,age.range=c(35,74),
       g_chol.men=4.2935939, g_chol.wom=6.5843, S0.men=1-0.035, S0.wom=1-0.011){

  if (!calibrated)
    stop("Only calibrated version implemented")


  ########### FUNCIONS PR?VIES ##########
  dummy<-function(var){   # ha d'estar codificat amb enters consecutius i aix? amb les etiquetes de valor.
    nlev=length(attr(var,"value.labels"))
    return(t(apply(as.matrix(var),1,function(x) return(if (is.na(x)) matrix(NA,1,nlev) else diag(nlev)[x,]))))
  }


  #***************************************************************************************************************************.
  #***************************************************************************************************************************.


  # colesterol: [-Inf,160) , [160,200) , [200,240) , [240,280) , [280,Inf)
  col_cat<-cut(coltot,c(-Inf,160,200,240,280,Inf),right=FALSE,include.lowest=TRUE)
  value.labels<-attributes(col_cat)$level
  col_cat=as.integer(col_cat)
  attr(col_cat,"value.labels")=structure(1:length(value.labels),names=value.labels)
  col_dum<-dummy(col_cat)

  # colesterol HDL
  hdl_cat<-cut(hdl,c(-Inf,35,45,50,60,Inf),right=FALSE,include.lowest=TRUE)
  value.labels<-attributes(hdl_cat)$level
  hdl_cat=as.integer(hdl_cat)
  attr(hdl_cat,"value.labels")=structure(1:length(value.labels),names=value.labels)
  hdl_dum<-dummy(hdl_cat)

  # Blood Pressure
  bp_cat=ifelse(tas<120 & tad<80,1,
                  ifelse(tas<130 & tad<85,2,
                          ifelse(tas<140 & tad<90,3,
                                  ifelse(tas<160 & tad<100,4,
                                          ifelse(tas>=160 | tad>=100,5,NA)))))
  attr(bp_cat,"value.labels")<-c("?ptima"=1,"bp_normal"=2,"alta"=3,"hipert. grado I"=4,"hipert. grado II"=5)
  bp_dum<-dummy(bp_cat)


  # Coeficientes

  # hombres
  coef.men<-c(
	0.053273,
	0,
	-0.378077,
	0,
	0.569563,
	0.743777,
	0.828414,
	0.60738,
	0.368415,
	0,
	0.000117,
	-0.46075,
	0.094779,
	0,
	0.422509,
	0.659446,
	0.896402,
	0.525166,
	0.727742)


  # mujeres
  coef.wom<-c(
	0.172121,
	-0.001035,
	-0.205142,
  0,
	0.439787,
	0.556523,
	0.892965,
	0.732869,
	0.596586,
	0.596926,
	0,
	-0.543377,
	-0.741411,
  0,
	-0.371037,
	0.215387,
	0.610854,
	0.866987,
	0.975058)

	  # variables
  variables<-c(
  "edad",
  "edad^2",
  "Colesterol total: <160",
  "Colesterol total: 160 - <200",
  "Colesterol total: 200 - <240",
  "Colesterol total: 240 - <280",
  "Colesterol total: >=280",
  "Colest. HDL: <35",
  "Colest. HDL: 35 - <45",
  "Colest. HDL: 45 - <50",
  "Colest. HDL: 50 - <60",
  "Colest. HDL: >=60",
  "Tensi?n arterial: ?ptima",
  "Tensi?n arterial: bp_normal",
  "Tensi?n arterial: alta",
  "Tensi?n arterial: hipert. grado I",
  "Tensi?n arterial: hipert. grado II",
  "Diab?tico",
  "Fumador")


  names(coef.men)<-names(coef.wom)<-variables

  # matriz de dise?o
  X<-cbind(age,age^2,col_dum,hdl_dum,bp_dum,diab,smoke)

  # quitamos los missings en alguna variable
  #keep<-!apply(is.na(X),1,any)
  #X<-X[keep,]

  l_chol.men<-X%*%coef.men
  l_chol.wom<-X%*%coef.wom

  a_chol.men=l_chol.men-g_chol.men
  a_chol.wom=l_chol.wom-g_chol.wom

  b_chol.men=exp(a_chol.men)
  b_chol.wom=exp(a_chol.wom)

  result<-ifelse(sex==1,1-S0.men^b_chol.men,1-S0.wom^b_chol.wom)

  result<-ifelse(age<age.range[1] | age>age.range[2],NA,result)

  attr(result,"l_chol.men")<-l_chol.men
  attr(result,"l_chol.wom")<-l_chol.wom

  return(result)

}



