#'calculadora.risc.recalibrated
#'Calculo del riesgo en enfermedad coronaria con las funciones de Framingham  y la adaptada de REGICOR
#' En aquesta funció es recalibra l'equació REGICOR tenint en compte la variància de les xbetas.
#'@param sex 1:hombres, 2:mujer ( o otro n?mero diferente de zero)
#'@param age edad en a?os
#'@param coltot colesterol total en mg/dL
#'@param hdl colesterol HDL en mg/dL
#'@param tas TAS en mmHg
#'@param tad TAD en mmHg
#'@param diab diabetes (1=s?, 0=no)
#'@param smoke fumador actual o <1a (1=s?, 0=no)
#'@param calibrated funci? calibrada (TRUE) o original (FALSE).



#*****************************************************************************************************************************************************************
#******                                               Sintaxis preparada por Jaume Marrugat el 02.02.2002, ********
#******************************************************************************************************************************************************************
#*************  Calculo del riesgo en enfermedad coronaria con las funciones de Framingham  y la adaptada de REGICOR              *************
#*************                             Referencias de inter?s:  Marrugat J Rev Esp Cardiol 2003; 56: 253-                                              ************
#*************                                                                 Marrugat J J Epidemiol Comm Health  2003; 57: 634-                              ************
#*************			                                 Ramos R Med Clin 2003;121: 521                                                          ************
#******************************************************************************************************************************************************************


#*********           Para usar esta sintaxis hay que adaptar los nombres de las variables a las usadas en cada base de datos    *****************

#******** Substituir o cambiar los nombres originales de las variables: sexo(sex), edad (age),  colesterol TOTAL (coltot), colesterol HDL (HDL),
#******** TA sist?lica (presis_x), TA diast?lica (presdi_x), Antecedente de diabetes (diabetes) y de consumo de tabaco (smoker).


#'@export
calculadora.risc.recalibrated<-function(sex,age,coltot,hdl,tas,tad,diab,smoke,calibrated=TRUE,age.range=c(35,74)){




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


  #**********************************************************************************************************************************************
  #********************                                 FUNCI?N de Framingham  (Circulation 1998)                      **************************
  #**********************************************************************************************************************************************

  #*** C?lculo con la funci?n de Framingham con las caracter?sticas originales de Framingham

  # Coeficientes

  # hombres
  coef.men<-c(
  0.04826,
  0,
  -0.65945,
  0,
  0.17692,
  0.50539,
  0.65713,
  0.49744,
  0.2431,
  0,
  -0.05107,
  -0.4866,
  -0.00226,
  0,
  0.2832,
  0.52168,
  0.61859,
  0.42839,
  0.52337)

  # mujeres
  coef.wom<-c(
  0.33766,
  -0.00268,
  -0.26138,
  0,
  0.20771,
  0.24385,
  0.53513,
  0.84312,
  0.37796,
  0.19785,
  0,
  -0.42951,
  -0.53363,
  0,
  -0.06773,
  0.26288,
  0.46573,
  0.59626,
  0.29246)

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

  # sum(betes*mean(x))
  g_chol.men=ifelse(!calibrated,3.0975,3.489)
  g_chol.wom=ifelse(!calibrated,9.9245,10.279)

  a_chol.men=l_chol.men-g_chol.men
  a_chol.wom=l_chol.wom-g_chol.wom

  b_chol.men=exp(a_chol.men)
  b_chol.wom=exp(a_chol.wom)

  # supervivencia basal (en la media de las covariables)
  S0.men=ifelse(!calibrated,0.90015, 0.9316813)
  S0.wom=ifelse(!calibrated,0.96246, 0.9643639)

  #* ATENCI?N: la So que se introduce en la f?rmula de la regresi?n de Cox se estima a partir de la de Hard end-points de Girona y un factor de correcci?n
  #*		proporcionado por los investigadores de Framingham a los autores de esta sintaxis
  #*                 La So estimada para todos los endpoints en los hombres de Girona es: 0,035 * 1.40 = 0,049  --> Ho = 1 - 0,049 = 0,951
  #*                 La So estimada para todos los endpoints en las mujeres de Girona  es: 0,011 * 1,91 = 0,022  --> Ho = 1 - 0,022 = 0,978;


  result<-ifelse(sex==1,1-S0.men^b_chol.men,1-S0.wom^b_chol.wom)

  result<-ifelse(age<age.range[1] | age>age.range[2],NA,result)

  return(result)

}

