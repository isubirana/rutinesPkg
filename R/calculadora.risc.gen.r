
#####################################################
############## C?LCUL PER A REGICOR/FRAM ############
#####################################################

#'calculadora.risc.gen
#'Calculadora risc REGICOR/FRAMINGHAM que té en compte la genètica
#'
#'@param sex 1:hombres, 2:mujer ( o otro n?mero diferente de zero)
#'@param age edad en a?os
#'@param coltot colesterol total en mg/dL
#'@param hdl colesterol HDL en mg/dL
#'@param tas TAS en mmHg
#'@param tad TAD en mmHg
#'@param diab diabetes (1=s?, 0=no)
#'@param smoke fumador actual o <1a (1=s?, 0=no)
#'@param snps matrix on cada fila ?s el nombre d'alels de risc (0, 1 ? 2) per a cada snp, per a cada individu.
#'@param comp.gen -0.2462213 # suma del productes de les betes per les prevalences dels alels de risc.
#'@param calibrated calibrada (TRUE) o original (FALSE).
#'@param betas.gen.men vector betas SNPs en homes
#'@param betas.gen.men vector betas SNPs en dones

#'@export
calculadora.risc.gen<-function(sex,age,coltot,hdl,tas,tad,diab,smoke,snps,comp.gen,calibrated=TRUE,age.range=c(35,74),betas.gen.men,betas.gen.wom){





if (missing(betas.gen.men))
  betas.gen.men<-c(  0.11330,  0.30010,  0.20701,  0.19062,  0.12222,  0.12220,  0.17395,  0.15700,  0.13980,  0.18230)

if (missing(betas.gen.wom))
  betas.gen.wom<-c(  0.11330,  0.30010,  0.20701,  0.19062,  0.12222,  0.12220,  0.17395,  0.15700,  0.13980,  0.18230)




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
  0.52337,

  betas.gen.men

  )

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
  0.29246,

  betas.gen.wom

  )

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
  "Fumador",

  "rs12526453",
  "rs1333049",
  "rs1474787",
  "rs1515098",
  "rs17465637",
  "rs3184504",
  "rs501120",
  "rs6725887",
  "rs9818870",
  "rs9982601"

  )

  names(coef.men)<-names(coef.wom)<-variables

  # matriz de dise?o
  X<-cbind(age,age^2,col_dum,hdl_dum,bp_dum,diab,smoke,snps)

  # quitamos los missings en alguna variable
  #keep<-!apply(is.na(X),1,any)
  #X<-X[keep,]

  l_chol.men<-X%*%coef.men
  l_chol.wom<-X%*%coef.wom



  # sum(betes*mean(x))
  g_chol.men=ifelse(!calibrated,3.0975+comp.gen,3.489+comp.gen)
  g_chol.wom=ifelse(!calibrated,9.9245+comp.gen,10.279+comp.gen)

  a_chol.men=l_chol.men-g_chol.men
  a_chol.wom=l_chol.wom-g_chol.wom

  b_chol.men=exp(a_chol.men)
  b_chol.wom=exp(a_chol.wom)

  # supervivencia basal (en la media de las covariables)
  S0.men=ifelse(!calibrated,0.90015,0.951)
  S0.wom=ifelse(!calibrated,0.96246,0.978)

  #* ATENCI?N: la So que se introduce en la f?rmula de la regresi?n de Cox se estima a partir de la de Hard end-points de Girona y un factor de correcci?n
  #*		proporcionado por los investigadores de Framingham a los autores de esta sintaxis
  #*                 La So estimada para todos los endpoints en los hombres de Girona es: 0,035 * 1.40 = 0,049  --> Ho = 1 - 0,049 = 0,951
  #*                 La So estimada para todos los endpoints en las mujeres de Girona  es: 0,011 * 1,91 = 0,022  --> Ho = 1 - 0,022 = 0,978;


  result<-ifelse(sex==1,1-S0.men^b_chol.men,1-S0.wom^b_chol.wom)

  result<-ifelse(age<age.range[1] | age>age.range[2],NA,result)

  if (sex==1) attr(result,"coef")<-coef.men  else attr(result,"coef")<-coef.wom
  attr(result,"X")<-X
  colnames(X)<-names(attr(result,"coef"))
  X<-X[,c(3:17,19),drop=FALSE]
  beta<-attr(result,"coef")[c(3:17,19)]
  beta<-beta[X[1,]==1]
  attr(result,"beta")<-beta

  return(result)

}


#####################################################
############## C?LCUL PER SCORE #####################
#####################################################


#'calculadora.score
#'Calculadora risc SCORE que té en compte la genètica
#'@param sexo (0 dones, 1 homes)
#'@param edat edat
#'@param coltot (mg/dl)
#'@param tas (mmHg)
#'@param fuma2 (0 mai ? ex>1a; 1 - actual ? ex<1a)
#'@param diabetes (0 no, 1 s?)

#'@export

calculadora.score<-function(sexo,edat,coltot,tas,fuma2,diabetes,snps,comp.gen,anys=5,low.risk=TRUE){

  colmol=coltot*0.02586

  beta.snps<-c(
  0.11330,
  0.30010,
  0.20701,
  0.19062,
  0.12222,
  0.12220,
  0.17395,
  0.15700,
  0.13980,
  0.18230)

  names(beta.snps)<-c(
  "rs12526453",
  "rs1333049",
  "rs1474787",
  "rs1515098",
  "rs17465637",
  "rs3184504",
  "rs501120",
  "rs6725887",
  "rs9818870",
  "rs9982601")

  x.beta.snps<-snps%*%beta.snps
  x.beta.snps<-x.beta.snps-comp.gen

  ## per als FATAL CORONARY HEART DESEASE.

  if (low.risk){
    alphamen = -22.1
    alphawom = -29.8
    pmen = 4.71
    pwom = 6.36
  } else {
    alphamen = -21.0
    alphawom = -28.7
    pmen = 4.62
    pwom = 6.23
  }

  so_edat= ifelse (sexo == 0, exp(-exp(alphawom)*(edat-20)^pwom), exp(-exp(alphamen)*(edat-20)^pmen))
  so_ed5 = ifelse (sexo == 0, exp(-exp(alphawom)*(edat-20+anys)^pwom), exp(-exp(alphamen)*(edat-20+anys)^pmen))

  w=0.24*(colmol-6)+0.018*(tas-120)+0.71*fuma2 + x.beta.snps
  score0=(so_edat)**exp(w)
  score5=(so_ed5)**exp(w)
  scorechd=(1-(score5/score0))

  scorechd = ifelse(diabetes==1 & sexo==0, scorechd*4, scorechd)
  scorechd = ifelse(diabetes==1 & sexo==1,scorechd*2, scorechd)

  #attr(scorechd,"vari.label")<-'Risc de fatal coronary heart desease'


  ##  per als FATAL NO-CORONARY HEART DESEASE.

  if (low.risk){
    alphamen = -26.7
    alphawom = -31
    pmen = 5.64
    pwom = 6.62
  } else {
    alphamen = -25.7
    alphawom = -30.0
    pmen = 5.47
    pwom = 6.42
  }

  so_edat = ifelse(sexo == 0, exp(-exp(alphawom)*(edat-20)^pwom),exp(-exp(alphamen)*(edat-20)^pmen))
  so_ed5 = ifelse(sexo == 0, exp(-exp(alphawom)*(edat-20+anys)^pwom),exp(-exp(alphamen)*(edat-20+anys)^pmen))


  w=0.02*(colmol-6)+0.022*(tas-120)+0.63*fuma2 + x.beta.snps
  score0=(so_edat)^exp(w)
  score5=(so_ed5)^exp(w)
  scorecv=(1-(score5/score0))

  scorecv = ifelse(diabetes==1 & sexo==0, scorecv*4, scorecv)
  scorecv = ifelse(diabetes==1 & sexo==1, scorecv*2, scorecv)

  #attr(scorecv,"vari.label")<-'Risc de fatal no-coronary heart desease'


  #  per a tots els FATAL HEART DISEASE.

  scoreall=scorechd+scorecv
  #attr(scoreall,"vari.label")<- 'Risc de fatal cardiovascular heart desease'
  scoreall

}
