#'calculadora.score
#'CALCUL RISC SCORE
#'@param sexo (0 dones, 1 homes)
#'@param edat: ha de ser >=45 i <=64
#'@param coltot (mg/dl)
#'@param tas (mmHg)
#'@param fuma2 (0 mai ? ex>1a; 1 - actual ? ex<1a)
#'@param diabetes (0 no, 1 s?)


#####################################################
############## C?LCUL PER SCORE #####################
#####################################################

#'@export
calculadora.score<-function(sexo,edat,coltot,tas,fuma2,diabetes,anys=5,low.risk=TRUE){



  colmol=coltot*0.02586

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

  w=0.24*(colmol-6)+0.018*(tas-120)+0.71*fuma2
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


  w=0.02*(colmol-6)+0.022*(tas-120)+0.63*fuma2
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

# comprovar!!!
# library(foreign)
# dades<-read.spss("U:\\Estudis\\Epidemiologia\\VERIFICA\\Dades\\verifica_oficial.SAV",FALSE,TRUE)
# names(dades)<-tolower(names(dades))
# dades<-subset(dades,exc_sco==0 & edat<65)
# dades$xxx<-with(dades,calculadora.score(sexo,edat,coltot,tas,fuma2,diabetes))
# plot(xxx~scoreall,data=dades)
# abline(0,1)
# max(with(dades,abs(scoreall-xxx)))

# summary(with(dades,calculadora.score(sexo,edat,coltot,tas,fuma2,diabetes,anys=5,low.risk=TRUE)))
# summary(with(dades,calculadora.score(sexo,edat,coltot,tas,fuma2,diabetes,anys=5,low.risk=FALSE)))

# summary(with(dades,calculadora.score(sexo,edat,coltot,tas,fuma2,diabetes,anys=10,low.risk=TRUE)))
# summary(with(dades,calculadora.score(sexo,edat,coltot,tas,fuma2,diabetes,anys=10,low.risk=FALSE)))
