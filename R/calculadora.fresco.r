#'calculadora.fresco
#'
#'



###### curt #########


# CHD
#'@export
calculadora.fresco.curt<-function(sexo,edad,fuma,imc,rang.edat=c(35,79), anys=10){
  if (anys!=10 & anys!=5) stop("Els anys nom?s poden ser 5 o 10")
  ## homes
    # prevalences
  meansmen <- c(
  55.83768882,
  0.295619335,
  0.239954683
  )
    # betes
  coefsmen <- c(
  0.052898876,
  0.465519602,
  0.330568597
  )
    # supervivencia basal
  survmen <- 0.961666274
  if (anys==5) survmen <- exp(log(survmen)/2)

  ## dones
    # prevalences
  meanswom <- c(
  55.77610868,
  0.133629417,
  0.300372281
  )
    # betes
  coefswom <- c(
  0.080068894,
  0.775859409,
  0.217467072
  )
    # supervivencia basal
  survwom <- 0.987324042
  if (anys==5) survwom <- exp(log(survwom)/2)

  ## matriu de disseny
  X <- cbind(edad,fuma,as.integer(imc>30))
  ## calcul risc
  lpmen <- X%*%coefsmen - sum(meansmen*coefsmen)
  lpwom <- X%*%coefswom - sum(meanswom*coefswom)
  risc <- ifelse(sexo==1,1-survmen^exp(lpmen),1-survwom^exp(lpwom))
  risc <- ifelse(edad>rang.edat[2] | edad<rang.edat[1], NA, risc)
  return(risc)
}


### CVD
#'@export
calculadora.fresco.curt.cvd<-function(sexo,edad,fuma,imc,rang.edat=c(35,79), anys=10){
  if (anys!=10 & anys!=5) stop("Els anys nom?s poden ser 5 o 10")
  ## homes
    # prevalences
  meansmen <- c(
  55.83480492,
  0.295751264,
  0.24013282
  )
    # betes
  coefsmen <- c(
  0.061344022,
  0.345387394,
  0.251189022
  )
    # supervivencia basal
  survmen <- 0.942410683
  if (anys==5) survmen <- exp(log(survmen)/2)

  ## dones
    # prevalences
  meanswom <- c(
  55.77008491,
  0.133638145,
  0.300326584
  )
    # betes
  coefswom <- c(
  0.088920213,
  0.85630136,
  0.010613123
  )
    # supervivencia basal
  survwom <- 0.977895208
  if (anys==5) survwom <- exp(log(survwom)/2)

  ## matriu de disseny
  X <- cbind(edad,fuma,as.integer(imc>30))
  ## calcul risc
  lpmen <- X%*%coefsmen - sum(meansmen*coefsmen)
  lpwom <- X%*%coefswom - sum(meanswom*coefswom)
  risc <- ifelse(sexo==1,1-survmen^exp(lpmen),1-survwom^exp(lpwom))
  risc <- ifelse(edad>rang.edat[2] | edad<rang.edat[1], NA, risc)
  return(risc)
}







###### llarg ##########


### CHD
#'@export
calculadora.fresco.llarg<-function(sexo,edad,fuma,diab,tas,coltot,hdl,txhta,rang.edat=c(35,79), anys=10){
  if (anys!=10 & anys!=5) stop("Els anys nom?s poden ser 5 o 10")
  ## homes
    # prevalences
  meansmen<-c(
  56.04231884,
  0.289440994,
  0.178302277,
  13.73509745,
  21.84697524,
  4.946438675,
  0.115031056,
  14.82078675,
  778.77705
  )
    # betes
  coefsmen<-c(
  0.241493577,
  2.453468438,
  0.528334325,
  0.887644516,
  0.061045576,
  -0.210672879,
  0.518628309,
  -0.033513655,
  -0.013332921
  )
    # supervivencia basal
  survmen<-0.96904572
  if (anys==5) survmen <- exp(log(survmen)/2)

  ## dones
    # prevalences
  meanswom<-c(
  55.94395407,
  0.134696807,
  0.12787944,
  13.29916756,
  22.27253254,
  5.857197488,
  0.142662361,
  0,
  0
  )
    # betes
  coefswom<-c(
  0.065613785,
  0.784366378,
  0.778327789,
  0.03764199,
  0.076892885,
  -0.272066738,
  0.133493885,
  0,
  0
  )
    # supervivencia basal
  survwom<-0.990252709
  if (anys==5) survwom <- exp(log(survwom)/2)

  ## matriu de disseny
  X <- cbind(edad,fuma,diab,tas/10,coltot/10,hdl/10,as.integer(tas>120)*txhta,edad*fuma,edad*tas/10)
  ## calcul risc
  lpmen <- X%*%coefsmen - sum(meansmen*coefsmen)
  lpwom <- X%*%coefswom - sum(meanswom*coefswom)
  risc <- ifelse(sexo==1,1-survmen^exp(lpmen),1-survwom^exp(lpwom))
  risc <- ifelse(edad>rang.edat[2] | edad<rang.edat[1], NA, risc)
  return(risc)
}


### CVD
#'@export
calculadora.fresco.llarg.cvd<-function(sexo,edad,fuma,diab,tas,coltot,hdl,txhta,rang.edat=c(35,79), anys=10){
  if (anys!=10 & anys!=5) stop("Els anys nom?s poden ser 5 o 10")
  ## homes
    # prevalences
  meansmen<-c(
  56.04384514,
  0.289543349,
  0.17844143,
  13.73711132,
  21.84766168,
  4.94662864,
  0.115569159,
  14.8281767,
  778.9060538
  )
    # betes
  coefsmen<-c(
  0.198322106,
  1.913084273,
  0.518695523,
  0.727720896,
  0.044916647,
  -0.139309534,
  0.329632782,
  -0.026317654,
  -0.009980717
  )
    # supervivencia basal
  survmen<-0.951029334
  if (anys==5) survmen <- exp(log(survmen)/2)

  ## dones
    # prevalences
  meanswom<-c(
  55.94395407,
  0.134696807,
  0.12787944,
  13.29916756,
  22.27253254,
  5.857197488,
  0.142662361,
  0,
  0
  )
    # betes
  coefswom<-c(
  0.076537226,
  0.826336373,
  0.684014207,
  0.038081578,
  0.032467775,
  -0.168130749,
  0.119359496,
  0,
  0
  )
    # supervivencia basal
  survwom<-0.98091908
  if (anys==5) survwom <- exp(log(survwom)/2)

  ## matriu de disseny
  X <- cbind(edad,fuma,diab,tas/10,coltot/10,hdl/10,as.integer(tas>120)*txhta,edad*fuma,edad*tas/10)
  ## calcul risc
  lpmen <- X%*%coefsmen - sum(meansmen*coefsmen)
  lpwom <- X%*%coefswom - sum(meanswom*coefswom)
  risc <- ifelse(sexo==1,1-survmen^exp(lpmen),1-survwom^exp(lpwom))
  risc <- ifelse(edad>rang.edat[2] | edad<rang.edat[1], NA, risc)
  return(risc)
}



######### comprovaci? ##########

# xdat$frescoshort2<-with(xdat,calculadora.fresco.curt(sex,age,smk,bmi))*100
# xdat$frescolong2<-with(xdat,calculadora.fresco.llarg(sex,age,smk,diab,tas,coltot,hdldir,htamed))*100

  # curt
# xdat[,c("sex","age","smk","bmi",grep("frescoshort",names(xdat),value=TRUE))]
# rev(sort(abs(xdat$frescoshort-xdat$frescoshort2)))

  # llarg
# xdat[,c("sex","age","smk","diab","tas","coltot","hdldir","htamed",grep("frescoshort",names(xdat),value=TRUE))]
# rev(sort(abs(xdat$frescolong-xdat$frescolong2)))



