#' nutrients.conversion
#' Cálculo de la cantidad en gramos de cada nutriente diariamente consumida por cada individuo
#' @param file.alim fitxer SPSS amb els aliments
#' @param file.coef fitxer SPSS amb els coeficients per a fer la conversió
#' @export

nutrients.conversion<-function(file.alim,file.coef){

  file1<-file.alim
  file2<-file.coef

  ######## MAIN #############

  extension1<-tolower(substr(file1,nchar(file1)-3,nchar(file1)))
  extension2<-tolower(substr(file2,nchar(file2)-3,nchar(file2)))

  if (extension1!=".sav" || extension2!=".sav") stop("Els dos arxius han de ser de SPSS")


  ### Datos de la encuesta de alimentaci?n de los individuos: formato ACCESS ###
  alim<-read.spss4(file1,FALSE)
  alim.output<-alim


  ### Datos de porcentage de cada nutriente en los alimentos ###
  coef<-read.spss4(file2,FALSE)
  coef.output<-coef

  ### Arreglamos los datos "coef": quitamos la primera columna que contiene los nombres de los alimentos ###
  # As?, la matriz contiene solo n?meros reales #

  nombre.alim<-as.character(coef[,1])
  nombre.alim<-gsub(" ","",nombre.alim) # triem els espais en blanc
  coef<-coef[nombre.alim!="",] # aliminem les files on no s'especifica cap aliment.


  ## comprova que no hi hagin nombre.alim repetits
  if (any(table(coef[,1])>1)){
    repe<-which(table(coef[,1])>1)
    cat("\n\nHi ha noms d'aliments repetits en la taula 'coef':\n")
    print(names(table(coef[,1])[repe]))
    cat("\nque estan a la/es files\n\n")
    cat(which(table(coef[,1])%in%names(table(coef[,1])[repe])))
    cat("\n\n")
    stop("Fi de la macro")
  }


  row.names(coef)<-as.character(coef[,1])
  coef<-coef[,-1]



  ### Arreglamos los datos "alim": quitamos la primera columna que contiene los identificadores de los individuos ###
  # As?, la matriz contiene solo n?meros reales #

  row.names(coef)<-toupper(gsub(" ","",row.names(coef)))  # passem a maj?scula els noms dels aliments
  names(alim)<-toupper(names(alim))
  corr<-unlist(sapply(row.names(coef),function(x) which(names(alim)==x)))

  ## aliments en coef que no es troben a alim.
  if (length(row.names(coef)[!row.names(coef)%in%names(corr)])>0){
    cat("\nEls aliments",paste(row.names(coef)[!row.names(coef)%in%names(corr)],collapse=" "),"No s'han pogut trobar a 'alim'")
  }



  ###############################################################################################
  ### Conversi?n de los datos del formato "data frame" a "matrix", necesario para los c?lculos ###

  alim.mat<-as.matrix(alim[,corr]) # nom?s seleccionem de 'alim' les variables corresponents als aliments que s? estan a 'coef'
  coef.mat<-as.matrix(coef[row.names(coef)%in%names(corr),])

  cat("\ndim BD aliments","files=",dim(alim.mat)[1],"columnes=",dim(alim.mat)[2])
  cat("\ndim BD aliments","files=",dim(coef.mat)[1],"columnes=",dim(coef.mat)[2],"\n")


  # Transformamos los missings de coef en 0
  coef.mat<-ifelse(is.na(coef.mat),0,coef.mat)     # funciona per a vectors i matrius, per? no per a data.frames

  # conversion en proporci?n (n?mero entre 0 y 1)
  coef.mat<-coef.mat/100

  # Producto matricial #
  # cantidad de alimentos*porcentage de cada nutriente en cada alimento #
  # La matriz "nutrient" resultante contiene la cantidad diaria (en gramos) #
  # de los nutrientes consumidos por cada individuo #

  nutrient<-alim.mat %*% coef.mat

  aliments.nutrients<-data.frame(list(alim,nutrient))


  #################################################################################################


  return(aliments.nutrients)

}

