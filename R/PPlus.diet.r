#' Calcula dieta
#' @param diet dieta
#' @param bd (default is 0)
#' @import car
#' @export
PPlus.diet <- function(diet, bd=0) {
  #bd=0 Halo
  #bd=1 B25

  if (bd==0){
      diet$p14_1 <- diet$dieta_1
      diet$p14_2 <- diet$dieta_2
      diet$p14_3 <- with(diet,ifelse(dieta_3a==0,0,ifelse(dieta_3a==1 & dieta_3b==0,0,1)))
      diet$p14_4 <- car::recode(diet$dieta_4,"0:2=0;3:4=1")
      diet$p14_5 <- car::recode(diet$dieta_5,"0=1;1:2=0")
      diet$p14_6 <- car::recode(diet$dieta_6,"0=1;1:2=0")
      diet$p14_7 <- car::recode(diet$dieta_7,"0=1;1:2=0")
      diet$p14_8 <- car::recode(diet$dieta_8,"0:1=0;2=1")
      diet$p14_9 <- car::recode(diet$dieta_9,"0:2=0;3:4=1")
      diet$p14_10 <- car::recode(diet$dieta_10,"0:2=0;3:4=1")
      diet$p14_11 <- car::recode(diet$dieta_11,"0:1=1;2=0")
      diet$p14_12 <- car::recode(diet$dieta_12,"0:2=0;3:4=1")
      diet$p14_13 <- diet$dieta_13
      diet$p14_14 <- car::recode(diet$dieta_14,"0:1=0;2:3=1")
  }

  if (bd==1){
      diet$p14_1 <- diet$dieta_1
      diet$p14_2 <- car::recode(diet$dieta_2,"1=0;2=1")
      diet$p14_3 <- with(diet,ifelse(dieta_3a==0,0,ifelse(dieta_3a==1 & dieta_3b==0,0,1)))
      diet$p14_4 <- car::recode(diet$dieta_4,"0:2=0;3:4=1")
      diet$p14_5 <- car::recode(diet$dieta_5,"0=1;1:2=0")
      diet$p14_6 <- car::recode(diet$dieta_6,"0=1;1:2=0")
      diet$p14_7 <- car::recode(diet$dieta_7,"0=1;1:2=0")
      diet$p14_8 <- diet$dieta_8
      diet$p14_9 <- car::recode(diet$dieta_9,"0:2=0;3:4=1")
      diet$p14_10 <- car::recode(diet$dieta_10,"0:2=0;3:4=1")
      diet$p14_11 <- car::recode(diet$dieta_11,"0:1=1;2:3=0")
      diet$p14_12 <- car::recode(diet$dieta_12,"0:2=0;3:4=1")
      diet$p14_13 <- diet$dieta_13
      diet$p14_14 <- car::recode(diet$dieta_14,"0:1=0;2:3=1")
  }

  diet$punts <- apply(diet[,paste("p14_",1:14,sep="")],1,sum,na.rm=TRUE)
  attr(diet$punts,"vari.label") <- "Suma de puntuación de P14"

  diet$adhesio <- with(diet,ifelse(punts>8,1,ifelse(punts<9,0,9)))

  varis <- names(diet[,paste("p14_",1:14,sep="")])
  diet <- remove.vars(diet,varis)


  return(diet)


}
