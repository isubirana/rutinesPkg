#' calcula AF
#' @param af base de datos actifidad física
#' @export


PPlus.AF <- function(af) {

  #variables --> dies_camina,min_camina,dies_passeja,min_passeja,dies_excursio,min_excursio,
                #dies_escales,min_escales,dies_hort,min_hort,dies_esport,min_esport

  filt <- c(NA,88,99,888,999)

  ####  FAMILIAR
  af$dies_passeja <- with(af,ifelse(dies_passeja %in% filt, 0, dies_passeja))
  af$min_passeja <-  with(af,ifelse(min_passeja  %in% filt, 0, min_passeja))

  af$dies_camina <- with(af,ifelse(dies_camina %in% filt, 0, dies_camina))
  af$min_camina <-  with(af,ifelse(min_camina  %in% filt, 0, min_camina))

  af$dies_hort <- with(af,ifelse(dies_hort %in% filt, 0, dies_hort))
  af$min_hort <-  with(af,ifelse(min_hort  %in% filt, 0, min_hort))

  af$dies_excursio <- with(af,ifelse(dies_excursio %in% filt, 0, dies_excursio))
  af$min_excursio <-  with(af,ifelse(min_excursio  %in% filt, 0, min_excursio))

  af$dies_escales <- with(af,ifelse(dies_escales %in% filt, 0, dies_escales))
  af$min_escales <-  with(af,ifelse(min_escales  %in% filt, 0, min_escales))

  af$dies_esport <- with(af,ifelse(dies_esport %in% filt, 0, dies_esport))
  af$min_esport <-  with(af,ifelse(min_esport  %in% filt, 0, min_esport))


  af$geaf_lig<-with(af,ifelse(is.na(dies_passeja) | dies_passeja%in%c(88,99) | is.na(min_passeja)  | min_passeja%in%c(888,999),
                          99999,(dies_passeja*min_passeja*4)/4.29))

  af$geaf_mod<-with(af,ifelse(is.na(dies_camina) | dies_camina%in%c(88,99) | is.na(min_camina) | min_camina%in%c(888,999) |
                                 is.na(dies_hort)   | dies_hort%in%c(88,99)   | is.na(min_hort)   | min_hort%in%c(888,999),
                          99999,(dies_camina*min_camina*5)/4.29 + (dies_hort*min_hort*5)/4.29))

  af$geaf_int<-with(af,ifelse(is.na(dies_excursio) | dies_excursio%in%c(88,99) | is.na(min_excursio) | min_excursio%in%c(888,999) |
                                 is.na(dies_escales)  | dies_escales%in%c(88,99)  | is.na(min_escales)  | min_escales%in%c(888,999) |
                                 is.na(dies_esport)   | dies_esport%in%c(88,99)   | is.na(min_esport)   | min_esport%in%c(888,999),
                          99999,(dies_excursio*min_excursio*6)/4.29 + (dies_escales*min_escales*4)/4.29 + (dies_esport*min_esport*10)/4.29))

  af$geaf_tot<-with(af,ifelse(is.na(dies_passeja)  | dies_passeja%in%c(88,99)  | is.na(min_passeja)  | min_passeja%in%c(888,999) |
                                 is.na(dies_camina)   | dies_camina%in%c(88,99)   | is.na(min_camina)   | min_camina%in%c(888,999) |
                                 is.na(dies_hort)     | dies_hort%in%c(88,99)     | is.na(min_hort)     | min_hort%in%c(888,999) |
                                 is.na(dies_excursio) | dies_excursio%in%c(88,99) | is.na(min_excursio) | min_excursio%in%c(888,999) |
                                 is.na(dies_escales)  | dies_escales%in%c(88,99)  | is.na(min_escales)  | min_escales%in%c(888,999) |
                                 is.na(dies_esport)   | dies_esport%in%c(88,99)   | is.na(min_esport)   | min_esport%in%c(888,999),
                          99999,(dies_passeja*min_passeja*4)/4.29 +
                                (dies_camina*min_camina*5)/4.29 + (dies_hort*min_hort*5)/4.29 +
                                (dies_excursio*min_excursio*6)/4.29 + (dies_escales*min_escales*4)/4.29 + (dies_esport*min_esport*10)/4.29))

  summary(af$geaf_lig)
  summary(af$geaf_mod)
  summary(af$geaf_int)
  summary(af$geaf_tot)

  af$geaf_lig <- round(af$geaf_lig,2)
  af$geaf_mod <- round(af$geaf_mod,2)
  af$geaf_int <- round(af$geaf_int,2)
  af$geaf_tot <- round(af$geaf_tot,2)

  attr(af$geaf_lig,"vari.label")<-"Gasto energético en actividad física ligera (MET·min/sem)"
  attr(af$geaf_mod,"vari.label")<-"Gasto energético en actividad física moderada (MET·min/sem)"
  attr(af$geaf_int,"vari.label")<-"Gasto energético en actividad física intensa (MET·min/sem)"
  attr(af$geaf_tot,"vari.label")<-"Gasto energético en actividad física total (MET·min/sem)"



  return(af)


}


