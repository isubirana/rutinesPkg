#' causa.exitus_v2
#' causa exitus
#' @param cie cie9 or cie10
#' @param status Por defecto "1='muerto'"
#' @param fexit 'diaexit' or 'mesexit' or 'anyoexit'
#' @param cexit default NULL
#' @param sexitus<-Aqui se pondra el valor que tienen los muertos en la variable 'status'
#' @param sobran si es '0', se devuelve la causa de exitus, si es '>0' se devuelve 'xcieIsobran' (si es CIE que sobra o no) (los CIEs que sobran)
#' @example \donttest{
#' x1<-data.frame(cie=cie_1_9,tipo="cie_1_9")
#' x2<-data.frame(cie=cie_2_9,tipo='cie_2_9')
#' x3<-data.frame(cie=cie_3_9,tipo='cie_3_9')
#' x4<-data.frame(cie=cie_4_9,tipo='cie_4_9')
#' x5<-data.frame(cie=cie_1_10,tipo='cie_1_10')
#' x6<-data.frame(cie=cie_2_10,tipo='cie_2_10')
#' x7<-data.frame(cie=cie_3_10,tipo='cie_3_10')
#' x8<-data.frame(cie=cie_4_10,tipo='cie_4_10')
#' x9<-data.frame(cie=cie_3_9_sobran,tipo='cie_3_9_sobran')
#' x<-as.data.frame(rbind(x1,x2,x3,x4,x5,x6,x7,x8,x9))
#' }
#' @export

causa.exitus_v2 <- function(cie, status=9, fexit=NULL, cexit=NULL, sexitus=1, sobran=0) {



  ####  CEXIT 1   (MUERTE POR CARDIOPATIA ISQU?MICA)
  cie_1_9<-c(seq(410,414,by=1),seq(4100,4149,by=1))

  cie_1_10<-c('I200','I201','I208','I209',
              paste('I',210:229,sep=""),
             'I236',paste('I',240:249,sep=""),paste('I',251:259,sep=""))


  ####  CEXIT 2 (MUERTE POR ENFER. CEREBROVASCULAR)
  cie_2_9<-c(seq(430,434,by=1),seq(4300,4349,by=1),
             seq(436,438,by=1),seq(4360,4389,by=1),
            # seq(4375,4377,by=1),
             #seq(4379,4389,by=1),
             435,seq(4350,4359,by=1))

  cie_2_10<-c(paste('I',600:639,sep=""),'I64',paste('I',670:679,sep=""),
              paste('I',688:698,sep=""),
              paste('I',650:669,sep=""),paste('G',450:452,sep=""),paste('G',454:463,sep=""),'G468',
             'F011','F019')
  #            'I64X','I67',


  ####  CEXIT 3 (MUERTE POR OTRAS CAUSAS CV)
  cie_3_9<-c(4438,4439,
             401,seq(4010,4019,by=1),
             402,seq(4020,4029,by=1),
             403,seq(4030,4039,by=1),
             404,seq(4040,4049,by=1),
             405,seq(4050,4059,by=1),
             426,seq(4260,4269,by=1),
             427,seq(4270,4273,by=1), seq(4276,4279,by=1),
             428,seq(4280,4289,by=1),
             429,seq(4290,4299,by=1),
             #seq(4291,4299,by=1),
             5184,
             441,seq(4410,4419,by=1),
             442,seq(4420,4429,by=1),
             746,seq(7460,7469,by=1),
             785,seq(7850,7854,by=1), seq(7856,7859,by=1),
             4249, 396 ,
             3949, 4150, 4151, 4169, 4200, 4230, 4239, 4240, 4241, 4250, 4251, 4254, 4255, 4599, 4512, 4479, 4471)


  cie_3_10<-c('J81',
              'I10',
              paste('I',110:139,sep=""),
              paste('I',150:159,sep=""),
              paste('I',440:455,sep=""),'I458','I459','I498',
              paste('I',470:479,sep=""),'I48',paste('I',491:495,sep=""),'I499','R008','R012',
              paste('I',500:529,sep=""),'R570','R571',
              paste('I',231:238,sep=""), paste('I',515:516,sep=""),'K558','K559',
              paste('I',720:729,sep=""),
              'Q213','Q245',
              #'I050','I052','I059','I069','I080','I081',
              '4249','I050','I052','I059','I069','I080','I081','I260','I269','I270','I279','I313','I319','I330',
              'I340','I350','I351','I352','I358','I359','I379','I38','I420','I421','I422','I429','I803','I81',
              'I820','I821','I822','I823','I828','I829','I99')

  ####  CEXIT 5 (Arteriosclerosis Periferica)
  cie_5_9<-c(440,seq(4400,4409,by=1),
             444,seq(4440,4449,by=1),
             557,seq(5570,5579,by=1))

  cie_5_10<-c('I250', paste('I',700:719,sep=""), paste('I',738:749,sep=""),
              'I792',
              'K550', 'K551')


  ####  CEXIT 7 (Muerte subita)
  cie_7_9<-c('4274','4275',
              798,seq(7980,7989,by=1))
  cie_7_10<-c(paste('I',460:469,sep=""),'I490','R960','R961','R98','R95')



  ####  CEXIT 4 (MUERTE POR OTRA CAUSA NO CV), paso de 3 a 4: 390-445,
  #cie_4_9<-c('418','459','4590')
  #cie_4_10<-c('I647','I648','I780','I120')
  cie_4_10<-c('I00')
  #'I504','I725', Pasa a ser un 3


  ####  CIE 3 SOBRAN (Todas las: 447-459) menos las demas CIEs 9
  #x_cie_3_9_sobran<-c(seq(447,459),seq(4470,4599,by=1))
  #cie_3_9_sobran<-x_cie_3_9_sobran[x_cie_3_9_sobran%nin%c(cie_1_9,cie_2_9,cie_3_9,cie_4_9)]

  ####  CIE 4 SOBRAN (Todas las: 390-445) menos las demas CIEs 9
  #x_cie_4_9_sobran<-c(seq(390,445),seq(3900,4459,by=1))
  #cie_4_9_sobran<-x_cie_4_9_sobran[x_cie_4_9_sobran%nin%c(cie_1_9,cie_2_9,cie_3_9,cie_4_9,cie_3_9_sobran)]



  #todo<-c(cie_1_9,cie_1_10,cie_2_9,cie_2_10,cie_3_9,cie_3_10,cie_4_9,cie_4_10,cie_3_9_sobran,cie_4_9_sobran)


  ##############################  Asigno la causa segun el CIE  ##################
  ### Exitus con CIE = 4,
  ### Exitus sin CIE = 9

  if (is.null(cexit))
    cexit<-rep(NA,length(cie))

  if (is.null(fexit))
    fexit<-rep(NA,length(cie))

  cie_f<-ifelse(!is.na(cie),4,ifelse(!is.na(cexit),cexit,ifelse((status==sexitus & !is.na(status)) | !is.na(fexit),9,NA)))


  #CIE 1
  cie_f<-ifelse(cie%in%cie_1_9 & (cie_f==4|cie_f==9) , 1 , cie_f)
  cie_f<-ifelse(cie%in%cie_1_10 & (cie_f==4|cie_f==9) , 1 , cie_f)

  #CIE 2
  cie_f<-ifelse(cie%in%cie_2_9 & (cie_f==4|cie_f==9) , 2 , cie_f)
  cie_f<-ifelse(cie%in%cie_2_10 & (cie_f==4|cie_f==9) , 2 , cie_f)

  #CIE 3
  cie_f<-ifelse(cie%in%cie_3_9 & (cie_f==4|cie_f==9) , 3 , cie_f)
  cie_f<-ifelse(cie%in%cie_3_10 & (cie_f==4|cie_f==9) , 3 , cie_f)


  #CIE 5
  cie_f<-ifelse(cie%in%cie_5_9 & (cie_f==4|cie_f==9) , 5 , cie_f)
  cie_f<-ifelse(cie%in%cie_5_10 & (cie_f==4|cie_f==9) , 5 , cie_f)

  #CIE 7
  cie_f<-ifelse(cie%in%cie_7_9 & (cie_f==4|cie_f==9) , 7 , cie_f)
  cie_f<-ifelse(cie%in%cie_7_10 & (cie_f==4|cie_f==9) , 7 , cie_f)

  #CIE 3 (las 'I' que sobran)
  todasI<-grep("^I",cie,value=TRUE)
  Isobran<-todasI[todasI %nin% c(cie_1_10, cie_2_10, cie_3_10, cie_4_10, cie_5_10, cie_7_10)]
  xcieIsobran<-ifelse(cie%in%Isobran , 1 , 0)  #Informacion de las 'I' que sobran
  cie_f<-ifelse(cie%in%Isobran , 3 , cie_f)  #Asigno un '3' a las 'I' q sobran









  ################################################################################

  if (sobran==0)
    {
      return(cie_f)


    }
  else
    {
      return(Isobran)
  #    return(xcieIsobran)
    }


}




