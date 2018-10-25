#'arregla formats
#'
#'@param taula data.frame o matrix
#'@param identif NULL
#'@param force: si és TRUE arregla els formats, encara que l'atribut "formats.arreglats" sigui TRUE.
#'@param rate.miss.date = 0.5
#'@note si la variable data no està definida com a chron, aleshores hauran d'està en el següent format, si no ho deixa a caràcter.
#'les dates estan separades per algun d'aquests caràcters: '-', '/', '.'.
#'les hores estan separades per el caràcter ':'.
#'
#'si la variable està en format data, o sigui, chron, estarà en el format (veure ?chron):
#'les dates day-mon-years
#'les hores com a h:m:s

#' @export
arregla.formats<-function(taula,identif=NULL,force=FALSE,rate.miss.date=0.5){

# "chron","Hmisc","gdata"

  options(warn=-1) # no ensenya els warnings per pantalla

  taula<-as.data.frame(taula)  # per permetre entrar variables individualment d'un data.frame

  taula.original<-taula    # taula original: per no perdre les dades originals

  formats<-rep(NA,ncol(taula))
  taula.perduts<-taula.blank<-taula.perduts.numeric<-taula.perduts.antic<-taula.string<-list()

  for (j in 1:ncol(taula)){ ## bucle per a totes les variables


    if (force || is.null(attr(taula[,j],"formats.arreglats")) || !attr(taula[,j],"formats.arreglats")){   ### NO FA RES SI TAULA JA TÉ ELS FORMATS ARREGLATS ##

      cat("\n\n-----Arreglant variable '",names(taula)[j],"'---------\n")

      format.antic<-attr(taula[,j],"format.SPSS")

      taula.perduts[[j]]<-taula.blank[[j]]<-taula.perduts.antic[[j]]<-taula.string[[j]]<-0

      var<-as.character(taula[,j])
      var<-trim(var)
      var<-ifelse(var=="",NA,var)  # si ja era caràcter els "" es consideren NA.

      if (sum(var=="NA",na.rm=TRUE)==nrow(taula)) var<-rep(NA,nrow(taula))

      assignat=FALSE

      ### VARIABLE BUIDA ###

      if (!assignat & sum(is.na(var))==nrow(taula)){
        formats[j]<-"F8.2"
        taula[,j]<-var
        assignat=TRUE
      }

      ### VARIABLE NUMÈRICA ###

      if (!assignat){
        aux<-var
        index.blank<-grep(" ",aux)
        aux<-gsub(" ","",aux)
        aux<-gsub(",",".",aux)
        aux<-gsub("[\\.]+",".",aux)  # posem 45..32 -> 45.32
        aux<-ifelse(aux==".",NA,aux)  # considerem que un punt (o abans una coma) sola de fet és un missing
        na.abans<-is.na(aux)
        na.despres<-is.na(as.double(aux))
        if (all(is.na(aux))){ # tots són missings
          taula[,j]<-aux
          formats[j]<-"F8.2"
          assignat=TRUE
        }else{
          if (sum(na.abans)==sum(na.despres)){   # no perd cap valor
            var<-as.double(aux)
            enters<-trunc(var)
            decimals<-var-trunc(var)
  			    num.enters<-max(nchar(as.character(enters[!is.na(enters)])))
    			  num.dec<-max(nchar(format(decimals[!is.na(decimals)])))-2
    			  num.dec<-ifelse(num.dec<0,0,num.dec)
   			    num.dec<-ifelse(num.dec>16,16,num.dec)  #com a màxim pot haver-hi 16 decimals!
    			  formats[j]<-paste("F",num.enters+num.dec+1,".",num.dec,sep="")
            taula[,j]<-var
      		  assignat=TRUE
      		  if (length(index.blank)>0){ # ho ha pogut convertir a numèric però en alguns hi ha espais en blanc (ex. 2 1.4 --> 21.4)
              if (!is.null(identif)){
                taula.blank[[j]]<-eval(parse(text=paste("data.frame(list(",identif,"=taula[index.blank,identif],ara=taula[index.blank,j],abans=taula.original[index.blank,j]))",sep="")))
              }
              if (is.null(identif)){
                taula.blank[[j]]<-data.frame(list(row_num=index.blank,ara=taula[index.blank,j],abans=taula.original[index.blank,j]))
              }
              cat("\nHan aparegut espais en blanc que s'han eliminat en la variable per als individus:\n")
              print(taula.blank[[j]])
              cat("\n\n")
            }
         } # si perd algun valor no ho toca.
        }
      }


      ### VARIABLES FORMAT DATA ###

      if (!assignat){

        var.class<-class(taula[,j])
        if (is.null(var.class) || !sum(var.class%in%c("dates","times"))==2){ # el format no està assignat a chron.

          na.abans<-is.na(var)  # així ens assegurem de no perdre cap valor

          #posem el format en /.
          aux<-var
          aux<-gsub("  "," ",aux)
          aux<-gsub("\\)","",aux)
          aux<-gsub("\\(","",aux)
          aux<-gsub("-","/",aux)
          aux<-gsub("\\.","/",aux)

          ## dies
          primer<-unlist(lapply(strsplit(aux,"/"),function(x) x[1]))
          ## mesos
          segon<-unlist(lapply(strsplit(aux,"/"),function(x) x[2]))
             # suposant que els mesos estan en literal
          segon<-tolower(segon)
            # en anglés (com en R ó en SPSS)
          segon[grep("^jan",segon)]<-1
          segon[grep("^feb",segon)]<-2
          segon[grep("^mar",segon)]<-3
          segon[grep("^apr",segon)]<-4
          segon[grep("^may",segon)]<-5
          segon[grep("^jun",segon)]<-6
          segon[grep("^jul",segon)]<-7
          segon[grep("^aug",segon)]<-8
          segon[grep("^sep",segon)]<-9
          segon[grep("^oct",segon)]<-10
          segon[grep("^nov",segon)]<-11
          segon[grep("^dec",segon)]<-12
             # en castellà
          segon[grep("^ene",segon)]<-1
          segon[grep("^feb",segon)]<-2
          segon[grep("^mar",segon)]<-3
          segon[grep("^abr",segon)]<-4
          segon[grep("^may",segon)]<-5
          segon[grep("^jun",segon)]<-6
          segon[grep("^jul",segon)]<-7
          segon[grep("^ago",segon)]<-8
          segon[grep("^sep",segon)]<-9
          segon[grep("^oct",segon)]<-10
          segon[grep("^nov",segon)]<-11
          segon[grep("^dic",segon)]<-12
             # en català
          segon[grep("^gen",segon)]<-1
          segon[grep("^feb",segon)]<-2
          segon[grep("^mar",segon)]<-3
          segon[grep("^abr",segon)]<-4
          segon[grep("^mai",segon)]<-5
          segon[grep("^jun",segon)]<-6
          segon[grep("^jul",segon)]<-7
          segon[grep("^ago",segon)]<-8
          segon[grep("^set",segon)]<-9
          segon[grep("^oct",segon)]<-10
          segon[grep("^nov",segon)]<-11
          segon[grep("^dic",segon)]<-12
          ## mesos i potser també segons
          tercer.aux<-unlist(lapply(strsplit(aux,"/"),function(x) x[3]))
          tercer.aux<-trim(tercer.aux)  # si no donaria errors en formats tipus 1/1/ 2001
          tercer<-unlist(lapply(strsplit(tercer.aux," "),function(x) x[1]))
          if (length(grep(" ",tercer.aux))>0) quart<-unlist(lapply(strsplit(tercer.aux," "),function(x) x[2]))     # ha de ser la hora.

          #els individus amb dates del tipus ' / /1999', o ' // ' es queden a missing.
          data.missings<-apply(is.na(cbind(as.integer(primer),as.integer(segon),as.integer(tercer))),1,any)
          if (sum(data.missings)>0) primer[data.missings]<-segon[data.missings]<-tercer[data.missings]<-rep(NA,sum(data.missings))

          primer<-as.integer(primer)
          segon<-as.integer(segon)
          tercer<-as.integer(tercer)

          if (sum(is.na(primer))<nrow(taula) && sum(is.na(segon))<nrow(taula) && sum(is.na(tercer))<nrow(taula)){ ## no podem convertir cap valor a data: ho deixem a caràcter

            aux2<-paste(as.character(primer),as.character(segon),as.character(tercer),sep="/")
            # totes les 6 possibilitats de "més a menys normals"
            format.data<-matrix(NA,nrow(taula),6) # les sis possibilitats

            format.data[,1]<-ifelse(primer%in%c(1:31) & segon%in%1:12 & (tercer>31 | tercer<10),1,0)  #{dates<-dates(aux2,format=c(dates = "d/m/y"));format.data=c(format.data,"d/m/y")}
            format.data[,2]<-ifelse(primer%in%c(1:12) & segon%in%1:31 & (tercer>31 | tercer<10),1,0)  #{dates<-dates(aux2,format=c(dates = "m/d/y"));format.data=c(format.data,"m/d/y")}
            format.data[,3]<-ifelse((primer>31 | primer<10) & segon%in%1:12 & tercer%in%1:31,1,0)  #{dates<-dates(aux2,format=c(dates = "y/m/d"));format.data=c(format.data,"y/m/d")}
            format.data[,4]<-ifelse((primer>31 | primer<10) & segon%in%1:31 & tercer%in%1:12,1,0)  #{dates<-dates(aux2,format=c(dates = "y/d/m"));format.data=c(format.data,"y/d/m")}
            format.data[,5]<-ifelse(primer%in%1:12 & (segon>31 | segon<10) & tercer%in%1:31,1,0)   #{dates<-dates(aux2,format=c(dates = "m/y/d"));format.data=c(format.data,"m/y/d")}
            format.data[,6]<-ifelse(primer%in%1:31 & (segon>31 | segon<10) & tercer%in%1:12,1,0)  #{dates<-dates(aux2,format=c(dates = "d/y/m"));format.data=c(format.data,"d/y/m")}

            colnames(format.data)<-c("d/m/y","m/d/y","y/m/d","y/d/m","m/y/d","d/y/m")
            formats.data.possibles<-colnames(format.data)[apply(format.data==1,2,any)]

            format.data<-names(sort(colSums(format.data),decreasing = TRUE))[1] # agafem el format més freqüent, i si hi ha empat, el primer d'ells.

            dates<-dates(aux2,format=c(dates=format.data),out.format=c(dates="day-mon-year"))



            if (inherits(try(get("quart"),silent=TRUE), "try-error")){ ## format data
              var<-chron(dates=dates,out.format=c(dates="day-mon-year"))
              taula[,j]<-var
              formats[j]<-"DATE11"
              assignat=TRUE

            } else {     ### format data hora

              quart<-gsub("\\.",":",quart)  # separats per ':'.
              time.primer<-unlist(lapply(strsplit(quart,":"),function(x) x[1]))
              time.segon<-unlist(lapply(strsplit(quart,":"),function(x) x[2]))
              time.tercer<-unlist(lapply(strsplit(quart,":"),function(x) x[3]))

              if (!any(is.na(as.double(time.primer)) || is.na(as.double(time.segon)) || is.na(as.double(time.tercer)))){ # si no passa això no podem convertir-ho a hora

                time.primer<-as.integer(time.primer)
                time.segon<-as.integer(time.segon)
                time.tercer<-as.integer(time.tercer)

                format.time<-NULL
                aux2<-paste(as.character(time.primer),as.character(time.segon),as.character(time.tercer),sep=":")
                # totes les 6 possibilitats de "més a menys normals"
                time.primer.aux<-time.primer[!is.na(time.primer)]
                time.segon.aux<-time.segon[!is.na(time.segon)]
                time.tercer.aux<-time.tercer[!is.na(time.tercer)]
                if (all(time.primer.aux%in%0:60) & all(time.segon.aux%in%0:60) & all(time.tercer.aux%in%0:24))  {times<-times(aux2,format=c(times = "s:m:h"));format.time=c(format.time,"s:m:h")}
                if (all(time.primer.aux%in%0:24) & all(time.segon.aux%in%0:60) & all(time.tercer.aux%in%0:60))  {times<-times(aux2,format=c(times = "h:m:s"));format.time=c(format.time,"h:m:s")}
                if (all(time.primer.aux%in%0:24) & all(time.segon.aux%in%0:60) & all(time.tercer.aux%in%0:60))  {times<-times(aux2,format=c(times = "h:s:m"));format.time=c(format.time,"h:s:m")}
                if (all(time.primer.aux%in%0:60) & all(time.segon.aux%in%0:60) & all(time.tercer.aux%in%0:24))  {times<-times(aux2,format=c(times = "m:s:h"));format.time=c(format.time,"m:s:h")}
                if (all(time.primer.aux%in%0:60) & all(time.segon.aux%in%0:24) & all(time.tercer.aux%in%0:60))  {times<-times(aux2,format=c(times = "m:h:s"));format.time=c(format.time,"m:h:s")}
                if (all(time.primer.aux%in%0:60) & all(time.segon.aux%in%0:24) & all(time.tercer.aux%in%0:60))  {times<-times(aux2,format=c(times = "s:h:m"));format.time=c(format.time,"s:h:m")}

                if (!is.null(format.time)){
                  if (length(format.time)>1){
                    format.time<-format.time[1]  # n'agafem un: el primer.
                    times<-times(aux2,format=c(times = format.time),out.format=c(times="h:m:s"))
                  }
                  var<-chron(dates,times)
                  taula[,j]<-var
                  formats[j]<-"DATETIME23"
                  assignat=TRUE
                } else{
                  cat("ADVERTÈNCIA: La variable té segons, minuts o hores impossibles: només es posaran els dies","\n\n") # s'ha de quedar a caràcter
                  var<-chron(dates)
                  taula[,j]<-var
                  formats[j]<-"DATE11"
                  assignat=TRUE
                }

              }
            }
          } # no podem convertir a data cap valor: ho deixem a caràcter

        } else { # el format ja està assignat a chron, però hem de posar els formats estàndars (day-mon-year h:m:s)

          var<-taula[,j]

          if (length(attributes(var)$format)>1){ # té dies i hores
            var<-chron(var,out.format=c(dates="day-mon-year",times="h:m:s"))
            taula[,j]<-var
            formats[j]<-"DATETIME23"
            assignat=TRUE
          } else { # només té dies
            var<-chron(var,out.format=c(dates="day-mon-year"))
            taula[,j]<-var
            formats[j]<-"DATE11"
            assignat=TRUE
          }
        }

        if (assignat) na.despres<-is.na(var)

        if (assignat && sum(na.despres)>sum(na.abans)){  # per algun motiu hem perdut algun valor en el procés de conversió
          if ((sum(na.despres)-sum(na.abans))/sum(!na.abans)>rate.miss.date){     # s'han perdut més del 50% dels valors en el procés de conversió
            assignat=FALSE
            var<-taula.original[,j] # torna a posar tal i com estava
            cat("La variable no s'ha convertit a data perquè es perden més del",rate.miss.date*100,"% dels valors")
          }else{ # si es perden menys del 50% es deixa a data però s'informa de quins s'han perdut.

            ######  TAULA DE PERDUTS EN LA CONVERSIÓ A DATA #######
            llista.perduts<-taula.original[na.despres!=na.abans,j]
            if (!is.null(identif)){
              num_indiv=taula[na.despres!=na.abans,identif]
              taula.perduts[[j]]<-eval(parse(text=paste("data.frame(list(",identif,"=num_indiv,valors=llista.perduts))",sep="")))
            }
            if (is.null(identif)){
              num_indiv=which(na.despres!=na.abans)
              taula.perduts[[j]]<-data.frame(list(row_num=num_indiv,valors=llista.perduts))
            }
            cat("\n\nS'han perdut",nrow(taula.perduts),"valors de la variable en la conversió a format data\n\n")
            print(taula.perduts[[j]])
            cat("\n\n")

          }
        }

        ## dates anteriors a "14/10/1582" que 'R' entén bé però que a l'exportar-ho a SPSS queden com a missing si es defineixen en format data
        if (assignat){
          llista.perduts.antic<-(var<chron(dates="14/10/1582",format=c(dates="d/m/y")))
          llista.perduts.antic<-ifelse(is.na(llista.perduts.antic),FALSE,llista.perduts.antic)
        }
        if (assignat && sum(llista.perduts.antic)>0){
          if (!is.null(identif)){
            num_indiv=taula[llista.perduts.antic,identif]
            taula.perduts.antic[[j]]<-eval(parse(text=paste("data.frame(list(",identif,"=num_indiv,valors=taula.original[llista.perduts.antic,j]))",sep="")))
          }
          if (is.null(identif)){
            num_indiv=which(llista.perduts.antic)
            taula.perduts.antic[[j]]<-data.frame(list(row_num=num_indiv,valors=as.character(taula.original[llista.perduts.antic,j])))
          }
          cat("\n\nEn l'exportació a SPSS es perdran els valors de la variable per ser dates anteriors a 14/10/1582 per als individus:\n")
          print(taula.perduts.antic[[j]])
          cat("\n\n")
        }
      }


      ### VARIABLES CARÀCTER (si no ho ha assignat ni a numèric ni a data, es queda a caràcter

      if (!assignat){ ## es queda a caràcter
        taula[,j]<-as.character(var)
        tempx <- taula[,j]
        tempx <- ifelse(is.na(tempx)," ",tempx)
        formats[j]<-paste("A",max(nchar(tempx)),sep="")
        index<-unique(c(grep("\n",taula[,j]),grep("\t",taula[,j]),grep("\r",taula[,j])))
        if (length(index)>0){
          var.antic<-taula[index,j]
          taula[,j]<-gsub("\n"," ",taula[,j]) # salt de carro substituit a espai en blanc
          taula[,j]<-gsub("\t"," ",taula[,j]) # tabulador substituit a espai en blanc
          taula[,j]<-gsub("\r"," ",taula[,j]) # quadrat substituit a espai en blanc
          if (!is.null(identif)){
            num_indiv=taula[index,identif]
            taula.string[[j]]<-eval(parse(text=paste("data.frame(list(",identif,"=num_indiv,valors=var.antic",sep="")))
          }
          if (is.null(identif)){
            num_indiv=index
            taula.string[[j]]<-data.frame(list(row_num=num_indiv,valors=var.antic))
          }
          cat("\n\nS'han eliminat tabuladors ('\\t') salts de carro ('\\n') i/o quadrats ('\\r') per als individus:\n")
          print(taula.string[[j]])
          cat("\n\n")
        }
        assignat=TRUE
      }

      ## tornem a recuperar els value labels i vari.label que hem perdut en el procés d'arreglar els formats.
      ## i assignem a cada variable el seu format.SPSS.
      attr(taula[,j],"value.labels")<-attr(taula.original[,j],"value.labels")
      attr(taula[,j],"vari.label")<-attr(taula.original[,j],"vari.label")
      attr(taula[,j],"format.SPSS")<-formats[j]
      attr(taula[,j],"formats.arreglats")<-TRUE

      cat("\n   Se li ha assignat el format 'SPSS':",formats[j],"     \n")
      if (!is.null(format.antic)) cat("   Abans tenia el format 'SPSS'",format.antic,"\n\n")
      # si tenia value.labels i era caràcter i l'hem convertit a numèric llista els value labels que tenia.
      if (!is.null(format.antic) && (sub("[0-9.]+$","",format.antic)=="A" & sub("[0-9.]+$","",formats[j])=="F" & !is.null(attr(taula[,j],"value.labels")))){
        cat("\nLa variable tenia les següents etiquetes de valor:\n")
        print(attr(taula[,j],"value.labels"))
        attr(taula[,j],"value.labels")=structure(as.double(attr(taula[,j],"value.labels")),names=names(attr(taula[,j],"value.labels")))
        cat("\nque ara passen a ser:\n")
        print(attr(taula[,j],"value.labels"))
      }
    }  ## no fa res si els formats estan arreglats

    if (!inherits(try(get("quart"),silent=TRUE), "try-error")) rm(quart) # si exiteix la variable quart l'elimina.

  } ## FI BUCLE

  options(warn=0)  # a partir d'aquí torna a ensenyar els warnings

  return(taula)

}








