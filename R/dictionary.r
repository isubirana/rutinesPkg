#'dictionary
#' CREA UN FITXER *.SPS DE VALUE LABELS I VARI LABELS D'UNA BASE DE DADES *.SAV ####
#' @param files nom dels fitxers
#' @param file.res nom del fitxer resultat


#'@export
dictionary<-function(files=NULL,file.res=""){

# files: bases de dades SPSS (.sav) de la qual extreiem la informaci? de les vari i value labels.
# file.res: arxiu de sintaxis (.sps) on va a parar tota aquesta informaci?.

  inter=FALSE

  if (is.null(files)){
    inter=TRUE
    files=choose.files(default = "*.sav", caption = "Selecciona la/es BD/'s de SPSS")
  }


  if (!is.null(files) && length(files)>0){

    if (inter){
      file.res.default=paste(dirname(files[1]),"/dictionary de ",sub(".sav","",basename(files[1])),".sps",sep="")
      file.res<-winDialogString(message="Arxiu de resultats (recorda posar l'extensi? .sps)", default=file.res.default)
    }

    write("\n\n",file.res)

    for (i in 1:length(files)){ ## bucle per a tots els arxius .sav

      file<-files[i]

      obj<-spss_varlist(file)
      bd.name<-sub(".sav","",basename(file))

      vari.labels<-obj[,"varlabel"]
      noms.var<-obj[,"longname"]
      missings<-obj[,3:6]
      #formats<-obj["printfmt"]

      dades<-read.spss(file, use.value.labels=FALSE, to.data.frame=TRUE)
      names(dades)<-noms.var
      value.labels.list<-lapply(dades,function(x) attr(x,"value.labels"))


      ## etiquetes de les variables ##

      vari.labels.sint<-
  		    c("vari labels",
  		    paste("\t",noms.var,"\t\"",vari.labels,"\"",
  		    c(rep("",length(vari.labels)-1),"."),sep=""))

      write(paste("\n\n\n********** DICTIONARY: ",bd.name,"****************.",sep=""),"xxxauxiliar.sps",append=FALSE)
      write(" ","xxxauxiliar.sps",append=TRUE)
      write(" ","xxxauxiliar.sps",append=TRUE)
      write("*** VARI LABELS ***.","xxxauxiliar.sps",append=TRUE)
      write(vari.labels.sint,"xxxauxiliar.sps",append=TRUE)


      ## etiquetes de valor ##

      value.labels.sint=NULL

      if (length(value.labels.list)>0){

        for (j in 1:length(value.labels.list)){
          #print(noms.var[j]);print(j)

          value.labels<-value.labels.list[[j]]

          if (!is.null(value.labels)){

            value.labels<-sort(value.labels)
            #names(value.labels)<-sub("'","",as.character(names(value.labels)))  # triame les comes simples.

            value.labels<-data.frame(labels=names(value.labels),values=as.double(value.labels))

            aux<-c(paste("value labels",noms.var[j]),
                paste("\t",value.labels[,2],"\t\"",value.labels[,1],"\"",
                c(rep("",nrow(value.labels)-1),"."),sep=""))

            value.labels.sint=c(value.labels.sint," ",aux)

            taula<-cbind(noms.var[i],vari.labels[i],value.labels)
            taula[,1]<-as.character(taula[,1])
            taula[,2]<-as.character(taula[,2])
            if (nrow(taula)>1){
              taula[2:nrow(taula),1]<-rep(" ",nrow(taula)-1)
              taula[2:nrow(taula),2]<-rep(" ",nrow(taula)-1)
            }
          }
        }

        write(" ","xxxauxiliar.sps",append=TRUE)
        write("*** VALUE LABELS ***.","xxxauxiliar.sps",append=TRUE)
        write(value.labels.sint,"xxxauxiliar.sps",append=TRUE)

      }



      ## missings ##

      if (any(!missings[,1]==0)){  ## si hi ha algun missing declarat

		    missing.sint=NULL

        missings[,1]<-as.integer(missings[,1])
		    look.miss<-which(!missings[,1]==0)

		    for (j in look.miss){

			   if (missings[j,1]==1) aux.miss<-paste("(",missings[j,2],")",sep="")
			   if (missings[j,1]==2) aux.miss<-paste("(",missings[j,2]," ",missings[j,3],")",sep="")
			   if (missings[j,1]==3) aux.miss<-paste("(",missings[j,2]," ",missings[j,3]," ",missings[j,4],")",sep="")
			   if (missings[j,1]==-2) aux.miss<-paste("(",missings[j,2]," thru ",missings[j,3],")",sep="")
			   if (missings[j,1]==-3) aux.miss<-paste("(",missings[j,2]," thru ",missings[j,3]," ",missings[j,4],")",sep="")

			   missing.sint<-c(missing.sint,paste("MISSING VALUES ",names(dades)[j]," ",aux.miss,".",sep=""))

        }

		    write(" ","xxxauxiliar.sps",append=TRUE)
		    write("*** MISSING VALUES ***.","xxxauxiliar.sps",append=TRUE)
		    write(" ","xxxauxiliar.sps",append=TRUE)
		    write(missing.sint,"xxxauxiliar.sps",append=TRUE)

	   }

      file.append(file.res,"xxxauxiliar.sps")

    } # bucle per a cada arxiu *.sav escollit

    file.show(file.res)

  } else cat("No s'ha escollit cap base de dades\n")

}













