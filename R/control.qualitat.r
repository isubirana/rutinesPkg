#'control.qualitat
#'genera un informe amb estadístics per a fer control de qualitat
#'@param file NULL
#'@param data.intern FALSE
#'@param max.valors 10
#'@param desc NULL
#'@param max.rare.values 4
#'@param max.factor max.valors
#'@param digits 1
#'@param file.res "xxx.xls"

#'@export
control.qualitat<-function(file=NULL,data.intern=FALSE,max.valors=10,desc=NULL,max.rare.values=4,max.factor=max.valors,digits=1,file.res="xxx.xls"){



  ############# FUNCIONS PR?VIES #################

  # funci? de retorna en format dd/mm/yyyy les dates (anys en quatre digits)
  format.data<-function(x) paste(as.character(days(x)),"-",as.character(months(x)),"-",as.character(years(x)),sep="")

  # funci? que retorna valors rars segons hclust.
  rare.values<-function(x){
    t<-table(x)
    valors<-as.double(names(t))
  	valors<-as.double(scale(valors,scale=FALSE))  ## centrem els valors
  	valors=abs(valors)
    d=dist(valors)
  	cut.tree<-try(cutree(hclust(d),2),silent=TRUE)
  	if (!is.character(cut.tree)){
      freq.rars<-as.integer(table(cut.tree))
      grup.min=as.integer(names(table(cut.tree))[which(as.integer(table(cut.tree))==min(freq.rars))])
      rare.values<-as.double(names(t)[cut.tree==grup.min])
      if (length(rare.values)<=max.rare.values){ # pocs valors allunyats de la resta: valors rars
        t.rare<-table(x[x%in%rare.values])
        res<-data.frame(list(values=names(t.rare),freq=as.integer(t.rare)))
        if (dim(res)[1]==0) res=NULL
        return(res)
      } else return(NULL)
    } else return(NULL)# no ha pogut fer ni dos clusters.
  }


  # funci? que retorna les etiquetes i els valors
  labels<-function(x){
    value.labels <- attr(x, "value.labels")
    if (!is.null(value.labels)){
      notinvl <- unique(x)[!unique(x) %in% as.numeric(value.labels)]
      notinvl <- na.omit(notinvl)
      if (length(notinvl)>0) value.labels <- c(notinvl, value.labels)
      ww <- which(names(value.labels)=="")
      if (length(ww)==1) names(value.labels)[ww] <- "[Sense etiqueta]"
      if (length(ww)>1) names(value.labels)[ww] <- paste0("[Sense etiqueta]",1:length(ww))
      t <- table(factor(x, value.labels, names(value.labels)))
      res<-data.frame(list(valors=I(names(t)),labels=I(value.labels),freq=as.integer(t)))
    }
    if (is.null(value.labels)){
      t<-table(x)
      t<-data.frame(list(valors=names(t),freq=as.integer(t)))
      res<-as.data.frame(cbind(t,as.character(t[,1])))
      names(res)<-c("valors","freq","labels")
    }
    res<-res[order(as.double(as.character(res[,2]))),]
    return(res[,c("valors","labels","freq")])
  }

  table.built<-function(x){
    if (!is.null(x)){ # per a les variables descartades
      res<-c(x$nom.var,x$label,x$n,x$perc.missing)
      if (!is.null(x$taula.freq)){
        x$taula.freq<-as.matrix(x$taula.freq)
        if (dim(x$taula.freq)[1]>1){
          res<-rbind(res,matrix("",dim(x$taula.freq)[1]-1,length(res)))
          res<-cbind(res,x$taula.freq)
        } else{
          res<-c(res,x$taula.freq)
          res<-t(as.matrix(res))
        }
        colnames(res)<-c("nom.var","label","n","perc.missing",colnames(x$taula.freq))
      } else {
        res<-t(as.matrix(c(res,x$descriptius)))
        colnames(res)<-c("nom.var","label","n","perc.missing",names(x$descriptius))
        if (!is.null(x$valors.rars)){
          aux=x$valors.rars
          valors.rars<-paste(paste(aux[,1]," (",aux[,2],")",sep=""),collapse="; ")
          res<-cbind(res,valors.rars)
          colnames(res)<-c("nom.var","label","n","perc.missing",names(x$descriptius),"rare.values")
        }
      }
      return(res)
    }
  }


  ############# FI FUNCIONS PR?VIES #################


  inter=FALSE

  if (is.null(file)){  ## selecci? del fitxer de forma interactiva.
    inter=TRUE
	 if (!data.intern){ file<-choose.files("*.sav",caption="Selecciona la BD de SPSS",multi=FALSE)
	 } else{
    dades<-get(select.list(ls(envir=.GlobalEnv),title="Selecciona el data.frame"),envir=.GlobalEnv)
    if (!is.data.frame(dades)) stop("No s'ha escollit un data.frame")
   }
  } else{
    if (substr(file,nchar(file)-3,nchar(file))!=".sav"){
      data.intern=TRUE
      dades<-get(file,envir=.GlobalEnv)
      if (!is.data.frame(dades)) stop("No s'ha escollit un data.frame")
    } else data.intern=FALSE
  }


  if (!data.intern) dades<-read.spss4(file) # elimina els missing values definits per l'usuari a SPSS.

  # llista de etiquetes de valor
  value.labels<-lapply(dades,function(x) attr(x,"value.labels"))


  ## arreglem els formats.
  dades<-arregla.formats(dades)


	noms<-names(dades)
	vari.labels<-unlist(lapply(dades,function(x){res<-attr(x,"vari.label"); if (is.null(res)) return("") else return(res)}))
  vari.labels<-cbind(noms,vari.labels)
	row.names(vari.labels)<-NULL
  formats<-unlist(lapply(dades,function(x) attr(x,"format.SPSS")))


  ## variables que no farem el control de qualitat
  if (inter){
    aux<-paste(vari.labels[,1],vari.labels[,2],sep=": ")
    desc=select.list(aux,multiple=TRUE,title="Descartades?")
    desc<-noms[aux%in%desc]
    if (length(desc)==0) desc=NULL
  }

	if (is.null(desc)) mirar=1:ncol(dades)
	if (!is.null(desc)) mirar=which(!noms%in%desc)

  taula<-list()

  n=nrow(dades)

  ## INICIALITZA LES TAULES AMB LES 3 COSES COMUNES ##
  for (i in mirar){
    taula[[i]]<-list()
    taula[[i]]$nom.var=noms[i]
    taula[[i]]$label=vari.labels[i,2]
    taula[[i]]$n=n
  }



  ##### BUCLE PER A TOTES LES VARIABLES NO DESCARTADES (DESC) #####

	for (i in mirar){


	print(i)
	print(names(dades)[i])


 		var<-dades[,i]


		## IDENTIFICACI? DE LA CATEGORIA DE LA VARIABLE
		# categoria = 1: num?rica
		# categoria = 2: cadena
		# categoria = 3: data.

		categoria=NULL
		if (is.null(categoria) && length(grep("DATE",formats[i]))>0) categoria=3
    if (is.null(categoria) && length(grep("A",formats[i]))>0){
      var=as.character(var)
      categoria=2
    }
    if (is.null(categoria) && length(grep("F",formats[i]))>0) categoria=1

    taula[[i]]$categoria=switch(categoria,"numeric","string","date")


		## PERCENTATGE DE MISSINGS ##

    n.missing=sum(is.na(var))
		perc.missing<-100*n.missing/n
		perc.missing<-format(round(perc.missing,digits),trim=TRUE,nsmall=digits)
    taula[[i]]$perc.missing<-perc.missing


    if (n.missing<n){

      t<-table(var)


      ####### VARIABLES NUM?RIQUES #######

      if (categoria == 1){

        if (length(t)>max.valors){

          valors.eliminats=NULL
          fer.taula.freq=FALSE
          # VALORS ETIQUETATS #
          if (!is.null(attr(var,"value.labels"))){
            lab.values<-labels(var)
            lab.values<-lab.values[!is.na(lab.values[,2]),]
            valors.eliminats<-c(valors.eliminats,as.integer(as.character(lab.values[,1]))) # servir? per a fer descriptius (eliminarem els valors etiquetats).
            # si gaireb? tots els valors estan etiquetats, encara que n'hi hagin m?s de max.valors fa una taula de freq??ncies.
            aux<-as.integer(names(t))%in%unique(valors.eliminats)
            fer.taula.freq<-sum(aux)/length(t)>0.8
            if (!fer.taula.freq)taula[[i]]$valors.etiquetats=lab.values
          }

          if (!fer.taula.freq){
            ## VALORS RARS ##
            var<-var[!var%in%valors.eliminats]    # si valors.eliminats es NULL els selecciona a tots.
            valors.rars<-rare.values(var)
            if (!is.null(valors.rars)){
              valors.eliminats<-c(valors.eliminats,as.integer(as.character(valors.rars[,1])))
              taula[[i]]$valors.rars<-valors.rars
            }

            ## DESCRIPTIUS ELIMINANT ELS VALORS RARS
            var<-var[!var%in%valors.eliminats]
            descriptius<-quantile(var,prob=c(0,0.05,0.25,0.5,0.75,0.95,1),type=6,na.rm=TRUE)
            taula[[i]]$descriptius<-format(round(descriptius,digits),trim=TRUE,nsmall=digits)
          }
        }

        if (length(t)<=max.valors || fer.taula.freq){

          ## TAULA DE FREQ??NCIES AMB VALORS I VALUE.LABELS
          aux<-taula[[i]]$taula.freq<-labels(var)
          if(sum(aux[,1]%in%aux[,2])==nrow(aux)) taula[[i]]$taula.freq=taula[[i]]$taula.freq[,-2]  # si passa aix? ?s que no hi havia etiquetes (eliminem la columna de labels)
          perc<-100*aux[,3]/(n-n.missing)
          perc<-format(round(perc,digits),trim=TRUE,nsmall=digits)
          taula[[i]]$taula.freq$perc=perc

        }

      }


      ########### VARIABLES CADENA #############

      if (categoria == 2){  # o fa una taula de freq?encies o no fa res.
        if (length(t)<=max.factor){
          aux<-taula[[i]]$taula.freq<-data.frame(list(values=names(t),freq=as.integer(t)))
          perc<-100*aux[,2]/(n-n.missing)
          perc<-format(round(perc,digits),trim=TRUE,nsmall=digits)
          taula[[i]]$taula.freq$perc=perc
        }else taula[[i]]$taula.freq<-NULL  # no d?na la taula de freq??ncies per haver-ho massa valors diferents (max.factor).
      }


      ############ VARIABLES DATA #############

      if (categoria == 3){

        if (length(t)>max.valors){ # fa descriptius

          valors.eliminats=NULL
          # VALORS ETIQUETATS #
          if (!is.null(attr(var,"value.labels"))){
            lab.values<-labels(var)
            lab.values<-lab.values[!is.na(lab.values[,2]),]
            valors.eliminats<-c(valors.eliminats,as.integer(as.character(lab.values[,1]))) # servir? per a fer descriptius (eliminarem els valors etiquetats).
            taula[[i]]$valors.etiquetats=lab.values
            taula[[i]]$valors.etiquetats[,1]=chron(as.integer(as.character(taula[[i]]$valors.etiquetats[,1])),out.format=c(dates="day-mon-year"))
          }

          ## VALORS RARS ##
          var<-var[!var%in%valors.eliminats]
          valors.rars<-rare.values(var)
          if (!is.null(valors.rars)){
            valors.eliminats<-c(valors.eliminats,as.integer(as.character(valors.rars[,1])))
            valors.rars[,1]<-sapply(chron(as.integer(as.character(valors.rars[,1])),format=c(dates="day-mon-year")),format.data)
            taula[[i]]$valors.rars<-valors.rars
          }

          ## DESCRIPTIUS ELIMINANT ELS VALORS RARS I ELS VALORS ETIQUETATS.
          var<-var[!var%in%valors.eliminats]
          descriptius<-quantile(var,prob=c(0,0.05,0.25,0.5,0.75,0.95,1),type=6,na.rm=TRUE)
          taula[[i]]$descriptius<-sapply(descriptius,format.data)

        } else{   # fa una taula de freq??ncies
          # values=as.integer(names(t))
          values <- names(t)
          values<-sapply(chron(values,format=c(dates="day-mon-year")),format.data)
          aux<-taula[[i]]$taula.freq<-data.frame(list(values=values,freq=as.integer(t)))
          perc<-100*aux[,2]/(n-n.missing)
          perc<-format(round(perc,digits),trim=TRUE,nsmall=digits)
          taula[[i]]$taula.freq$perc<-perc

        }

      }

    } else cat("\n\nla variable",noms[i],"est? buida\n\n")

  }

  taula.res<-lapply(taula,table.built)


  titol1<-c("variable","label","N","%miss","min","5%","25%","50%","75%","95%","max","rare values")
  titol2<-c("variable","label","N","%miss","value","value.label","n","perc")
  titol3<-c("variable","label","N","%miss","value","n","perc")
  titol4<-c("variable","label","N","%miss")

  # escriu i obre la taula en excell.
  write(file,file.res)
  write(" ",file.res)
  write.table(rbind(titol1),file.res,sep="\t",append=TRUE,col.names=FALSE,row.names=FALSE)
  write.table(rbind(titol2),file.res,sep="\t",append=TRUE,col.names=FALSE,row.names=FALSE)
  write.table(rbind(titol3),file.res,sep="\t",append=TRUE,col.names=FALSE,row.names=FALSE)
  write.table(rbind(titol4),file.res,sep="\t",append=TRUE,col.names=FALSE,row.names=FALSE)
  for (i in 1:length(taula.res)){
    write.table(taula.res[[i]],file.res,append=TRUE,col.names=FALSE,row.names=FALSE,sep="\t",quote=FALSE)
    write(" ",file.res,append=TRUE)
  }
  shell.exec(file.res)


  ## escriu la taula en latex.
  titol1<-paste(paste(titol1,collapse=" & "),"\\\\")
  titol2<-paste(paste(titol2,collapse=" & "),"\\\\")
  titol3<-paste(paste(titol3,collapse=" & "),"\\\\")
  titol4<-paste(paste(titol4,collapse=" & "),"\\\\")
  file.res<-"prova01.tex"
  write("\n",file.res)
  for (i in 2:length(taula.res))
    {
    if (ncol(taula.res[[i]])==12) write(titol1,file.res,append=TRUE)
    if (ncol(taula.res[[i]])==8) write(titol2,file.res,append=TRUE)
    if (ncol(taula.res[[i]])==7) write(titol3,file.res,append=TRUE)
    if (ncol(taula.res[[i]])==4) write(titol4,file.res,append=TRUE)
    write("\\hline",file.res,append=TRUE)
    currentvar<-paste(apply(as.matrix(taula.res[[i]]),1,paste,collapse=" & "),"\\\\")
    write(currentvar,file.res,append=TRUE)
    }
    file.show(file.res)


  return(list(taula=taula,taula.res=taula.res))

}















