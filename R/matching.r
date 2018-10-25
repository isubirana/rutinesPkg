#' matching
#' Aparallament casos i controls
#' @param dades: nom de la base de dades en R
#' @param nom.casecon: nom de la variable que distingeix entre casos i controls
#' @param valor.cas: valor identificador de cas en la variable nom.casecon
#' @param nom.id: nom de la variable identificadora (?nica per individu)
#' @param num.controls: n?mero de controls a matchejar per a cada cas
#' @param var.match: vector character de les variables a matchejar
#' @param tol: tolerància per a les variables var.match (amb el mateix ordre)
#' tol=c(0,2)    0, +-2     //  tol=list(c(0,0),c(0,2))     0, [0,2]
#' @param nom.pair: nom de la variable parella
#' @param seed.casos: llavor per als casos
#' @param seed.controls: llavor per als controls
#' @import Hmisc
#' @examples \dontrun{
#' ncases=10
#' ncontrols=ncases*2
#' x<-data.frame(id=1:ncases,edad=trunc(rnorm(ncases,60,10)),sexe=rbinom(ncases,1,0.3),cascon=rep(1,ncases))
#' y<-data.frame(id=1:ncontrols+1000,edad=trunc(rnorm(ncontrols,53,10)),sexe=rbinom(ncontrols,1,0.5),cascon=rep(0,ncontrols))
#' d<-rbind(x,y)
#' # difer?ncia d'edat entre casos i controls de 5 anys i mateix sexe.
#' dm<-matching(dades=d,
#'          nom.casecon="cascon",
#'          valor.cas=1,
#'          nom.id="id",
#'          num.controls=1,
#'          var.match=c("sexe","edad"),
#'          tol=c(0,5),
#'          nom.pair="pair",
#'          seed.casos=321,
#'          seed.controls=678)
#'
#' d<-merge(d,dm,by="id",all.x=TRUE)
#' # controls m?s grans que els casos per? no m?s de 5 anys i mateix sexe
#' dm<-matching(dades=d,
#'          nom.casecon="cascon",
#'          valor.cas=1,
#'          nom.id="id",
#'          num.controls=1,
#'          var.match=c("sexe","edad"),
#'          tol=list(c(0,0),c(0,5)),
#'          nom.pair="pair",
#'          seed.casos=321,
#'          seed.controls=678)
#' d<-merge(d,dm,by="id",all.x=TRUE)
#' }



matching<-function(dades,nom.casecon,valor.cas,nom.id,num.controls,var.match,tol,nom.pair,seed.casos=321,seed.controls=678){

  find.matches<-function (x, y, tol, scale = tol, maxmatch = 10)
  {

      #if (.R.)  rep.int <- rep  ## aquesta instruccio no funciona al ordinador de la JUdith amb la versi? 3.1
      rep.int <- rep ## las substituexo per aquesta
      if (!is.matrix(x)) x <- as.matrix(x)
      n <- nrow(x)
      p <- ncol(x)

      if (!is.list(tol)){
        print("ha entrat aqu?")
        temp<-list()
        for (k in 1:length(tol)) temp[[k]]<-c(-tol[k],tol[k])
        tol<-temp
      }

      if (!is.matrix(y)) y <- as.matrix(y)
      if (p != ncol(y)) stop("number of columns of x and y must match")

      ny <- nrow(y)
      rown <- dimnames(x)[[1]]
      ry <- dimnames(y)[[1]]
      matches <- matrix(if (length(ry)) "" else 0, n, maxmatch, dimnames = list(rown, paste("Match #",1:maxmatch, sep = "")))
      distance <- matrix(NA, n, maxmatch, dimnames = list(rown,paste("Distance #", 1:maxmatch, sep = "")))
      if (length(ry) == 0) ry <- 1:ny
      scale <- unlist(lapply(tol,diff))
      scale <- ifelse(scale == 0, 1, scale)
      mx <- 0
      for (i in 1:n) {
          dif <- y - rep(x[i, ], rep.int(ny, p))
          inside<-NULL
          for (k in 1:p) inside<-cbind(inside,dif[,k]<=tol[[k]][2] & dif[,k]>=tol[[k]][1])
          which <- which(apply(inside,1,all))
          lw <- length(which)
          if (lw) {
              scaled <- dif[which, , drop = FALSE]/rep(scale, rep.int(lw,p))
              dist <- rowSums(scaled^2)
              lw <- min(lw, maxmatch)
              mx <- max(mx, lw)
              d <- order(dist)[1:lw]
              matches[i, 1:lw] <- ry[which[d]]
              distance[i, 1:lw] <- dist[d]
          }
      }
      structure(list(matches = matches[, 1:mx], distance = distance[,1:mx]), class = "find.matches")
  }


  ############  FI FUNCIONS PR?VIES ##############

  CASECON=dades[,nom.casecon]

  casos=dades[CASECON==valor.cas,c(nom.id,var.match)]
  controls=dades[!CASECON==valor.cas,c(nom.id,var.match)]

  match.res<-find.matches(as.matrix(casos[,-1]),as.matrix(controls[,-1]),tol=tol,maxmatch=nrow(casos))

  cont.matched<-matrix(NA,nrow(casos),num.controls)

  match<-match.res$matches

  dist<-match.res$distance

  if(is.null(rownames(match))) rownames(match)<-1:nrow(match)
  if(is.null(rownames(dist))) rownames(dist)<-1:nrow(dist)

  # agafem els casos amb un ordre aleatori.

  set.seed(seed.casos)

  ordre.casos<-sample(rownames(match))

  match2<-match[ordre.casos,]
  dist2<-dist[ordre.casos,]

  id.cas<-row.names(match2)  # ? row.names(dist2)

  set.seed(seed.controls)

  for (i in 1:nrow(casos)){

  	candidats<-match2[i,]
  	candidats<-candidats[!candidats==""]
  	distancies<-dist2[i,1:length(candidats)]

  	if (i>1){ # no agafem el/s candidat/s que hem agafat per als anteriors casos.
  		elim.anteriors<-!candidats%in%cont.matched[1:(i-1),]
  		candidats<-candidats[elim.anteriors]
  		distancies<-distancies[elim.anteriors]
  	}

  	if (length(candidats)<num.controls) print(paste("L'individu ",casos[id.cas[i],nom.id]," no s'ha pogut matchejar",sep=""))

  	if (length(candidats)==num.controls) cont.matched[i,]=candidats

  	if (length(candidats)>num.controls) {	## veure ajuda: estan ordenats segons la dist?ncia de forma creixent
  		## ordenem els candidats de forma aleatoria dins de cada dist2[i,]
 			candidats<-lapply(split(as.character(candidats),distancies),function(x) {if (length(x)==1) return(x); if (length(x)>1) return(sample(x))})
 			candidats<-as.character(unlist(candidats))
  		cont.matched[i,]<-candidats[1:num.controls]
  	}


  }

  aux.rep<-NULL
  for (i in 1:ncol(cont.matched)) aux.rep<-c(aux.rep,cont.matched[,i])

  cat("Aix? ha de sumar zero",sum(table(aux.rep)>1),"\n")  ## sempre ha de donar zero.
  print(paste("S'han pogut matchejar ",sum(!is.na(cont.matched[,1]))," casos d'un total de ",nrow(casos)," casos",sep=" "))


  no.matched.casos<-id.cas[is.na(cont.matched[,1])]
  no.matched.casos<-casos[no.matched.casos,nom.id]

  matched.casos<-id.cas[!is.na(cont.matched[,1])]
  # matched.casos<-casos[matched.casos,nom.id]
  matched.casos<-casos[as.numeric(matched.casos),nom.id]



  matched.controls<-cont.matched[!is.na(cont.matched[,1]),,drop=FALSE]


  if (num.controls==1){
    #matched.controls<-controls[matched.controls,nom.id]
    matched.controls<-controls[as.numeric(matched.controls),nom.id]
  }
  if (num.controls>1){
  	for (i in 1:num.controls){
  		#matched.controls[,i]<-controls[matched.controls[,i],nom.id]
  		matched.controls[,i]<-controls[as.numeric(matched.controls[,i]),nom.id]
  	}
  }

  taula.match<-data.frame(list(parella=1:length(matched.casos),id_cas=matched.casos,id_cont=matched.controls))

  parella<-rep(taula.match$parella,each=1+num.controls)

  id<-NULL
  for (i in 1:nrow(taula.match)){
  	if (num.controls==1) id=c(id,matched.casos[i],matched.controls[i])
  	if (num.controls>1) id=c(id,matched.casos[i],matched.controls[i,])
  }


  cascon<-rep(c(1,rep(0,num.controls)),nrow(taula.match))

  taula.res<-data.frame(list(ID=I(id),CASCON=cascon,PARELLA=parella))

  names(taula.res)[3]<-nom.pair
  names(taula.res)[1]<-nom.id

  return(taula.res[,-2])

}

















