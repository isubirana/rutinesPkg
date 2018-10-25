#'carrega.llibreria
#'fa un library i un install.packages (si cal)
#'@param llibreria: vector de character amb el nom de les llibreries a descarregar.


#'@export
carrega.llibreria<-function(llibreria){

# path.llibreries: carpeta on estan les llibreries en .zip



  if (sum(paste("package:",llibreria,sep="")%in%search())<length(llibreria)){ # si no no cal fer res: ja estan totes carregades

    llibreries.disponibles<-.packages(all.available = TRUE)
    for (i in 1:length(llibreria)){
      if (!llibreria[i]%in%llibreries.disponibles){
        install.packages(llibreria[i], contriburl = contrib.url(getOption("repos"),getOption("pkgType")))
        llib.disp.antic<-llibreries.disponibles
        llibreries.disponibles<-.packages(all.available = TRUE)
        if (length(llibreries.disponibles)==length(llib.disp.antic)){
          winDialog("ok",message="No pot descarregar la llibreria d'internet, l'hauras d'anar a buscar a un local file!!!")
          llibr.ok=FALSE
          while(!llibr.ok){
            llibreria.file<-choose.files(caption="On esta la llibreria",llibreria[i],"?",multi=FALSE)
            if (!length(grep(llibreria[i],basename(llibreria.file)))){
              llibr.ok=winDialog("yesno",paste("Has seleccionat la llibreria",basename(llibreria.file),"enlloc de",llibreria[i],".Estas d'acord?"))=="YES"
            } else llibr.ok=TRUE
          }
          cat("\n_______","Downloading local package ",basename(llibreria.file)," from ",dirname(llibreria.file),"_____\n")
          install.packages(llibreria.file,repos=NULL,destdir=.Library)
        }
      }
      if (!paste("package:",llibreria[i],sep="")%in%search()){
        eval(parse(text=paste("require(",llibreria[i],")",sep="")))
      }
    }

  }

}
