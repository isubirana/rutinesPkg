#'Classification
#'Retorna la freq. de les combinacions de variables i la categoria a la que correspon cada individu
#'Mostra els NA com a 9999  (selec=2 fa que retorni la categoria a cada individu)
#'@param dades base de dades
#'@param vars vector amb els noms de les variables
#'@param selec (default is 1)

#'@export
Classification <- function(dades, vars, selec = 1){


  dades[,vars] <- lapply(dades[,vars], function(x)car::recode(x,"NA=9999"))

  vars.attrib <- lapply(dades[,vars], function(x) attr(x,"value.labels"))
  dat.table <- table(dades[,vars])
  dat.table <- as.data.frame(dat.table)
  for (i in 1:length(vars)){
    attr(dat.table[,i],"value.labels") <-  vars.attrib[[i]]$value.labels
  }

  dat.table$Freq.perc <- round(dat.table$Freq/(sum(dat.table$Freq)),2)

  z <- list()
  z$table.freq <- dat.table

  pos <- function(x,aux){
  tmp <- 999
  for (i in 1:nrow(aux)){
    tmp <- ifelse(all(x==aux[i,])==TRUE, i, tmp)
  }
    return(tmp)
  }

  if (selec==2){
  z$positions <-  apply(dades[,vars], 1,function(x) pos(x,dat.table[1:length(vars)]))
  }

  return(z)
}



