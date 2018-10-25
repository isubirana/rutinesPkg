#'crea.haplo2
#'Crea haplotips a partir de SNPs tenint en compte els pesos
#'@param dades que inclouen els SNPs
#'@param locus.labels noms dels SNPs
#'@param allel de l'haplotip
#'@param pesos pesos dels SNPs

#'@export
crea.haplo2<-
function(data,locus.labels,haplo.allele,pesos){

    polis<-data[,locus.labels]
    N<-length(locus.labels)
    geno<-matrix(NA,nrow(polis),(2*N))
    for (i in 1:N){  # bucle per a cada polimorfisme
      ll<-names(table(polis[,i]))
      a1<-substr(ll[2],1,1)
      a2<-substr(ll[2],2,2)
      g1<-paste(a1,a1,sep="")
      g2<-paste(a1,a2,sep="")
      g3<-paste(a2,a2,sep="")
      geno[polis[,i]==g1,(2*i-1)]<-a1
      geno[polis[,i]==g2,(2*i-1)]<-a1
      geno[polis[,i]==g3,(2*i-1)]<-a2
      geno[polis[,i]==g1,(2*i)]<-a1
      geno[polis[,i]==g2,(2*i)]<-a2
      geno[polis[,i]==g3,(2*i)]<-a2
    }
    carrega.llibreria("haplo.stats")
    library(haplo.stats)
    geno <- setupGeno(geno, miss.val=NA)
    for (i in 1:N){
      al<-attr(geno, "unique.alleles")[[i]]
      geno[,2*i-1] <- ifelse(geno[,(2*i-1)]==1, al[1], al[2])
      geno[,2*i] <- ifelse(geno[,2*i]==1, al[1], al[2])
    }
    em<-haplo.em(geno, locus.label=locus.labels, miss.val=c(0, NA))
    hh<-em$haplotype
    hh<-apply(hh,1,paste,collapse="")
    num_haplo<-sapply(haplo.allele,function(x) which(hh==x))
    num_copies<-apply(cbind(em$hap1code,em$hap2code),1,function(x){
      ss1<-ss2<-0
      for (i in 1:length(num_haplo)){
        ss1<-ss1+pesos[i]*(x[1]==num_haplo[i])+pesos[i]*(x[1]==num_haplo[i])
        ss2<-ss2+pesos[i]*(x[2]==num_haplo[i])+pesos[i]*(x[2]==num_haplo[i])
      }
      ss1+ss2
    })
    ans<-tapply(num_copies*em$post,em$indx.subj,sum)
    attr(ans,"em")<-em
    ans

}
