#'crea.haplo
#'Crea haplotips a partir de SNPs
#'@param dades que inclouen els SNPs
#'@param locus.labels noms dels SNPs
#'@param allel de l'haplotip

#'@export
crea.haplo<-function(data,locus.labels,haplo.allele){
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
		num_haplo<-which(hh==haplo.allele)
		num_copies<-apply(cbind(em$hap1code,em$hap2code),1,function(x) sum(x==num_haplo))
		ans<-tapply(num_copies*em$post,em$indx.subj,sum)
    attr(ans,"em")<-em
		ans
}
