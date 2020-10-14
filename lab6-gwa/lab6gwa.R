# R lab Arild GWAs
setwd('~/bioinformatics/period1_intro_to_stats/lab6-gwa/')
#for comments see text file
fmsURL <- "http://www.stat-gen.org/book.e1/data/FMS_data.txt"
fms <- read.delim(file=fmsURL, header=T, sep="\t")
head(fms)
attach(fms)
NamesAkt1Snps <- names(fms)[substr(names(fms),1,4)=="akt1"] #Note use of substring function to pull out snp names with akt1 included
length(NamesAkt1Snps) #24 SNPs within the akt1 gene
FMSgeno <- fms[,is.element(names(fms),NamesAkt1Snps)] 
FMSgenoNum <- data.matrix(FMSgeno)
FMSgenoNum[is.na(FMSgenoNum)] <- 4
dim(FMSgenoNum)
DistFmsGeno <- as.matrix(dist(FMSgenoNum)) 
DistFmsGeno[1:5,1:5]
#multidimensional scaling aka principal component analysis
plot(cmdscale(DistFmsGeno),xlab="C1",ylab="C2")
abline(v=0,lty=2)
abline(h=4,lty=2)
PCFMS <- prcomp(FMSgenoNum)
plot(PCFMS$"x"[,1],PCFMS$"x"[,2],xlab="PC1",ylab="PC2")


vircoURL <- "http://www.stat-gen.org/book.e1/data/Virco_data.csv"
virco <- read.csv(file=vircoURL, header=T, sep=",")
attach(virco)
PrMut <- virco[,23:121]!="-" & virco[,23:121]!="."
NObs <- dim(virco)[1]
PrMutSub <-data.frame(PrMut[ , apply(PrMut,2,sum) > NObs*.05])
Trait <- IDV.Fold - NFV.Fold

TtestP <- function(Geno){
  return(t.test(Trait[Geno==1], Trait[Geno==0], na.rm=T)$"p.value")
}
Pvec <- apply(PrMutSub, 2, TtestP)
sort(Pvec)
length(Pvec)
names(PrMutSub)[Pvec < 0.05]
PvecAdj <- p.adjust(Pvec, method = 'bonferroni')

sort(PvecAdj)
names(PrMutSub)[PvecAdj < 0.05]

PvecBH <- p.adjust(Pvec, method="BH") 
names(PrMutSub)[PvecBH < 0.05]





