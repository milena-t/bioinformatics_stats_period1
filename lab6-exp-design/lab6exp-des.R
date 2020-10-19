#lab6 experimental design 
setwd('~/bioinformatics/period1_intro_to_stats/lab6-exp-design/')
if (!requireNamespace("BiocManager", quietly = TRUE))
    install.packages("BiocManager")
BiocManager::install()
BiocManager::install(c("edgeR", "baySeq"))
library(baySeq)
library(edgeR)

load("mobData.RData")
head(mobData)
mobDataGroups<-c("MM","MM","WM","WM","WW","WW")
d<-DGEList(counts=mobData,group=factor(mobDataGroups))

d.full<-d# Give the object new name, to keep it for answering the question.
head(d$counts)
head(cpm(d))
apply(d$counts,2,sum)
keep<-rowSums(cpm(d)>100)>=2
  #with 1000: filter [1] 24138 48318 34379 27008 52622 54823
  #with 100: filter [1]  7308 15067  9697  8278 15776 16444
  #with 1: filter [1] 10 36  8 39 10 14
d<-d[keep,]
d$samples$lib.size<-colSums(d$counts)
d$samples$lib.size
d.full$samples$lib.size - d$samples$lib.size

d<-calcNormFactors(d)
plotMDS(d,method="bcv",col=as.numeric(d$samples$group))
legend("bottomleft",as.character(unique(d$samples$group)),col=1:3,pch=20)













