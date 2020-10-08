#lab 5
setwd('~/MEGAsync/Bioinformatik MSc/Semester 1/intro to programming/statistics/lab5/')
pkgs <- c("factoextra", "NbClust", "cluster", "fpc", "gridExtra", 'car', 'ggplot2')
install.packages(pkgs)
library(factoextra)
library(NbClust)
library(cluster)
library(fpc)
library(gridExtra)
library(car)
library(ggplot2)

#(1) k means clustering
#   how many clusters does the dataset likely consist of

tadpole <- read.table("tadpole_food.txt", header=TRUE)
dat <- scale(tadpole[3:9])
#cluster the data differently with 2, 3, 4, 5
k2 <- kmeans(dat, centers = 2, nstart = 25)
k3 <- kmeans(dat, centers = 3, nstart = 25)
k4 <- kmeans(dat, centers = 4, nstart = 25)
k5 <- kmeans(dat, centers = 5, nstart = 25)
#plot the results
p1 <- fviz_cluster(k2, genom = 'point', data = dat, labelsize = 0) + ggtitle('k=2')
p2 <- fviz_cluster(k3, genom = 'point', data = dat, labelsize = 0) + ggtitle('k=3')
p3 <- fviz_cluster(k4, genom = 'point', data = dat, labelsize = 0) + ggtitle('k=4')
p4 <- fviz_cluster(k5, genom = 'point', data = dat, labelsize = 0) + ggtitle('k=5')
grid.arrange(p1, p2, p3, p4, nrow = 2)
#dimension 2 explains a lot less than dimension 1
#evaluate which number of clusters fits the data best (shoud in reality probably be done before)
fviz_nbclust(dat, kmeans, method = "silhouette") + labs(subtitle ="Silhouette method")
#the 2 clusters is likely the best option                    

#hierarchical clustering
#number of clusters does not need to be predefined, 'tree' cluster
#calculate a dissimilarity matrix
d <- dist(dat, method = 'euclidian')
hc1 <- hclust(d, method = 'complete')
par(mfrow=c(2,2))
plot(hc1, cex = 0.6, hang = -1)
rect.hclust(hc1, k = 2, border = 2:5)
plot(hc1, cex = 0.6, hang = -1)
rect.hclust(hc1, k = 3, border = 2:5)
plot(hc1, cex = 0.6, hang = -1)
rect.hclust(hc1, k = 4, border = 2:5)
plot(hc1, cex = 0.6, hang = -1)
rect.hclust(hc1, k = 5, border = 2:5)
par(mfrow=c(1,1)) 
#2-5 clusters as with kmeans
#probably 2 or three clusters

#(3) classical multidimensional scaling (mds) also known as principal coordinate analysis
library(ggfortify)
dat <- read.table("SNPdata2.txt", header=TRUE)
dat2 <- as.matrix(dat[, 3:1003])
d <- dist(dat2, method="euclidean")
#run multidimenstional scaling, reduce the number to k= 2 dimensions
fit <- cmdscale(d,k=2)
fit 
cols = c('red', 'blue', 'black', 'steelblue', "yellow", "black", "magenta", "green", "purple")
dat$Pop = as.factor(dat$Pop)
plot(fit, xlab = 'Coordinate 1', ylab = 'Coordinate 2', main = 'Classical_MDS', type = 'n')
points(fit, col = dat$Pop, cex = .7, pch = 19)
legend('bottomleft', col=cols, legend=levels(dat$Pop), pch = 16, cex = 0.7)
#now run the PCA on the SNP data (scale false uses the covariance matrix on unstandardized data)
SNP_PCA <- prcomp(dat2, scale=F)
#plot without the loadings (PC arrows)
autoplot(SNP_PCA, data = dat, colour = 'Pop')


#(4) Redundancy analysis (RDA)
install.packages("vegan")
library(vegan)
install.packages("psych")
library(psych)
env = read.table("environmental.txt", header=TRUE)
EnvVars = cbind(env$pH, env$ForestCanopy, env$Meantemp)
colnames(EnvVars) <- c("pH", "ForestCanopy","Meantemp")
EnvVars <- data.frame(scale(EnvVars, scale=TRUE, center=TRUE))
space <- cbind(env$Lat,env$Long)
colnames(space) <- c("Lat", "Long")
space <- data.frame(scale(space, scale=TRUE, center=TRUE))
corr.test(EnvVars)
#maybe pH ~ ForestCanopy is very correlated? |r| = 0.67 but not enough to exclude
dat3 <- scale(dat2, scale = T)
#first redundancy analysis
rda_env <- rda(dat3 ~ pH + ForestCanopy + Meantemp, scale = F, data = EnvVars)
rda_env
#constrained: envireonmental variables conditioned on the response variables
#unconstrained: not explained by response variables
sign.test <- anova.cca(rda_env)
sign.test
RsquareAdj(rda_env)

#ordination
Pop <- env$Pop
bg <- c("#ff7f00","#1f78b4","#ffff33","#a6cee3","#33a02c","#e31a1c", "#00ff7f", "#ff00ff", "#006262")
plot(rda_env,xlim = c(-3, 3), ylim = c(-2, 2), type="n", scaling=3)
points(rda_env, display="species", pch=20, cex=0.7, col="gray32", scaling=3)
# the SNPs
points(rda_env, display="sites", pch=21, cex=1.3, col="gray32", scaling=3, bg=bg[Pop]) # the Individuals
text(rda_env, scaling=3, display="bp", col="#0868ac", cex=1,choices =c(1,2), labels = c("pH","Canopy","Temperature")) # the predictors
legend("bottomleft", legend=levels(Pop), bty="n", col="gray32", pch=21, cex=1, pt.bg=bg)
#snp 2
rda_test = rda(dat3~space$Lat+space$Long, scale=FALSE)
sign.test2 = anova.cca(rda_test)
sign.test2
RsquareAdj(rda_test)











