#lab 4

setwd('~/MEGAsync/Bioinformatik MSc/Semester 1/intro to programming/statistics/lab4/')
#read.delim('AmericanCrime.txt') -> crime

dat <- read.table('AmericanCrime.txt', header = T)
#extract several data sets and save them as vectors
v.crime <- log10(dat[,2])
c.rate <- dat[,1]
PC.crime <- prcomp(cbind(v.crime, c.rate), scale = T) #scale true means that the two variables are weighted equally
summary(PC.crime)
PC.Education <- prcomp(dat[,4:5], scale = T)
summary(PC.Education)
#how much variance is explained by each PC (eigenvalues) and how they relate to the original variables (eigenvectors)

#extract the scores for the forst, most important, PC
pc.crime.scores <- PC.crime$x[,1]
pc.education.scores <- PC.Education$x[,1]
mod <- lm(pc.crime.scores ~ dat$police.funding + pc.education.scores)
summary(mod)
#now the police funding is a lot more significant than it was in the model with just the crime rate vs. police funding 
#I assume that the police funding is way more correlated to the crime rate than the education?
#when you compare it to the model at the end when the violent crime ~ crime+police.funding+hs+college):
# v.crime strongly correlated to normal crime (makes sense) but not at all to police funding, 
  # perc highschool is stronger correlated than the PC of hs and college


#ex.2
data('iris')
levels(iris$Species)
iris.setosa <- iris[iris$Species == 'setosa', 1:4]
iris.versicolor <- iris[iris$Species == 'versicolor', 1:4]
iris.virginica <- iris[iris$Species == 'virginica', 1:4]
par(mfrow = c(2,2))
for (i in 1:4) { 
  hist(iris.setosa[,i],xlab=NULL, main=names(iris.setosa)[i]) 
}
#petal with is not normally distributed at all. you could transform it but this is not the topic of this exercise so we will ignore it
for (i in 1:4) { 
  hist(iris.virginica[,i],xlab=NULL, main=names(iris.virginica)[i]) 
}
for (i in 1:4) { 
  hist(iris.versicolor[,i],xlab=NULL, main=names(iris.versicolor)[i]) 
}
par(mfrow = c(1,1))
#perform pca
ir.pca <- prcomp(iris[, 1:4] , scale = TRUE)
ir.pca
summary(ir.pca)
install.packages("ggfortify")
library(ggfortify)
autoplot(ir.pca, data = iris, colour = 'Species',
         loadings = TRUE, loadings.colour = 'blue',
         loadings.label = TRUE, loadings.label.size = 3)


#ex3
dat <- read.table('tadpole_food.txt', header = T)
head(dat)
str(dat)
tadpole.pca <- prcomp(dat[,3:9], scale = T)
summary(tadpole.pca)

autoplot(tadpole.pca, data = dat, colour = 'Food',
         loadings = TRUE, loadings.colour = 'blue',
         loadings.label = TRUE, loadings.label.size = 3)


#ex4
dat$Pop <- factor(dat$Pop)
mod.Man <- manova(cbind(Muscle_depth, Body_depth, Tail_depth) ~ Food, data = dat)
summary(mod.Man)

mod.MAN2 <- manova(cbind(Muscle_depth,Body_depth,Tail_depth) ~ Food*Pop, data=dat )
summary(mod.MAN2)

mod.MAN3 <- manova(cbind(Muscle_depth,Body_depth,Tail_depth) ~ Food + Error(Pop + Pop:Food), data=dat )
summary(mod.MAN3)

autoplot(tadpole.pca, data = dat, colour = 'Pop',
         loadings = TRUE, loadings.colour = 'blue',
         loadings.label = TRUE, loadings.label.size = 3)






