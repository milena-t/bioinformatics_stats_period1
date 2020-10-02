#lab 2 scientific computing - statistics
setwd('~/MEGAsync/Bioinformatik MSc/Semester 1/intro to programming/statistics/lab2/')
read.delim("diet.txt") -> diet
head(diet)
tail(diet)
str(diet)
diet$weight.loss <- diet$pre.weight -diet$weight6weeks
boxplot(weight.loss ~ Diet, data = diet, col = 'light gray', ylab = 'Weight loss (kg)', xlab = 'Diet type')
abline(h = 0, col = 'blue')
mod1a <- aov(weight.loss ~ Diet, data = diet)
mod1b <- lm(weight.loss ~ Diet, data = diet)
plot(mod1a)
plot(mod1b)
hist(resid(mod1a))
par(mfrow = c(2,2))

par(mfrow = c(1,1))

summary(mod1a)
summary(mod1b)

library(car)
Anova(mod1a)
Anova(mod1b)

TukeyHSD(mod1a)
plot(TukeyHSD(mod1a))

#two way anova

mod2a <- lm(weight.loss ~ Diet + gender, data = diet)
plot(mod2a)
summary(mod2a)
#output: intercept -> first letter in the alphabet -> females on diet A
#-> males averaged over all diets
mod2b <- lm(weight.loss ~ Diet * gender, data = diet)
plot(mod2b)
summary(mod2b)
#here male weightloss on all diets
#to get the effect of diet c - males:
#summarize the estimates: start always with the intrcept 
# + diet c + gender m + dietC:genderM
library(lattice)
bwplot(weight.loss ~ Diet|gender, data = diet)
library(effects)
plot(allEffects(mod2b))
anova(mod2b)

#generalized linear models

A <- rpois(10000, 1)
B <- rpois(10000, 1.2)
pois.data <- data.frame(c(rep('A', 10000), rep('B', 10000)), c(A, B))
mod.pois <- glm(pois.data[,2]~pois.data[,1], family = 'poisson')
summary(mod.pois)
Anova(mod.pois)

A <- rbinom(10000, 1, 0.2)
B <- rbinom(10000, 1, 0.1)
binom.data <- data.frame(c(rep('A', 10000), rep('B', 10000)), c(A,B))
mod.binom <- glm(binom.data[,2]~binom.data[,1], family = 'binomial')
summary(mod.binom)
Anova(mod.binom)

#linear mixed effects models

read.delim("jimson.txt") -> dat
dat$Type <- factor(dat$Type, levels = c(1,2), labels = c('G', 'N'))
dat$Pot <- as.factor(dat$Pot)

fm1a <- aov(LenWid ~ Type * Pot, data = dat)
summary(fm1a)
fm1b <- aov(LenWid ~ Type + Error(Pot + Type:Pot), data = dat)
summary(fm1b)

dat2 <- subset(dat, (Pot=="16533") | (Pot=="16534"))
fm2a <- aov(LenWid ~ Type + Pot + Type:Pot, data=dat2)
summary(fm2a)

fm2b <- aov(LenWid ~ Type + Error(Pot+ Type:Pot), data=dat2)
summary(fm2b)

bwplot(LenWid ~ Type|Pot, data = dat)
