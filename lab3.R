#lab3

shared <- rnorm(1000, 0, 20) #atandard deviation = 10, vari = 10^2
private.A <- rnorm(1000, 0, 20)
private.B <- rnorm(1000, 0, 20)
env.A <- rnorm(1000, 0, 10)
env.B <- rnorm(1000, 0, 10)
#for variation A add all single variations (shared, private, env.) because variance is additive -> variance is 900 (for both A and B)
A <- shared +private.A +env.A
B <- shared + private.B +env.B

plot(A, B)
cov(A, B)

#calculate the correlation coefficient (rho)
rho = cov(A, B)/(sqrt(var(A))*sqrt(var(B)))
cor(A, B)
cor.test(A,B)

#2
setwd('~/MEGAsync/Bioinformatik MSc/Semester 1/intro to programming/statistics/lab3/')
read.delim('AmericanCrime.txt') -> crime
summary(crime)
simple.1a <- lm(violent.crime ~ police.funding, data = crime)
summary(simple.1a)
par(mfrow=c(2,2)) 
plot(simple.1a)
par(mfrow=c(1,1)) 
hist(simple.1a$residuals)
#plot the data to look at the outlier
plot(c(1:length(crime$violent.crime)), crime$violent.crime)
plot(c(1:length(crime$police.funding)), crime$police.funding)
plot(crime$violent.crime, crime$police.funding)

plot(log10(violent.crime)~police.funding, data = crime)
simple.1b <- lm(log10(violent.crime) ~ police.funding, data = crime)
plot(simple.1b)
summary(simple.1b)
#r squared: how much of x is explained by y? in percent

number.of.oranges <-rnorm(length(crime$violent.crime), 100, 10)
number.of.apples <- rnorm(length(crime$violent.crime), 100, 10)
mod2a <-lm(log10(violent.crime)~police.funding, data = crime)
mod2b <- lm(log10(violent.crime)~ police.funding+number.of.oranges+number.of.apples, data = crime)
summary(mod2a)
summary(mod2b)
library(car)
cmod2 <- anova(mod2a, mod2b)

summary(cmod2)

#robust regression
library(MASS)
simple.3 <- rlm(violent.crime~police.funding, data = crime)
plot(simple.3)
summary(simple.3)
Anova(simple.3)

#exercise 3
ex.31 <- lm(log10(violent.crime) ~ crime.rate+police.funding+perc.highschool+perc.college, data = crime)
summary(ex.31)
ex.32 <- lm(log10(violent.crime) ~ crime.rate+police.funding+perc.highschool+perc.college, data = crime)
summary(ex.32)

uni.1a <- lm(log10(violent.crime) ~ police.funding, data = crime)
summary(uni.1a)
uni.1b <- lm(log10(violent.crime) ~ perc.highschool, data = crime)
summary(uni.1b)
multi.1 <- lm(log10(violent.crime) ~ police.funding + perc.highschool, data = crime)
summary(multi.1)

library(scatterplot3d)
scatterplot3d(crime$police.funding, crime$perc.highschool, crime$violent.crime, pch=20)

multi.2 <- lm(log10(violent.crime) ~ crime.rate + police.funding + perc.highschool + perc.college, data = crime)
summary(multi.2)

multi.3 <- lm(log(violent.crime) ~ police.funding + perc.highschool + perc.college, data = crime)
summary(multi.3)

install.packages('RcmdrMisc')
library(RcmdrMisc)
partial.cor(crime)
