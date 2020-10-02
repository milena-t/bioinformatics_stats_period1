#lab1 statistics (Milena Trabert)
####start assignment 10.09.2020####

males <- rnorm(10, 30, 10)
females <- rnorm(10, 30, 10)
par(mfrow=c(2,1))
hist(males)
hist(females)
t.test(males, females, var.equal = TRUE)
  #p-value = 0.3742 -> H0 will be accepted?
  #alternate hypothesis: the true difference in mean is not equal to 0
#numpeople = 1000
P_value <- 1:(1000)
ttestmf <- function(numpeople){
  for(i in 1:1000){
    males <- rnorm(numpeople, 30, 10)
    females <- rnorm(numpeople, 30, 10)
    P_value[i] <- t.test(males, females, var.equal = TRUE)$p.value
  }
  #hist(P_value, breaks = 100)
  sign.tests <-ifelse(P_value < 0.05, 1,0)
  #sum(sign.tests)
  return(sign.tests)
}
r = ttestmf(1000)
sum(r)
t_tasks <- 1:100
for (i in 1:100){
  t_tasks[i]<-sum(ttestmf(1000))
}


P_value <- 1:(100)
for(i in 1:100){
  males <- rnorm(numpeople, 30, 10)
  females <- rnorm(numpeople, 30, 10)
  P_value[i] <- t.test(males, females, var.equal = TRUE)$p.value
}

hist(p.adjust(P_value, "fdr"))
bio <- rnorm(1000, 0, 1)
Tech <- rnorm(1000, 0, 2)
Total <-bio+Tech
var(bio); var(Tech)
var(Total)
var(bio)+var(Tech)


