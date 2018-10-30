install.packages("foreign")
library(foreign)
dataset <- read.dta("/Users/juan/Downloads/turnout.dta")
dataset[1,1]
dataset$agesqrd[1]

lm2 <- glm(turnout ~ white + age + educate + income + agesqrd, family = binomial, data = dataset)
lm2$coefficients

XB <- sum(coef(lm2)*c(1, 0, 38, 12, 4, 0.01*38^2))
p <-  exp(XB) / (1 + exp(XB))
p

XB2 <- sum(coef(lm2)*c(1, 0, 38, 16, 4, 0.01*38^2))
p2 <-  exp(XB2) / (1 + exp(XB2))
p2

XB3 <- sum(coef(lm2)*c(1, mean(dataset$white), 38, 16, mean(dataset$income), 0.01*38^2))
p3 <-  exp(XB3) / (1 + exp(XB3))
p3

library(arm)

Xs <- c(1, mean(dataset$white), 38, 16, mean(dataset$income), 0.01*38^2)

sims <- sim(lm2, n.sim=1000)
sims.coef <- coef(sims)

storage <- c()

for (i in 1:1000) {
  storage[i] <- exp(sum(sims.coef[i,]*Xs))/(1 + exp(sum(sims.coef[i,]*Xs)))
}

quantile(storage, probs = c(0.005, 0.995))

storagedf_12 <- dfstorage <- matrix(NA, nrow = 1000, ncol = 78) # for ages 18:95, thus 78

for(age in c(18:95)) {
  for(i in 1:1000) {
    beta <-  sum(sims.coef[i,]*(c(1, mean(dataset$white), age, 16, mean(dataset$income), 0.01*age^2)))
    storagedf_12[i, age - 17] <- exp(beta)/(1 + exp(beta))
  }
}

conf.intervals <- apply(storagedf_12, 2, quantile, probs = c(0.005, 0.995))

conf.intervals

#FROM HERE IS WHAT LUCCA SENT ME
conf.intervals <- apply(storagedf_12, 2, quantile, probs = c(0.005, 0.995))
conf.intervals
plot(x = c(1:100), y = c(1:100), type = "n", xlim = c(18,95), ylim = c(0,1))


counter = 1
for(age in 1:length(18:95)) {
  segments(x0 = age,  y0 = conf.intervals[counter], x1 = age, y1 = conf.intervals[counter + 1])
  counter = counter + 2
}
