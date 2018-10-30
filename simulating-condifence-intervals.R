library(MASS)
library(ggplot2)

#generate the fake data
covvar <- matrix(c(1,.6,.6,.6,1,.6,.6,.6,1),nr = 3)
means <- c(20,25,30)
data <- as.data.frame(mvrnorm(100,means,covvar))
#dichotomize the dependent variable
data$V1 <- ifelse(data$V1 > 20,1,0)

#estimate the logit model
model1 <- glm(V1 ~ V2 + V3, data = data, family = "binomial")
summary(model1)

#get the components necessary for the simulation
beta <- coef(model1)
covvar <- vcov(model1)

#define the values at which the variables are held constant, V3 is allowed to vary with V2 at its mean
x <- cbind(1,mean(data$V2), seq(27,33,0.1))

#simulate parameter values from the multivariate normal distribution, 1000 draws
beta.sim <- mvrnorm(1000, beta,covvar)

#use some matrix algebra to calculate to systematic component for each draw
xb <- x %*% t(beta.sim)

#for each draw get prediced probability
p <- 1/(1 + exp(-xb))

#get the .5 and .95 confidence intervals for each value in the sequence for V3
predicted.prob <- apply(p,1,quantile, probs = c(.05,.5,.95))

#collect the results in a data frame for plotting in ggplot2
dat <- data.frame(x = seq(27,33,0.1), lower = predicted.prob[1,], middle = predicted.prob[2,], upper = predicted.prob[3,])

#plot the results with ggplot2
plot <- ggplot(dat, aes(x = x))
plot <- plot + geom_line(aes(y = lower), linetype = 2)
plot <- plot + geom_line(aes(y = middle), linetype = 1)
plot <- plot + geom_line(aes(y = upper), linetype = 2)
plot <- plot + theme_bw()
plot
