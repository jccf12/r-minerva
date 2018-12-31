x <- runif(1000,-1,1)
y <- 5 + 3 * x + 2*(x >= 0) + rnorm(1000)


d <- x > 0 
reg1 <- lm(y ~ x + d)
summary(reg1)
confint(reg1)
install.packages('rdrubost')

library(rdrobust); rdplot(y,x); summary(rdrobust(y,x))
