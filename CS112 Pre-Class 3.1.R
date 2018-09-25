#install.packages("arm")

#loading
setwd("~/Downloads")
yuhao_train <- read.csv("train_data_yuhao.csv")
yuhao_test <- read.csv("test_data_yuhao.csv")

#mydata
height_vector <- c(rnorm(200,1.70,0.07))
weight_vector <- c(rnorm(200,65,10))
noise_vector <- c(runif(200,-0.3,0.3))
meat_vector <- 0.35*height_vector + 0.15*weight_vector^(1/2) + noise_vector
juan_data <- data.frame("Height" = height_vector, "Weight" = weight_vector, "Meat" = meat_vector) 

juan_train_indexes <- sample(c(1:200), 100)

juan_train <- juan_data[juan_train_indexes,]
juan_test <- juan_data[juan_train_indexes,]

fit1.0yuhao <- lm(yuhao_train[,3]~yuhao_train[,2]+yuhao_train[,1], span = 1)
fit0.5yuhao <- lm(yuhao_train[,3]~yuhao_train[,2]+yuhao_train[,1], span = 0.5)
fit0.2yuhao <- lm(yuhao_train[,3]~yuhao_train[,2]+yuhao_train[,1], span = 0.2)

rmse1.0_train <- sqrt(mean((yuhao_train[,3] - predict(fit1.0yuhao))^2))
rmse0.5_train <- sqrt(mean((yuhao_train[,3] - predict(fit0.5yuhao))^2))
rmse0.2_train <- sqrt(mean((yuhao_train[,3] - predict(fit0.2yuhao))^2))

rmse1.0_test <- sqrt( mean( (yuhao_test[,3] - predict(fit1.0yuhao))^2 ) ) 
rmse0.5_test <- sqrt( mean( (yuhao_test[,3] - predict(fit0.5yuhao))^2 ) )  
rmse0.2_test <- sqrt( mean( (yuhao_test[,3] - predict(fit0.2yuhao))^2 ) )

ratio1.0 <- rmse1.0_train/rmse1.0_test
ratio0.5 <- rmse0.5_train/rmse0.5_test
ratio0.2 <- rmse0.2_train/rmse0.2_test

ratio1.0
ratio0.5
ratio0.2