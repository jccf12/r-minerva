setwd("~/Downloads")
yuhao_train <- read.csv("train_data_yuhao.csv")
yuhao_test <- read.csv("test_data_yuhao.csv")

height_vector <- c(rnorm(200,1.70,0.07))
weight_vector <- c(rnorm(200,65,10))
noise_vector <- c(runif(200,-0.3,0.3))
meat_vector <- 0.35*height_vector + 0.15*weight_vector^(1/2) + noise_vector
meateaters <- data.frame("Height" = height_vector, "Weight" = weight_vector, "Meat" = meat_vector) 

subset1 <- sample(c(1:200), 100) 

meateaters_train <- meateaters[subset1,]  #this takes subset1 observations and all the columns
meateaters_test <- meateaters[-subset1,] #this takes all EXCEPT subset1 obs, w/ all columns

y_train <- meateaters_train$Meat
x_train <- meateaters_train$Weight
y_test <- meateaters_test$Meat


fit1.0 <- loess(y_train~x_train, span = 1, degree = 1) # 
fit.5 <- loess(y_train~x_train, span = 0.5, degree = 1) # 
fit.2 <- loess(y_train~x_train, span = 0.2, degree = 1) # 

#lines(x, predict(fit1.0, x), lwd = 3, col = "red") # add lines to plot
#lines(x, predict(fit.5, x), lwd = 3, col = "blue") # add lines to plot
#lines(x, predict(fit.2, x), lwd = 3, col = "green")# add lines to plot

rmse1.0_train <- sqrt( mean( (y_train - predict(fit1.0))^2 ) ) #calculate RMSE
rmse.5_train <- sqrt( mean( (y_train - predict(fit.5))^2 ) ) # calculate RMSE
rmse.2_train <- sqrt( mean( (y_train - predict(fit.2))^2 ) ) # calculate RMSE

rmse1.0_test <- sqrt( mean( (y_test - predict(fit1.0))^2 ) ) #calculate RMSE
rmse.5_test <- sqrt( mean( (y_test - predict(fit.5))^2 ) ) # calculate RMSE
rmse.2_test <- sqrt( mean( (y_test - predict(fit.2))^2 ) ) # calculate RMSE

rmse1.0_train
rmse.5_train
rmse.2_train

rmse1.0_test
rmse.5_test
rmse.2_test



