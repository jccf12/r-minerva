training_set <- read.csv(“training_set.csv”)
x <- training_set$x_train
Y <- training_set$y_train

plot(x,y); # except now, you need training_set$x_train and training_set$y_train

fit1.0 <- loess(y~x, span = 1, degree = 1) # 
fit.5 <- loess(y~x, span = 0.5, degree = 1) # 
fit.2 <- loess(y~x, span = 0.2, degree = 1) # 

lines(x, predict(fit1.0, x), lwd = 3, col = "red") # add lines to plot
lines(x, predict(fit.5, x), lwd = 3, col = "blue") # add lines to plot
lines(x, predict(fit.2, x), lwd = 3, col = "green")# add lines to plot

rmse1.0 <- sqrt( mean( (y - predict(fit1.0))^2 ) ) #calculate RMSE
rmse.5 <- sqrt( mean( (y - predict(fit.5))^2 ) ) # calculate RMSE
rmse.2 <- sqrt( mean( (y - predict(fit.2))^2 ) ) # calculate RMSE
