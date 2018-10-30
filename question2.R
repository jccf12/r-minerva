library(arm)
library(Matching)
data("lalonde")

#isolating control group
lalonde_control <- lalonde[which(lalonde$treat == 0),] 

#linear model

predict_re78_control <- lm(re78 ~ age + educ + re74 + re75 + educ*re74 +
                             educ*re75 + age*re74 + age*re75 + re74*re75, data = lalonde_control)

#simulating the parameters
set.seed(123)
sim_params_re78 <- sim(predict_re78_control, n.sims=10000)

#function for predicting intervals given the predictor matrix, simulated paramaters, and desired n.sims

#age domain
ages <- c(min(lalonde_control$age):max(lalonde_control$age))
length(ages)

#calculating the medians
med_educ <- median(lalonde_control$educ)
med_re74 <- median(lalonde_control$re74)
med_re75 <- median(lalonde_control$re75)

#creating the predictor matrix for scenario a
matrix_a <- as.data.frame(cbind(ages, rep(med_educ,39), rep(med_re74,39), rep(med_re75,39)))
names(matrix_a) = c('age','educ','re74','re75')
matrix_a['educ*re74'] <- matrix_a$educ*matrix_a$re74
matrix_a['educ*re75'] <- matrix_a$educ*matrix_a$re75
matrix_a['age*re74'] <- matrix_a$age*matrix_a$re74
matrix_a['age*re75'] <- matrix_a$age*matrix_a$re75
matrix_a['re74*re75'] <- matrix_a$re74*matrix_a$re75

#95% prediction intervals for scenario a
pred_intervals_a <- Predict_Intervals(matrix_a, sim_params_re78, 10000)

#plotting the intervals  
plot(x = c(1:100), y = c(1:100), type = "n", xlim = c(17,55), ylim = c(1.5*min(pred_intervals_a[1,]),1.5*max(pred_intervals_a[2,])), 
     main = "main", xlab = "xlab", ylab = "ylab")

for (age in 17:55) {
  segments(
    x0 = age,
    y0 = pred_intervals_a[1, age - 16],
    x1 = age,
    y1 = pred_intervals_a[2, age - 16],
    lwd = 2)
}

#calculating the 90% quantiles
quant90_educ <- quantile(lalonde_control$educ, probs = 0.9) 
quant90_re74 <- quantile(lalonde_control$re74, probs = 0.9)
quant90_re75 <- quantile(lalonde_control$re75, probs = 0.9)

#creating the predictor maxtrix for scenario b
matrix_b <- as.data.frame(cbind(rep(1,39),ages, rep(quant90_educ,39), rep(quant90_re74,39), rep(quant90_re75,39)))
names(matrix_b) = c('intercept','age','educ','re74','re75')
matrix_b['educ*re74'] <- matrix_b$educ*matrix_b$re74
matrix_b['educ*re75'] <- matrix_b$educ*matrix_b$re75
matrix_b['age*re74'] <- matrix_b$age*matrix_b$re74
matrix_b['age*re75'] <- matrix_b$age*matrix_b$re75
matrix_b['re74*re75'] <- matrix_b$re74*matrix_b$re75


Predict_Intervals <- function(xmatrix,sim_params,num_sims) { 
  sim_ys <- data.frame()
  for (j in 1:length(xmatrix[,1])) {
    for (i in 1:num_sims) {
    sim_ys[i,j] <- 
      sum(xmatrix[j,]*sim_params@coef[i,]) + rnorm(1, 0, sim_params@sigma[i])
    }
  }

  return (apply(sim_ys,2,quantile,probs = c(0.025, 0.975)))
}

#95% prediction intervals for scenario b
set.seed(1)
#stored1 <- Predict_Intervals(matrix_b,sim_params_re78,1000)
#pred_intervals_b <-apply(stored1,2,quantile,probs = c(0.025, 0.975))
pred_intervals_b <- Predict_Intervals(matrix_b,sim_params_re78,10000)

set.seed(1)
stored2 = data.frame()
#repeat process with new values and data frame. 
for(j in ages){
  for(i in 1:10000){
    stored2[i, j - 16] <- 
      sum(matrix_b[j-16,] * sim_params_re78@coef[i,] )+ rnorm(1, 0, sim_params_re78@sigma[i])
  }
}
confints2 = apply(stored2, 2, quantile, probs = c(0.025, 0.975))

#plotting the intervals
plot(x = c(1:100), y = c(1:100), type = "n", xlim = c(17,55), ylim = c(1.5*min(pred_intervals_b[1,]),1.5*max(pred_intervals_b[2,])), 
     main = "b", xlab = "xlab", ylab = "ylab")

for (age in 17:55) {
  segments(
    x0 = age,
    y0 = pred_intervals_b[1, age - 16],
    x1 = age,
    y1 = pred_intervals_b[2, age - 16],
    lwd = 2)
}
