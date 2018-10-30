#Problem 1
set.seed(123)

#data-generating equation
X <- seq(1,5,length.out = 100)
errors <- rnorm(100)
Y <- 3*X + errors

#outlier
X[100] = 20
Y[100] = -20

reg_no_outlier <- lm(Y[1:99] ~ X[1:99])
reg_outlier <- lm(Y ~ X)
summary(reg_no_outlier)
summary(reg_outlier)

plot(X,Y)
abline(reg_no_outlier, col = 'red')
abline(reg_outlier, col = 'blue')

#Problem 2

#loading data
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
Predict_Intervals <- function(xmatrix,sim_params,num_sims) {
  sim_ys <- data.frame()
  for (j in 1:length(xmatrix[,1])) {
    for (i in 1:num_sims) {
      sim_ys[i,j] <- sim_params@coef[i,1] + sum(xmatrix[j,]*sim_params@coef[i,2:length(xmatrix[,1])]) + rnorm(1, 0, sim_params@sigma[i])
    }
  }
  return (apply(sim_ys,2,quantile,probs = c(0.025, 0.975)))
}

stored3 <- Predict_Intervals(matrix_b, sim_params_re78,500)
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
matrix_b <- as.data.frame(cbind(ages, rep(quant90_educ,39), rep(quant90_re74,39), rep(quant90_re75,39)))
names(matrix_b) = c('age','educ','re74','re75')
matrix_b['educ*re74'] <- matrix_b$educ*matrix_b$re74
matrix_b['educ*re75'] <- matrix_b$educ*matrix_b$re75
matrix_b['age*re74'] <- matrix_b$age*matrix_b$re74
matrix_b['age*re75'] <- matrix_b$age*matrix_b$re75
matrix_b['re74*re75'] <- matrix_b$re74*matrix_b$re75

#95% prediction intervals for scenario b
set.seed(1)
pred_intervals_b <- Predict_Intervals(matrix_b, sim_params_re78, 10000)

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

#Problem 3

#loading data and packages
library(foreign)
nswdata <- read.dta('/Users/juan/Downloads/nsw.dta')

#my bootstrap function. The obs parameter is a matrix in which the observations are each of the rows. B is the number of samples set as n by default

data.frame(cbind(intercept = c(NA),treat = c(NA)))
my_bootstrap <- function(obs, B = nrow(obs)) {
  boot_estimates <- data.frame(cbind(intercept = c(NA),treat = c(NA)))
  for (i in 1:B) {
    inds_samp <- sample(1:nrow(obs), nrow(obs), replace = TRUE)
    one_boot_data <- obs[inds_samp,]
    boot_estimates[i,] <- lm(re78 ~ treat, data = one_boot_data)$coef
  }
  return(boot_estimates)
}

#bootstrapped estimates
nsw_bootstrapped <- my_bootstrap(nswdata)

#bootstrapped 95% confidence interval
bootstrap_intervals <- apply(quantile(nsw_bootstrapped, probs = c(0.025, 0.975)))

#analytical 95% confidence interval
analytical_interval <- confint(lm(re78 ~ treat, data = nswdata))

#Problem 4

R_squared <- function (pred_ys, real_ys) {
  SSE = sum((pred_ys-real_ys)^2)
  SST = sum((pred_ys-mean(real_ys))^2)
  return(1-SSE/SST)
}

#Problem 5

logit_nsw <- glm(treat ~ age + education + black + hispanic + married + nodegree + re75, family = "binomial", data = nswdata)
estimated_probs <- predict(logit_nsw, type = "response")
estimated_probs_df <- data.frame(nswdata$treat,estimated_probs)
treat_inds <- which(estimated_probs_df$nswdata.treat == 1)
hist(estimated_probs_df[treat_inds,2])
hist(estimated_probs_df[-treat_inds,2])
