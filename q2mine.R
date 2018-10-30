library(arm)
library(Matching)
data("lalonde")

#isolating control group
lalonde_control <- lalonde[which(lalonde$treat == 0),] 

#linear model
predict_re78_control <- lm(re78 ~ age + educ + re74 + re75 + educ*re74 +
                             educ*re75 + age*re74 + age*re75 + re74*re75, data = lalonde_control)


#simulating the parameters
sim_params_re78 <- sim(predict_re78_control, n.sims=10000)

#age domain
ages <- c(min(lalonde_control$age):max(lalonde_control$age))
length(ages)

#calculating the medians
med_educ <- median(lalonde_control$educ)
med_re74 <- median(lalonde_control$re74)
med_re75 <- median(lalonde_control$re75)

set.seed(1)

#simulating the predicted values
sim_ys_a <- data.frame()
for (j in 1:39) {
  xs_a <- c(1,ages[j],med_educ, med_re74, med_re75, med_educ*med_re74,
            med_educ*med_re75, ages[j]*med_re74, ages[j]*med_re75, med_re74*med_re75)
  for (i in 1:10000) {
    betas_a <- sim_params_re78@coef[i,]
    sim_ys_a[i,j] <- sum(xs_a*betas_a) + rnorm(1, 0, sim_params_re78@sigma[i])
  }
}

pred_intervals_a <- apply(sim_ys_a,2,quantile,probs = c(0.025, 0.975))
pred_intervals_a

#plotting the intervals  
plot(x = c(1:100), y = c(1:100), type = "n", xlim = c(17,55), ylim = c(1.25*min(pred_intervals_a[1,]),1.25*max(pred_intervals_a[2,])), 
     main = "", xlab = "age", ylab = "re78")

for (age in 17:55) {
  segments(
    x0 = age,
    y0 = pred_intervals_a[1, age - 16],
    x1 = age,
    y1 = pred_intervals_a[2, age - 16],
    lwd = 2)
}
```

#calculating the 90% quantiles
quant90_educ <- quantile(lalonde_control$educ, probs = 0.9) 
quant90_re74 <- quantile(lalonde_control$re74, probs = 0.9)
quant90_re75 <- quantile(lalonde_control$re75, probs = 0.9)

set.seed(1)

#simulating the predicted values
sim_ys_b <- data.frame()
for (j in 1:39) {
  xs_b <- c(1,ages[j],quant90_educ, quant90_re74, quant90_re75, ages[j]*quant90_re74, ages[j]*quant90_re75, quant90_educ*quant90_re74,
            quant90_educ*quant90_re75,  quant90_re74*quant90_re75)
  for (i in 1:10000) {
    betas_b <- sim_params_re78@coef[i,]
    sim_ys_b[i,j] <- sum(xs_b*betas_b) + rnorm(1, 0, sim_params_re78@sigma[i])
  }
}


pred_intervals_b <- apply(sim_ys_b, 2, quantile, probs = c(0.025, 0.975))
pred_intervals_b

plot(x = c(1:100), y = c(1:100), type = "n", xlim = c(17,55), ylim = c(1.25*min(pred_intervals_b[1,]),1.25*max(pred_intervals_b[2,])), 
     main = "", xlab = "age", ylab = "re78")

for (age in 17:55) {
  segments(
    x0 = age,
    y0 = pred_intervals_b[1, age - 16],
    x1 = age,
    y1 = pred_intervals_b[2, age - 16],
    lwd = 2)
}