#Question 2
excluded <- which(lalonde$treat == 1)
lalonde.control <- lalonde[-excluded, ] #Only control group patients

lm_control = lm(re78 ~ age + educ + re74 +re75 +
                  educ*re74 + educ*re75 + age*re74 + age*re75 + re74*re75,
                data = lalonde.control) #Linear model with the specified predictors
lm_control$coef
#intercept = 3685.51, age = 2.21, educ = 39.06, re74 = -0.015, re75 = 0.784, 
#educ:re74 = 0.0344, educ:re75 = -0.072, age:re74 = -0.0057, age:re75 = 0.0093,
#re74:re75 = -0.000022

set.seed(123)
sim_results <- sim(lm_control, n.sims = 10000)
mean(sim_results@coef[,10]) #try this with all numbers for predictors gives following data
#            From sim         From lm_control
#Intercept   3645.201           3685.51
#age         2.234              2.21
#educ        43.319             39.06
#re74        -0.023             -0.015 
#re75        0.832               0.784
#educ:re74    0.033             0.0344
#educ:re75    -0.074            -0.072
#age:re74     -0.0051           -0.0057
#age:re75     0.008             0.0093
#re74:re75   -0.0000229          -0.000022

sigmas <- c(sim_results@sigma)
coefs <- c(sim_results@coef)

#find the medians
med.edu = median(lalonde.control$educ)
med.re74 = median(lalonde.control$re74)
med.re75 = median(lalonde.control$re75)
ages = c(min(lalonde.control$age): max(lalonde.control$age))

set.seed(1)
stored = data.frame() #Create an empty data frame

#Nested for loop that keeps valued at their medians and only age varies.
#Includes sigmas
for(j in ages){
  unit = c(1, j, med.edu, 0, 0, 0, 0, 0, 0, 0)
  for(i in 1:10000){
    stored[i, j - 16] <- 
      sum(unit * sim_results@coef[i,] )+ rnorm(1, 0, sim_results@sigma[i])
  }
}
stored
confints = apply(stored, 2, quantile, probs = c(0.025, 0.975)) #find confidence interval per age

#plot the graph
plot(x = c(1:100), y = c(1:100), type = "n", xlim = c(17,55), ylim = c(-20000,30000), 
     main = "Confidence intervals per age with variables at their medians", xlab = "Age", 
     ylab = "Real earnings in 1978")

for (age in ages) {
  segments(
    x0 = age,
    y0 = confints[1, age - 16],
    x1 = age,
    y1 = confints[2, age - 16],
    lwd = 2)
}

#90 Quantile problem
#set values at their 90th quantile
edu.90 = quantile(lalonde.control$educ, probs = c(0.9))
re74.90 = quantile(lalonde.control$re74, probs = c(0.9))
re75.90 = quantile(lalonde.control$re75, probs = c(0.9))

set.seed(1)
stored2 = data.frame()
#repeat process with new values and data frame. 
for(j in ages){
  unit = c(1, j, edu.90, re74.90, re75.90, j*re74.90, j*re75.90, edu.90*re74.90, edu.90*re75.90, re74.90*re75.90)
      re78 ~ age + educ + re74 +re75 + educ*re74 + educ*re75 + age*re74 + age*re75 + re74*re75,
  for(i in 1:10000){
    stored2[i, j - 16] <- 
      sum(unit * sim_results@coef[i,] )+ rnorm(1, 0, sim_results@sigma[i])
  }
}
head(stored2)
confints2 = apply(stored2, 2, quantile, probs = c(0.025, 0.975))

plot(x = c(1:100), y = c(1:100), type = "n", xlim = c(17,55), ylim = c(-70000,70000), 
     main = "Confidence intervals per age with variables at their 90th quantile", xlab = "Age", 
     ylab = "Real earnings in 1978")

for (age in ages) {
  segments(
    x0 = age,
    y0 = confints2[1, age - 16],
    x1 = age,
    y1 = confints2[2, age - 16],
    lwd = 2)
}