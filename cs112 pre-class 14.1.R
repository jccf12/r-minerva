pre_income <- c(20:1019)
randerr <- runif(1000,-60,60)


pre_income_all <- pre_income*3 + randerr

randerr2 <- runif(1000,-20,20)
randerr3 <- runif(1000,-20,20)

#Y(0)
post_income_all_treat <- pre_income_all + 500 + randerr2

#Y(1)
post_income_all_control <- pre_income_all + randerr3


cutoff_treated <- post_income_all_treat[c(1:500)]
cutoff_control <- post_income_all_control[c(501:1000)]

reg_discontinuity_data <- data.frame(cutoff_treated,cutoff_control)

write.csv(reg_discontinuity_data, file = "reg_discontinuity_data.csv")

#https://drive.google.com/file/d/12MzzSGBWQnR99jmQIBNV4TQtPXcvTP5G/view?usp=sharing