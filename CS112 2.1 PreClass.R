height_vector <- c(rnorm(200,1.70,0.07))
weight_vector <- c(rnorm(200,65,10))
noise_vector <- c(runif(200,-0.3,0.3))
meat_vector <- 0.35*height_vector + 0.15*weight_vector^(1/2) + noise_vector
meateaters <- data.frame("Height" = height_vector, "Weight" = weight_vector, "Meat" = meat_vector) 

subset1 <- sample(c(1:200), 100) 

meateaters_subset1 <- meateaters[subset1,]  #this takes subset1 observations and all the columns
meateaters_subset2 <- meateaters[-subset1,] #this takes all EXCEPT subset1 obs, w/ all columns

names(lalonde_subset1)
#hist(lalonde_subset1$educ)
plot(meateaters_subset1$Height, meateaters_subset1$Meat)
