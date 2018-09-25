height_vector <- c(rnorm(200,1.70,0.07))
weight_vector <- c(rnorm(200,65,10))
noise_vector <- c(runif(200,-0.3,0.3))
meat_vector <- 0.35*height_vector + 0.15*weight_vector^(1/2) + noise_vector
meateaters <- data.frame("Height" = height_vector, "Weight" = weight_vector, "Kg of meat" = meat_vector) 

#There's a meat eating competition in Minerva and we want to know
#how many kg of meat a person can eat depending on his/her height
#and weight. We found the following equation : 0.35*height +
#0.15*weight^(1/2) + r = kg_of_meat, where r is the noise that
#ranges from -0.3kg to 0.3kg