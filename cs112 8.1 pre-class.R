storage.vector <- NA

# Function that assigns treatment/control depending on 
# propensity scores (assignment probabilities)

experiment <- function(vector.of.probabilities) {
  k = 0
  for (i in 1:length(vector.of.probabilities)) {
    if(
      sample(x = c(1,0), size = 1, prob = c(vector.of.probabilities[i], 
                                            1 - vector.of.probabilities[i])) == 1) {
      storage.vector[k] <- i
      k = k + 1
    }
  }
  return(list(treated.units = storage.vector, 
              control.units = (1:(length(vector.of.probabilities)))[-storage.vector]))
}

x <- c(.68,.42,.73,.79,.63,.40,.38,.44,.41)


experiment(probs.wnokids)
'''$treated.units
[1] 3 4 5 6

$control.units
[1] 1 2 7 8 9
'''
par(mfrow=c(2,1))
# Incomes for the female-headed households without children are defined per the following code:
set.seed(123); nokids.income <- round(abs(exp(rnorm(1000, 5, 1))))
hist(nokids.income)
max1 <- max(nokids.income) #=3793
min1 <- min(nokids.income) #=9
mean1 <- mean(nokids.income) #=248.094

# Household sizes for the female-headed households with children are defined per this code:
set.seed(123); kids.hhsize <- round(sqrt(abs(rnorm(1000, 12, 100))) + .3)
hist(kids.hhsize)
max2 <- max(kids.hhsize) #=19
min2 <- min(kids.hhsize) #=0
mean2 <- mean(kids.hhsize) #=8.441

###
probs.wnokids <- 0.5*((((max(nokids.income) + 100) -
                          nokids.income)/(max(nokids.income) + 100))^4)

probs.wyeskids <- kids.hhsize/(max(kids.hhsize) + 1)
data.frame(probs.wnokids,probs.wyeskids)
plot(kids.hhsize,probs.wyeskids)
plot(nokids.income,probs.wnokids)


city.names <- c("A", "B", "C", "D", "E", "F", "G", "H")
observed.turnout = c( 17, 30, 13, 55, 26, 29, 48, 43)

observed.diffmeans <- mean(observed.turnout[c(2,4,6,8)]) - 
  mean(observed.turnout[c(1,3,5,7)])

print(observed.diffmeans)

foo <- data.frame(city.names, observed.turnout)

# Assignment function
assignment <- function() {
  # Four coin flips, establishing random assignment
  assig        <- foo[sample(1:2),]
  assig[3:4,]  <- foo[sample(3:4),]
  assig[5:6,]  <- foo[sample(5:6),]
  assig[7:8,]  <- foo[sample(7:8),]
  
  treatment.group   <- assig[c(1,3,5,7),]
  control.group     <- assig[c(2,4,6,8),]
  
  return(mean(treatment.group[,2]) - mean(control.group[,2]))
}

# Iterating the Assignment function
iter.RI <- function(iterations = 10000) {
  for (i in 1:iterations) 
  {storage.vector[i] <- assignment()
  
  }
  return(storage.vector)
}

storage.vector <- NULL
results <- iter.RI()
# Exploring the results

vvv <- quantile(results, prob = c(0.95))
length(unique(results))

hist(results)
plot(density(results))
abline(v = vvv, lwd = 2, col = "red")
vvv