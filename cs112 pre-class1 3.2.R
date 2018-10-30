library(MASS)
data(Prima.tr)
data("Pima.tr")
data("Pima.te")
Pima.tr
glm.fit <- glm(type ~ bmi+age, data = Pima.tr, family = binomial)
summary(glm.fit)

predicted_probs <- predict(glm.fit, type = "response")
predicted_ys <- rep(0, length(predicted_probs))
predicted_ys[predicted_probs > 0.25] = 1

predicted_probs_test <- predict(glm.fit, newdata = Pima.te, type = "response")
predicted_ys_test <- rep(0, 332)
predicted_ys_test[predicted_probs_test > 0.25] = 1

table(predicted_ys, Pima.tr$type)
table(predicted_ys_test, Pima.te$type)

for (i in 1:10) {
  table(predic)
}

