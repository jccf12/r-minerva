library(foreign)
nsw <- read.dta('/Users/juan/Downloads/nsw.dta')
summary(nsw)
head(nsw)

hist(nsw$age)
hist(nsw$education)
table(nsw$black)

pred_educ_black <- lm(education ~ black, data = nsw)
plot(nsw$black,nsw$education)
lines(nsw$black,predict(pred_educ_black))

pred_educ_hisp <- lm(education ~ hispanic, data = nsw)
plot(nsw$hispanic,nsw$education)
lines(nsw$hispanic,predict(pred_educ_black))

