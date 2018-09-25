foo <- read.csv(url("https://docs.google.com/spreadsheets/d/e/2PACX-1vQC5enEj91bsrAmXER5z0eC_xlUbe_rhYiXEeTlb90-w1pxbqfmejfRwaCvRZzm201_nSlS-ZG0MN70/pub?gid=1873712132&single=true&output=csv"))
summary(foo)
lm(foo)
head(foo)
foo

selection <- foo$treatment == 1
indexes <- which(selection)

treatment <- foo[indexes,]
control <- foo[-indexes,]

abline(regeq, lwd = 2, col = “blue)

mean(post.test[treatment == 1]) - mean(post.test[treatment==0])

plot(treatment, post.test, pch = 20, col = "purple4", cex = 1.2)
points(0, mean(post.test[treatment==0]), col = "green", pch = 20, cex = 3)
points(1, mean(post.test[treatment==1]), col = "green", pch = 20, cex = 3)
abline(lm1, col = "red", lwd = 1.2)
