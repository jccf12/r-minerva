#Beginning Question 2
foo <- read.csv("https://course-resources.minerva.kgi.edu/uploaded_files/mke/00086677-3767/peace.csv")

# extract relevant columns
foo <- foo[, c(6:8, 11:16, 99, 50, 114, 49, 63, 136, 109, 126, 48, 160, 142, 10)]

# remove 2 rows with missing data (there are better ways to handle missing data)
foo <- foo[c(-19, -47), ]

# check that all missing data is gone...
which(is.na(foo) == TRUE)

# take a peek at the data set (identify the columns)
head(foo)

#Original Model
glm1 <- glm(pbs2s3 ~ wartype + logcost + wardur + factnum + factnum2 + trnsfcap + untype4 
            + treaty + develop + exp + decade, data = foo, family = binomial)

#Model with added interaction term
glm2 <- glm(pbs2s3 ~ wartype + logcost + wardur + factnum + factnum2 + trnsfcap + untype4 
            + treaty + develop + exp + decade + untype4*wardur, data = foo, family = binomial)

summary(glm1)
summary(glm2)

means <- c(mean(foo$wartype), mean(foo$logcost), NA, mean(foo$factnum),
           mean(foo$factnum2), mean(foo$trnsfcap), mean(foo$untype4),  mean(foo$treaty),
           mean(foo$develop), mean(foo$exp), mean(foo$decade))

X <- c(means[1:2],1,means[4:11])

for (i in 2:315) {
  X <- rbind(X,c(means[1:2],i,means[4:11]))
}

X <- as.data.frame(X)
names(X) <- c('wartype', 'logcost', 'wardur', 'factnum', 'factnum2', 'trnsfcap',
              'untype4', 'treaty', 'develop', 'exp', 'decade')

X_control <- X
X_control['untype4'] = 0
X_treat <- X
X_treat['untype4'] = 1

Y_control_original <- predict(glm1, newdata = X_control, type = 'response')
Y_treat_original <- predict(glm1, newdata = X_treat, type = 'response')
y_plot_original <- Y_treat_original - Y_control_original

Y_control_interaction <- predict(glm2, newdata = X_control, type = 'response')
Y_treat_interaction <- predict(glm2, newdata = X_treat, type = 'response')
y_plot_interaction <- Y_treat_interaction - Y_control_interaction

plot(y_plot_original, type = 'l', , lty = 3, 
     ylab = 'Marginal effects of UN peacekeeping operations', 
     xlab = 'Duration of wars in months', 
     xlim=c(0, 315), ylim=c(0.0, 0.8)) 
lines(y_plot_interaction, type = 'l')
text(80,0.2, cex =0.7,'Model with interaction term')
text(190,0.43, cex =0.7,'Dotted: Original model')

#End of Question 2

#Beginning of Question 3
foo <- read.csv("https://course-resources.minerva.kgi.edu/uploaded_files/mke/00086677-3767/peace.csv")

foo <- foo[, c(6:8, 11:16, 99, 50, 114, 49, 63, 136, 109, 48, 160, 142, 10, 34:35,55)]
NAs <- which(is.na(foo) == TRUE)
for (i in 1:length(NAs)) {
  print(NAs[i] - floor(NAs[i]/124)*124)
}

foo <- foo[c(-19, -47, -4, -16, -84, -93, -98), ]

which(is.na(foo) == TRUE)

foo$untype

#(3) Define treatment as below:
Tr <- rep(0, length(foo$untype))
Tr[which(foo$untype != "None")] <- 1

foo$untype <- Tr
names(foo)

#LOGISTIC REGRESSION

glm_pbs2l <- glm(pbs2l ~ untype + wartype + logcost + wardur + factnum + factnum2 + trnsfcap
                 + treaty + develop + exp + decade, data = foo, family = binomial)

glm_pbs5l <- glm(pbs5l ~ untype + wartype + logcost + wardur + factnum + factnum2 + trnsfcap
                 + treaty + develop + exp + decade, data = foo, family = binomial)

summary(glm_pbs2l)
summary(glm_pbs5l)

#PROPENSITY SCORES

glm_propensity <- glm(untype ~ wartype + logcost + wardur + factnum + factnum2 + trnsfcap
                      + treaty + develop + exp + decade, data = foo, family = binomial)

X_variables <- cbind(foo$wartype, foo$logcost, foo$wardur, foo$factnum, foo$factnum2, foo$trnsfcap, foo$untype, 
                foo$treaty, foo$develop, foo$exp, foo$decade)

propensity_mout_2 <- Match(Y=foo$pbs2l,Tr=Tr, X=glm_propensity$fitted, estimand="ATT", BiasAdjust = TRUE)
propensity_mout_5 <- Match(Y=foo$pbs5l,Tr=Tr, X=glm_propensity$fitted, estimand="ATT", BiasAdjust = TRUE)
summary(propensity_mout_2, full = TRUE)
summary(propensity_mout_5, full = TRUE)

mb_propensity <- MatchBalance(untype ~ wartype + logcost + wardur + factnum + factnum2 + trnsfcap
                              + treaty + develop + exp + decade, data = foo,
                                    match.out=propensity_mout_2, nboots=500)

#GENETIC MATCHING

Xgen = cbind(foo$wartype, foo$logcost, foo$wardur, foo$factnum, foo$factnum2, foo$trnsfcap,
          foo$treaty, foo$develop, foo$exp, foo$decade)

genout <- GenMatch(Tr=foo$untype, X=Xgen, estimand="ATT",
                   pop.size=700, max.generations=25, wait.generations=25)


mout2 <- Match(Y=foo$pbs2l,Tr=foo$untype, X=Xgen, estimand="ATT", Weight.matrix=genout, BiasAdjust = TRUE)
mout5 <- Match(Y=foo$pbs5l,Tr=foo$untype, X=Xgen, estimand="ATT", Weight.matrix=genout, BiasAdjust = TRUE)
summary(mout2, full = TRUE)
summary(mout5, full = TRUE)

mb <- MatchBalance(untype ~ wartype + logcost + wardur + factnum + factnum2 + trnsfcap
                   + treaty + develop + exp + decade, data = foo,
                   match.out=mout2, nboots=500)
