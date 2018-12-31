data(lalonde)

#The covariates we want to match on
X = cbind(lalonde$age, lalonde$educ, lalonde$black, lalonde$hisp)

#The covariates we want to obtain balance on

#
#Let's call GenMatch() to find the optimal weight to give each
#covariate in 'X' so as we have achieved balance on the covariates in
#'BalanceMat'. This is only an example so we want GenMatch to be quick
#so the population size has been set to be only 16 via the 'pop.size'
#option. This is *WAY* too small for actual problems.
#For details see http://sekhon.berkeley.edu/papers/MatchingJSS.pdf.
#
genout <- GenMatch(Tr=lalonde$treat, X=X, estimand="ATT",
                   pop.size=300, max.generations=10, wait.generations=10)

 #The outcome variable
Y=lalonde$re78

#
# Now that GenMatch() has found the optimal weights, let's estimate
# our causal effect of interest using those weights
#
mout <- Match(Y=Y,Tr=lalonde$treat, X=X, estimand="ATT", Weight.matrix=genout)
summary(mout)

#                        
#Let's determine if balance has actually been obtained on the variables of interest
#                        
mb <- MatchBalance(lalonde$treat~lalonde$age + lalonde$educ + lalonde$black + lalonde$hisp,
                   match.out=mout, nboots=500)
