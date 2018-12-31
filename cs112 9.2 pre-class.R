DW_data
data(lalonde)

library(Matching)

glm1  <- glm(treat~age + education + black +
               hispanic, family=binomial, data=DW_data)


X  <- glm1$fitted
Tr  <- lalonde$treat

rr  <- Match(Tr=Tr, X=X, M=3)
summary(rr)

mb  <- MatchBalance(treat~age + education + black +
                      hispanic, data=DW_data, match.out=rr, nboots=10)

data(lalonde)

X = cbind(DW_data$age, DW_data$education, DW_data$black, 
          DW_data$hispanic)

install.packages("rgenoud")
genout <- GenMatch(Tr = DW_data$treat, X=X, estimand="ATT", M=1,
                   pop.size=16, max.generations=10, wait.generations=1)


mout <- Match(Tr= DW_data$treat, X=X, estimand="ATT", Weight.matrix=genout)
summary(mout)

mb <- MatchBalance(DW_data$treat ~ DW_data$age + DW_data$education + DW_data$black + DW_data$hispanic,
                   match.out=mout, nboots=500)