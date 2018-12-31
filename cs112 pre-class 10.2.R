#preclass 10.2

install.packages("rbounds")

library(Matching)
library(rgenoud)
library(rbounds)
library(foreign)

nsw <- read.dta("nsw.dta")
head(nsw)

Y <- nsw$re78
Tr <- nsw$treat

#GenMatch

X <- cbind(nsw$age, nsw$education, nsw$black, nsw$hispanic, nsw$married, nsw$nodegree, nsw$re75)

BalanceMat <- cbind(nsw$age, I(nsw$age^2), nsw$education, I(nsw$education^2), nsw$black, 
                    nsw$hispanic, nsw$married, nsw$nodegree, nsw$re75 , I(nsw$re75^2), I(nsw$age*nsw$nodegree), I(nsw$educ*nsw$re75))

#Genetic Weights

gen1 <- GenMatch(Tr=Tr, X=X, BalanceMat=BalanceMat, pop.size=200, 
                 data.type.int=FALSE, print=0, replace=FALSE, M=1)

#Match
mgen1 <- Match(Y=Y, Tr=Tr, X=X, Weight.matrix=gen1, replace=FALSE)

mb <- MatchBalance(nsw$treat ~ nsw$age + I(nsw$age^2) + nsw$education + I(nsw$education^2) + nsw$black + 
                     nsw$hispanic + nsw$married + nsw$nodegree + nsw$re75 + I(nsw$re75^2) + 
                     I(nsw$age*nsw$nodegree) + I(nsw$educ*nsw$re75),
                   data = nsw, match.out=mgen1, nboots=200)


summary(mgen1)

psens(mgen1, Gamma=1.5, GammaInc=.1)



hlsens(mgen1, Gamma=1.5, GammaInc=.1, .1)
?hlsens
