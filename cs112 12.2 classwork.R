library(AER)
library(Matching)
library(rbounds)
jtpa <- read.dta('/Users/juan/Downloads/jtpa.dta')
summary(ivreg(earnings ~ training | assignmt, data = jtpa))

summary(lm(earnings ~ training, data = jtpa))
summary(lm(earnings ~ training + afdc + sex + age , data = jtpa))
summary(lm(earnings ~ training + prevearn + married + black , data = jtpa))

mout <- Match(Tr = jtpa$training, Y=jtpa$earnings, 
              X = cbind(jtpa$afdc, jtpa$sex, jtpa$age,jtpa$prevearn, 
                        jtpa$married,jtpa$black, jtpa$hispanic))
              
summary(mout)

psens(mout, GammaInc = 0.1)

library(ivmodel)

ivmodel(Y = jtpa$earnings, Z = jtpa$assignmt, D = jtpa$training, 
        deltarange = c(-0.05, 0.05))
