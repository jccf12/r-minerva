# Install the "arm" package and load the library

install.packages("arm")

library(arm)

# Load data given in pre-class work: "http://tinyurl.com/z5ygmch"

# RUN THE REGRESSION
sesame <- read.csv(url("https://docs.google.com/spreadsheets/d/e/2PACX-1vQC5enEj91bsrAmXER5z0eC_xlUbe_rhYiXEeTlb90-w1pxbqfmejfRwaCvRZzm201_nSlS-ZG0MN70/pub?gid=1873712132&single=true&output=csv"))

# Note: The ".4" below, (“lm.4”), indicates results for students in grade 4
lm.4 <- lm (post.test ~ treatment + pre.test + I(treatment*pre.test), data = sesame)
lm.4.equivalent <- lm(post.test ~ treatment*pre.test, data = sesame)
lm.4.equivalent2 <- lm(post.test ~ treatment + pre.test + treatment:pre.test, data = sesame)
display (lm.4)
display (lm.4.equivalent) # Just to demonstrate how this syntax works
display (lm.4.equivalent2)# Just to demonstrate how this syntax works

# APPLY THE "SIM" FUNCTION TO SIMULATE THE UNCERTAINTY
lm.4.sim <- sim (lm.4.equivalent, n.sims = 10)
lm.4.sim

(lm.4.sim)@coef # explore the simulated coefficients all equally likely under this model

# QUESTION: Which coefficients are the key for estimating treatment effects?
# the coefficients for the treatment indicator and interaction term?
# Confirm that they are the SECOND and FOURTH columns, following the logic below...

# According to this model...
# treatment effect = Y(treatment = 1) - Y(treatment = 0)

# treatment effect = (coef(treatment)*treatment + coef(pre.test)*pre.test + 
#                     coef(interaction)*treatment*pre.test) when treatment == 1
#                    MINUS
#                    (coef(treatment)*treatment + coef(pre.test)*pre.test + 
#                     coef(interaction)*treatment*pre.test) when treatment == 0

# treatment effect = coef(treatment)*1 + coef(pre.test)*pre.test + coef(interaction)*1*pre.test) 
#                    MINUS
#                    (coef(treatment)*0 + coef(pre.test)*pre.test + coef(interaction)*0*pre.test)

# treatment effect = coef(treatment) + coef(interaction)*pre.test) 


# treatment effect = coef(interaction.term)*pre.test + coef(treatment)

# NOTICE!!!   This is what we worked on together as a class, earlier.
#             This equation can be rewritten as:
#             y = m*x + b  IF y = treatment effect and x = pre.test

# In other words, if you make a figure that plots pre-test on the x axis, and 
# treatment effect on the y-axis, coef(treatment) is "b", the intercept, and
# coef(interaction.term) is "m", the slope.

# Let's make a figure like that, reproducing the plot we saw earlier.

# REPRODUCE THE first line in the PLOT we saw earlier
# FIRST JUST THE AXES (no data plotted)
plot (0, 0, xlim=c(80, 120), ylim=c(-5,10),
      xlab="pre-test", ylab="treatment effect",
      main="treatment effect in grade 4")

abline (h = 0, lwd=.5, lty=2) # draws a horizontal line

# "abline" draws a line with intercept "a" and slope "b".
abline (a = coef(lm.4.sim)[1,2], b = coef(lm.4.sim)[1,4],
        lwd = .5, col = "gray")

## Additional OPTIONAL exercises...

# The line you just drew is associated with the first row in the table of 
# simulated coefficients.  
# You could write a "for loop" to draw one line for each 
# of the other rows..

for (i in 1:1000) {
  abline (a = coef(lm.4.sim)[i,2], b = coef(lm.4.sim)[i,4],
          lwd = .5, col = "gray")
}

## Recover the average treatment effect previously obtained by regression
## by simulating & averaging over the simulated coefficients.  Draw a thicker
## line showing the average treatment effect line.  
## Thicken the line with lwd = 3 added as an argument to abline.  
## You can also color the line: e.g., color = "purple4"

abline (a = mean(coef(lm.4.sim)[,2]), b = mean(coef(lm.4.sim)[,4]),
        lwd = 3, col = "purple4")

## Estimate standard error of 'treatment' coefficient by calculating the
## standard deviation of the simulated "treatment" coefficients.

## Use confint(lm.4.sim) to get confidence intervals, and then confirm the
## confidence intervals by simulation (using “quantile” function)...