## Sample solution >>>

## In our LPS data, MF and Mo aught to respond similarly to LPS challenge.
## If so, the LPS - ctrl data should be highly correlated.
##
## TASK:
##  -  is that the case?
dMF <- LPSdat$MF.LPS  - LPSdat$MF.ctrl
dMo <- LPSdat$Mo.LPS  - LPSdat$Mo.ctrl

cor(dMF, dMo)

##  -  plot the scatterplot for this hypothesis,
plot(dMF, dMo,
     pch=21,
     col="#00228855",
     bg="#0077FF44",
     cex = 1.4)

##  -  calculate a linear fit
lFit <- lm(dMo ~ dMF)

abline(lFit, col="firebrick")

pc <- predict(lm(dMo ~ dMF), interval = "confidence")
pp <- predict(lm(dMo ~ dMF), interval = "prediction")
head(pc)

# Now plot pp and pc limits
# first sort on x
o <- order(dMF) # o is an index vector, sorted on x-values

# second, recompute pp, pc in sorted order
pc <- predict(lm(dMo[o] ~ dMF[o]), interval = "confidence")
pp <- predict(lm(dMo[o] ~ dMF[o]), interval = "prediction")

# Then plot
plot(dMF, dMo,
     xlab = "Macrophage stimulation",
     ylab = "Monocyte stimulation",
     xlim = range(c(dMF, dMo, pp)),
     ylim = range(c(dMF, dMo, pp)),
     pch=21,
     col="#00228855",
     bg="#0077FF44",
     cex = 1.4)

matlines(dMF[o], pc, lty=c(1,2,2), col="slategrey")
matlines(dMF[o], pp, lty=c(1,3,3), col="firebrick")

##  -  assess whether there is a linear correlation.

## <<< Sample solution
# [END]
