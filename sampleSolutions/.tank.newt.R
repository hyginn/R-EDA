## Sample solution >>>                                               (tank.newt)

##  -  is that the case?
dMF <- LPSdat$MF.LPS  - LPSdat$MF.ctrl       # compute the response to LPS
dMo <- LPSdat$Mo.LPS  - LPSdat$Mo.ctrl

cor(dMF, dMo)                                # calculate correlation

##  -  plot a scatterplot
plot(dMF, dMo,
     pch=21,
     col="#00228855",
     bg="#0077FF44",
     cex = 1.4)

##  -  calculate a linear fit
(lFit <- lm(dMo ~ dMF))

abline(lFit, col="firebrick")                # add a regresion line to the plot

                                             # plot pp and pc limits ...
o <- order(dMF)                              # o is an index vector that sorts
                                             # the dMF values into increasing
                                             # order

pc <- predict(lm(dMo[o] ~ dMF[o]), interval = "confidence")
pp <- predict(lm(dMo[o] ~ dMF[o]), interval = "prediction")
## Note that we are re-ordering the genes with "o"

plot(dMF, dMo,                               # LPS response values
     xlab = "Macrophage stimulation",        # label for x-axis
     ylab = "Monocyte stimulation",          # label for y-axis
     xlim = range(c(dMF, dMo, pp)),          # make the limits for x- and y-
     ylim = range(c(dMF, dMo, pp)),          #   the same, and large enough to
     pch=21,                                 #   accommodate all points
     col="#00228855",                        # colour for point border
     bg="#0077FF44",                         # colour for point area
     cex = 1.4)                              # scale of the points

matlines(dMF[o], pc, lty=c(1,2,2), col="slategrey")
matlines(dMF[o], pp, lty=c(1,3,3), col="firebrick")

## <<< Sample solution
# [END]
