# EDAintroductionSampleSolutions-ShowPlot.R
#
# Purpose: show the final plot for the EDA introduction unit
# Version: 1.0
# Date:    2018-05-10
# Author:  Boris Steipe
#
# Input:   NA
# Output:  NA - plot as side-effect
# Dependencies: ./sampleSolutions/dat374378bc0.RData must exist
#
# ToDo:
# Notes:
#
# ==============================================================================

# Prepare:
# dat27d51984 <- LPSdat
#
# save("dat27d51984",
#      file = "./sampleSolutions/dat27d51984.RData")

# load
load("./sampleSolutions/dat27d51984.RData")

# plot
myStim <- dat27d51984$B.LPS - dat27d51984$B.ctrl
hist(myStim,
     freq = FALSE,
     breaks = 40,
     col="#0066AA44",
     main = "B.LPS - B.ctrl",
     xlab = "stimulation (delta(log(expression))")
xVals <- seq(min(myStim),max(myStim),0.1)
lines(xVals, dnorm(xVals, sd = sd(myStim)), col="#CC000055")

# clean up
rm(dat27d51984)

# [END]
