# RegressionSampleSolutions-ShowPlot.R
#
# Purpose: show the final plot for the EDA Regression unit
# Version: 1.0
# Date:    2019  05
# Author:  Boris Steipe
#
# Input:   NA
# Output:  NA - plot as side-effect
# Dependencies: ./sampleSolutions/dat3c7be868.RData must exist
#
# ToDo:
# Notes:
#
# ==============================================================================

# Prepare:
# load("./data/CCgenes.RData")
# load("./data/ygProfiles.RData")
# dat3c7be868 <- CCgenes
# datd294e201 <- ygProfiles
# rm(CCgenes)
# rm(ygProfiles)
#
# save(dat3c7be868, datd294e201, file = "./sampleSolutions/dat3c7be868.RData")

load("./sampleSolutions/dat3c7be868.RData")

exVals <- matrix(numeric(ncol(datd294e201) * nrow(dat3c7be868)),
                 nrow = ncol(datd294e201), ncol = nrow(dat3c7be868))

# ... load it with (scaled) expression profiles, from CCgenes - i.e. ordered by
#     expression peak
N <- nrow(dat3c7be868)
for (iRow in 1:N) {
  exVals[ , N - iRow + 1] <- scale(datd294e201[dat3c7be868$i[iRow], ])
}
rownames(exVals) <- colnames(datd294e201)
colnames(exVals) <- dat3c7be868$ID

# ... plot as image
image(exVals,
      col = colorRampPalette(c("#1cacf3",
                               "#1cacf3",
                               "#0f8a94",
                               "#000000",
                               "#000000",
                               "#9f388a",
                               "#de2f5d",
                               "#de2f5d"))(256),
      xaxt = "n", yaxt = "n", xlab = "time (min.)", ylab= "rank of phase",
      main = "Cell cycle progression")
axis(1, at = seq(0, 1, length.out = 25),
     labels = seq(0, 120, by = 5), cex.axis = 0.5)
abline(v = 0.5 + (1/50), col = "white", lwd = 0.5)
yTicks <- 1 - (c(100, 300, 500, 700) / N)
axis(2, at = yTicks, labels = c("100", "300", "500", "700"))


# clean up
rm(dat27d51984)
rm(datd294e201)

# [END]
