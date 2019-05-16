# clusteringSampleSolutions-ShowPlot.R
#
# Purpose: show the most sigificant plot for the dimension reduction unit
# Version: 1.0
# Date:    2019  05
# Author:  Boris Steipe
#
# Input:   NA
# Output:  NA - plot as side-effect
# Dependencies: "./sampleSolutions/dat59e5a1c9.Rdata" must exist
#
# ToDo:
# Notes:
#
# ==============================================================================


# Prepare:
#
# set.seed(11133)
# dat950014dd <- tsne(crabs[,4:8],
#                     epoch_callback = tsnePlot,
#                     epoch = 10,
#                     perplexity = 160,
#                     max_iter = 200)
#
# dat4ac35b57 <- crabs[,1:2]
# datd3ca0aed <- pcaCrabs$x[ ,1]
# save(dat4ac35b57, datd3ca0aed, dat950014dd, file = "./sampleSolutions/dat59e5a1c9.Rdata")

load("./sampleSolutions/dat59e5a1c9.Rdata")

crabsPlot(dat950014dd[,1], dat950014dd[,2],
          dat4ac35b57[ ,1], dat4ac35b57[ ,2], datd3ca0aed,
          main = "Crabs TSNE")

# clean up
rm(dat4ac35b57)
rm(datd3ca0aed)
rm(dat950014dd)

# [END]
