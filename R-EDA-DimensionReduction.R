# R-EDA-DimensionReduction.R
# Purpose:
#     Introduction to dimension reduction in biological data with R
#
# Author:  Boris Steipe (boris.steipe@utoronto.ca)
#
# Version: 2
# Date:    2018 05
#
# Version history:
#        V 2.0  Restructuring 2018, remove model correlation, is now included
#               in the regression unit.
#        V 1.1  2017 version
#        V 1.0    First code 2016
#
# Note:        Some prior contributions by:
#              Raphael Gottardo, FHCRC
#              Sohrab Shah, UBC
#
# TODO:
#
#
# == HOW TO WORK WITH THIS FILE ================================================
#
#  Go through this script line by line to read and understand the
#  code. Execute code by typing <cmd><enter>. When nothing is
#  selected, that will execute the current line and move the cursor to
#  the next line. You can also select more than one line, e.g. to
#  execute a block of code, or less than one line, e.g. to execute
#  only the core of a nested expression.
#
#  DO NOT save edits to this file! Otherwise this will create a conflict
#  when we update it. If you do edit it, save it under a new file name like
#  myR-EDA-Introduction.R  Your "local" files will not be overwritten by
#  updates.
#
#  If there are portions you don't understand, use R's help system,
#  Google for an answer, or ask me. Don't continue if you don't
#  understand what's going on. That's not how it works ...
#
#  Once you have typed and executed the function init(), you will find a file
#  called myEDANotes.R in the project directory.
#
#  Open it, you can place all of your code-experiments and notes into that
#  file. This will be your "Lab Journal" for this session. Copy code to
#  myEDANotes.R to experiment with options, or just play.
#  Especially play.
#
# ==============================================================================
#
# Module 3: Dimension Reduction
#
# ==============================================================================


#TOC> ==========================================================================
#TOC> 
#TOC>   Section  Title                                     Line
#TOC> ---------------------------------------------------------
#TOC>   1        PCA introduction                            77
#TOC>   1.1        minimal PCA example                      108
#TOC>   1.2        prcomp() and princomp()                  173
#TOC>   1.3        Scaling                                  190
#TOC>   2        EDA with PCA                               207
#TOC>   2.1        The relative importance of PCs           209
#TOC>   3        EDA with PCA                               244
#TOC>   3.1        Load LPSdata                             248
#TOC>   3.2        Explore the principal components         268
#TOC>   3.3        Explore some similar genes               298
#TOC>   4        t-SNE                                      325
#TOC>   4.1        tsne of crabs data                       348
#TOC>   4.2        tsne of LPS data                         370
#TOC> 
#TOC> ==========================================================================


# =    1  PCA introduction  ====================================================


# The goal of Principal Component Analysis (PCA) is to transform a number of
# possibly correlated variables into a smaller number of uncorrelated variables
# called principal components.

# The (possibly) smaller number of variables can be used for data reduction and
# visualization.

# Principal component analysis (PCA) converts a set of observations of possibly
# correlated variables into a set of values of uncorrelated variables called
# principal components. The first principal component is the projection of the
# data into a single dimension that has as high a variance as possible (that is,
# accounts for as much of the variability in the data as possible); each
# succeeding component in turn has the highest variance possible under the
# constraint that it be orthogonal to (uncorrelated with) the preceding
# components. Therefore the PCs provide a view on the structure of the data that
# best explains its variance. This is especially useful for EDA of
# high-dimensional data that can't be intuitively visualized. Given a set of
# points in Euclidean space, the first principal component corresponds to a line
# that passes through the multidimensional mean and minimizes the sum of squares
# of the distances of the points from the line.

# The second principal component is calculated in the same way, after all
# correlation with the first principal component has been subtracted out from
# the points.

# Let's illustrate this with a simple 2D example:


# ==   1.1  minimal PCA example  ===============================================

# 500 normally distributed samples each: uncorrelated
set.seed(112358)
x1 <- rnorm(500,0,1)
y1 <- rnorm(500,0,1)

# generate y2 corrleated with (dependent on) x1
y2 <- 2*x1 + y1
mean(y2)
y2 <- y2-mean(y2)
mean(y2)
sd(y2)
y2 <- y2 / sd(y2)
sd(y2)
print(sd(y2), digits=22)


# After applying the PCA, and plotting the original and the projected
# coordinates ...

oPar <- par(mfrow = c(2,2)) # set new and save old graphics state

# four plots ...
hist(x1)
hist(y2)
plot(x1, y1)
plot(x1, y2)

par(oPar) # restore graphics state parameters

#... we indeed find that most of the variance is now contained in the first
#histogram. In a sense, the single dimension of the first principal component
#contains most of the information of our originally two-dimensional data.


# calculate a PCA of x1 and y2
pcaSample <- prcomp(cbind(x1,y2))

# here are the information items from the returned list of results
pcaSample
pcaSample$sdev
pcaSample$rotation
summary(pcaSample)
head(pcaSample$x)
plot(pcaSample$x, xlim=c(-5,5), ylim=c(-5,5))

# Compare the histograms before and after the rotation:
oPar <- par(mfrow = c(2,2))
hist(x1, xlim=c(-4,4), ylim=c(0,150), main="")
hist(y2, xlim=c(-4,4), ylim=c(0,150), main="")
hist(pcaSample$x[,1], xlim=c(-4,4), ylim=c(0,150),
     main="", col=rgb(0.86,0,0,0.5))
hist(pcaSample$x[,2], xlim=c(-4,4), ylim=c(0,150),
     main="", col=rgb(0.31, 0.5, 0.74, 0.5))
par(oPar) # restore graphics state parameters

# Plot the sample along the Principal Components as axes
plot(pcaSample$x[,1],pcaSample$x[,2], xlim=c(-4,4), ylim=c(-4,4))

objectInfo(pcaSample)

?prcomp


# ==   1.2  prcomp() and princomp()  ===========================================

# R has two different functions for PCA: prcomp() and princomp(). They use
# different mathematical approaches but the results are virtually identical.
# prcomp() is numerically more stable. However, they also use different names
# for the elements of their result lists.

#  prcomp()  princomp()
#
#  center    center	  The vector that was subtracted to center the data
#  sdev      sdev     Standard deviations for each dimension of the rotated data
#  rotation  loadings The actual principal components
#  x         scores   The rotated data, i.e. after projection along each PC

# e.g. use data$x for the rotated results of a prcomp() call, but use
# data$scores if the result came from princomp()

# ==   1.3  Scaling  ===========================================================
#
# PCA is sensitive to the scaling of the variables.

# If we have just two variables and they have the same sample variance and are
# positively correlated, then the PCA will entail a rotation by 45° and the
# "loadings" for the two variables with respect to the principal component will
# be equal. But if we multiply all values of the first variable by 100, then the
# principal component will be almost the same as that variable, with a small
# contribution from the other variable, whereas the second component will be
# almost aligned with the second original variable. This means that whenever the
# different variables have different units (like temperature and mass), PCA is a
# somewhat arbitrary method of analysis. (Different results would be obtained if
# one used Fahrenheit rather than Celsius for example.) One way to address this
# is to scale variables to have unit variance.


# =    2  EDA with PCA  ========================================================

# ==   2.1  The relative importance of PCs  ====================================

# load one of the sample data sets in the R distribution: The crabs data frame
# has 200 rows and 8 columns, describing 5 morphological measurements on 50
# crabs each of two colour forms and both sexes, of the species Leptograpsus
# variegatus collected at Fremantle, W. Australia.

library(MASS)
data(crabs)

head(crabs)
# Two types: blue and orange
# Two genders: female and male
# FL frontal lobe size (mm)
# RW rear width (mm)
# CL carapace length (mm)
# CW carapace width (mm)
# BD body depth (mm)

# annotate...
fac <- as.factor(paste(crabs[, 1], crabs[, 2],sep="."))
head(fac)
c(fac[1], fac[51], fac[101], fac[151])
as.numeric(c(fac[1], fac[51], fac[101], fac[151]))

plot(crabs[, 4:8], pch=as.numeric(fac))
plot(crabs[, 4:5], pch=as.numeric(fac))
plot(crabs[, 5:6], pch=as.numeric(fac))


# TASK: Apply PCA to the crabs dataset to distinguish species and sex
#       from morphometric measurements.



# =    3  EDA with PCA  ========================================================

# Exploring the structure of datasets

# ==   3.1  Load LPSdata  ======================================================

load(file="LPSdat.RData")

dat <- LPSdat[ , - c(1, 16)]
boxplot(dat)



set.seed(112358)
Sel <- sample(1:nrow(dat), 10)

matplot(t(dat[Sel, ]),
        type="b", lwd=2, col=cm.colors(10),
        main = "Random selection",
        ylab = "log(expression)",
        xlab = "Conditions")



# ==   3.2  Explore the principal components  ==================================

# calculate the PCS
pcaLPS <- prcomp(dat)
plot(pcaLPS)

# Explore the correlations along the first few principal components

set.seed((112358))
sel <- sample(1:nrow(pcaLPS$x), 200)


plot(pcaLPS$x[ , 1], pcaLPS$x[ , 2], pch = 16, cex = 0.6, col="#0066EE33")
plot(pcaLPS$x[ , 1], pcaLPS$x[ , 3], pch = 16, cex = 0.6, col="#0066EE33")
plot(pcaLPS$x[ , 2], pcaLPS$x[ , 3], pch = 16, cex = 0.6, col="#0066EE33")
plot(pcaLPS$x[ , 2], pcaLPS$x[ , 4], pch = 16, cex = 0.6, col="#0066EE33")
plot(pcaLPS$x[ , 3], pcaLPS$x[ , 4], pch = 16, cex = 0.6, col="#0066EE33")


# Examine the actual principal components in a parallel-coordinates
# plot. If these data are expression profiles, we call
# these the "eigengenes", with reference to the "eigenvalues" of
# linear algebra.
N <- 6
matplot(pcaLPS$rotation[, 1:N],
        type="b", lwd=3,
        xlab = "condition", ylab="PCs")



# ==   3.3  Explore some similar genes  ========================================

plot(pcaLPS$x[ , 2], pcaLPS$x[ , 3], type = "n")
text(pcaLPS$x[ , 2], pcaLPS$x[ , 3], cex = 0.5)

Sel1 <- c(1331, 1339, 1338)
text(pcaLPS$x[Sel1 , 2], pcaLPS$x[Sel1 , 3],
     labels = Sel1, cex = 0.5, col = "#AA0000")

matplot(t(dat[Sel1, ]),
        type = "b", lwd = 3, col = "#AA0000",
        main = "Clustered Genes - Set 1",
        ylab = "log expression levels",
        xlab = "categories")

Sel2 <- c(888, 705, 890, 889, 884, 886, 887)
text(pcaLPS$x[Sel2 , 2], pcaLPS$x[Sel2 , 3],
     labels = Sel2, cex = 0.5, col = "#0099AA")

matplot(t(dat[Sel2, ]),
        type = "b", lwd = 3, col = "#0099AA",
        main = "Clustered Genes - Set 2",
        ylab = "log expression levels",
        xlab = "categories")



# =    4  t-SNE  ===============================================================

# t-Stochastic Neighbour Embedding is a powerful dimension re-
# duction algorithm developed in the lab of Geoff Hinton at UofT.
#
# see: https://en.wikipedia.org/wiki/T-distributed_stochastic_neighbor_embedding
#
# Its implementation for flow cytometry - viSNE -is available
# from the Dana Pe'er lab (as Matlab code).
# http://www.c2b2.columbia.edu/danapeerlab/html/cyt.html
# It is the basis for a very powerful flow-cytometry exploration tool:
# Amir et al. (2013) Nature Biotechnology, doi:10.1038/nbt.2594
#
# Below we will try the t-SNE algorithm for exploration of
# some of the data we have looked at before.

if (!require(tsne, quietly=TRUE)) {
  install.packages("tsne")
  library(tsne)
}
?tsne


# ==   4.1  tsne of crabs data  ================================================


# First: define a plotting function
tsnePlot <- function(x) {
  crabsPlot(x[,1], x[,2],
            crabs[ ,1], crabs[ ,2], pcaCrabs$x[ ,1],
            main = "Crabs TSNE")
}

# make the run reproducible
set.seed(208)

# run tsne
tsneCrabs <- tsne(crabs[,4:8],
                  epoch_callback = tsnePlot,
                  perplexity=70,
                  max_iter = 500)




# ==   4.2  tsne of LPS data  ==================================================


ecb <- function(x){
  plot(x, pch = 16, cex = 0.6, col="#0066EE33")
  points(x[Sel1, ], pch=16, cex=0.7, col="#AA0000")
  points(x[Sel2, ], pch=16, cex=0.7, col="#0099AA")
#  text(x, labels=paste(1:dim(cho.data)[1]), cex=0.7)
}

set.seed(112358)
tsneLPS <- tsne(dat,
                epoch_callback = ecb,
                perplexity = 50,
                max_iter = 500)

# Pick a cluster of genes in the vicinity of the originally
# selected values

str(tsneLPS)
plot(tsneLPS[ , ], type = "n")
points(tsneLPS[ Sel1, ], pch=16, cex=0.7, col="#AA0000")
points(tsneLPS[ Sel2, ], pch=16, cex=0.7, col="#0099AA")
text(tsneLPS[ , ], cex = 0.4)


Sel3 <- c(1333, 1341, 1337, 1335)
matplot(t(dat[Sel1, ]),
        type = "b", lwd = 3, col = "#AA0000",
        main = "Clustered Genes - Set 1",
        ylab = "log expression levels",
        xlab = "categories")
for (i in 1:length(Sel3)) {
  lines(t(dat[Sel3[i], ]), type="l", lwd=3, col = "#BB99EE")
}



# [End]
