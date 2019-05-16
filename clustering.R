# Rclustering.R
#
# Purpose:  Introduction to clustering - clustering of expression data
#
# Version: 3.0
#
# Date:    2019  05
# Author:  Boris Steipe (boris.steipe@utoronto.ca)
#
# V 3.0    Reconceived for 2019 workshop, use LPS data
# V 2.1    Use 2 - correlation distance metric
# V 2      2018 restructuring
# V 1.1    2017 updates
# V 1.0    First code 2016
#
# TODO:
#   - Plot heatmap as "standard" red/green heatmap view with annotation bars
#
# == HOW TO WORK WITH THIS FILE ================================================
#
#  This file contains scenarios and tasks, we will discuss them in detail in
#  class. If this file is named "myEDAintroduction.R", then edit it profusely,
#  write code, experiment with options, or just play.
#  Especially play.
#
#  If there is anything you don't understand, use R's help system,
#  Google for an answer, or ask. Especially ask. Don't continue if you don't
#  understand what's going on. That's not how it works ...
#
# ==============================================================================
#
#     C L U S T E R I N G
#
# ==============================================================================


#TOC> ==========================================================================
#TOC> 
#TOC>   Section  Title                                    Line
#TOC> --------------------------------------------------------
#TOC>   1        LOAD THE DATA                              66
#TOC>   2        HEATMAPS                                   92
#TOC>   3        HIERARCHICAL CLUSTERING                   141
#TOC>   3.1        Explorig distance metrics               167
#TOC>   3.2        Clusters From Dendrograms               221
#TOC>   3.2.1          Setting Cut-Levels Dynamically      298
#TOC>   4        PARTITIONING CLUSTERING                   373
#TOC>   4.1        K-means                                 376
#TOC>   4.2        K-medoids                               396
#TOC>   5        AFFINITY PROPAGATION CLUSTERING           411
#TOC>   6        CLUSTER QUALITY METRICS                   439
#TOC> 
#TOC> ==========================================================================


updateTOC()   # <<<--- Execute this to update the TOC


# In this unit we will explore clustering. Our exploration will
# discover groups of genes with related properties.

# source("./sampleSolutions/clusteringSampleSolutions-ShowPlot.R")



# =    1  LOAD THE DATA  =======================================================

# We will attempt to find groups in our LPS expression data, using clustering
# methods.
load("./data/LPSdat.RData")

# To select genes that actually are interesting, we'll select the genes
# with the top 250 standard deviations of expression values:

mySD <- apply(LPSdat[ , 2:15], 1, sd)
hist(mySD)
sel <- order(mySD, decreasing = TRUE)[1:250]
mySD[sel[1]]
mySD[sel[length(sel)]]

# It's convenient for clustering to use data objects that are simple
# numeric matrices
dat <- matrix(numeric(250 * 14), nrow = 250)
for (i in 1:length(sel)) {
  dat[i, ] <- as.numeric(LPSdat[sel[i], 2:15])
}
rownames(dat) <- LPSdat$genes[sel]
colnames(dat) <- colnames(LPSdat[2:15])
str(dat)


# =    2  HEATMAPS  ============================================================

# Heatmaps are a staple of gene expression analysis.
# You can tweak many of the parameters, but for a first look
# we'll just heatmap the data with default parameters.

# This is a standard view that can be applied to all manners
# of multidimensional data, not just genes.
heatmap(dat)

# Just for illustration and readability let's map only
# every fifth gene
heatmap(dat[seq(1, nrow(dat), by=5), ])

# What's the actual range of values?
range(dat[,1])

# TODO:
# TASK: In biology we are usually looking at red/black/green heatmaps ...
# what are these colours? Can we colour our data like that?
#
# Hint: https://www.r-graph-gallery.com/215-the-heatmap-function/



# Study the heatmap, and consider what it tells you.
# For example, there seem to be genes that are low at LPS
# but high control ...
set1 <- c("Cd274", "Cd69", "Daxx", "Zbp1", "Usp18")
# ... and there are genes for which the inverse is true:
set2 <- c("Gm2a", "Tmem176b", "Ctsh", "Alox5ap")

# We can use a "parallel coordinates" plot - matplot()
# to look at the actual expression levels. Note that
# matplot expects the values column-wise ordered, thus
# we have to transpose - t() - the data!
matplot(t(dat[set1, ]),
        type="l", lwd=2, col="skyblue", lty=1,
        xlab="conditions", ylab="log expression value")

# Then we can use lines() to superimpose the genes for set2.
# No transpose here :-)
for (i in 1:length(set2)) {
    lines(dat[set2[i], ], type="l", lwd=2, col="firebrick")
}

# Indeed, these genes - visibly different in the heatmap
# have similar expression profiles.

# =    3  HIERARCHICAL CLUSTERING  =============================================


# Hierarchical clustering is probably the most basic technique.
# The dendrograms on the rows and columns of the heatmap
# were created by hierarchical clustering.

# For hierarchical clustering, first we need to produce
# a distance table. There are many ways to define distances
# let's just go with the default: "Euclidian distance".
distDat <-dist(dat)

# Then we use the clustering distance matrix to produce a
# dendrogram in which the most similar genes are connected, and then
# similar genes or connected groups are added. There are
# several ways to define "most-similar", lets just go with the
# default for now: "complete linkage" hierarchical clustering
hc <- hclust(distDat)

plot(hc)

# Not bad. But do note that  distance as well as clustering
# method matter, and there is not really a "best" way that
# works for all data. You'll need to explore: what you are looking for
# is a distance metric that gives the clearest block structure.

# ==   3.1  Explorig distance metrics  =========================================

dEu <- function(x) dist(x, method="euclidian")
heatmap(dat, distfun = dEu)

dCan <- function(x) dist(x, method="canberra")
heatmap(dat, distfun = dCan)

dMax <- function(x) dist(x, method="maximum")
heatmap(dat, distfun = dMax)

dMink <- function(x) dist(x, method="minkowski")
heatmap(dat, distfun = dMink)

# You are not confined to the default distance functions, it
# is quite straightforward to define your own, for example
# using correlation properties. Here is a distance function
# defined as 1 - abs(pearson correlation)...

dCorAbs <- function(x) as.dist(1 - abs(cor(t(x))))
heatmap(dat, distfun = dCorAbs)

# ... which is useful if you are interested in similarity of shape, regardless
# of the absolute value, or ...

dCor <- function(x) as.dist(2 - cor(t(x)))
heatmap(dat, distfun = dCor)

# ... calculating the entire range of the possible correlations, i.e.
# differentiating between correlated and anticorrelated samples.

# In the case of our LPS dataset it does indeed seem that the
# dCor function (2 minus correlation) gives us the clearest block-structure.

# Let's use the function to produce a distance matrix:
# A distance matrix is simply a square matrix of elements, which contains the
# distance between them in the cells

N <- nrow(dat)
myDist <- matrix(numeric(N*N), nrow = N)
rownames(myDist) <- rownames(dat)
colnames(myDist) <- rownames(dat)

for (i in 1:N) {
  for (j in i:N){
    d <- 2 - cor(dat[i, ], dat[j, ])
    myDist[i, j] <- d
    myDist[j, i] <- d
  }
}
myDist <- as.dist(myDist)
hc <- hclust(myDist)
plot(hc)

# ==   3.2  Clusters From Dendrograms  =========================================

# To get clusters from a dendrogram, we need to "cut" it at some
# level. The tree then falls apart into sub-trees and each of these
# is one "cluster"...

# Draw rectangles at different cut-levels, to give the desired number
# of clusters.
rect.hclust(hc, k = 2)
rect.hclust(hc, k = 5)
rect.hclust(hc, k = 10)
rect.hclust(hc, k = 20)
rect.hclust(hc, k = 50)

# Now retrieve the actual indices and use them to generate
# parallel coordinate plots.

class <- cutree(hc, k = 20)

# Explain the output...
class

# The table() function allows us to count the number of
# occurences in each class ...
table(class)
sort(table(class), decreasing = TRUE)

# Let's plot the four largest classes (in parallel, into the same window)
# Look at this carefully. See how the selection statement on the object "class"
# generates a logical vector: TRUE in all rows for which the statement is true,
# and how this is used to select the rows of dat that we want to plot ...

oPar <- par(mfrow=c(2,2))
matplot(t(dat[class==1,]), type="l", xlab="time", ylab="log expression value")
matplot(t(dat[class==6,]), type="l", xlab="time", ylab="log expression value")
matplot(t(dat[class==5,]), type="l", xlab="time", ylab="log expression value")
matplot(t(dat[class==8,]), type="l", xlab="time", ylab="log expression value")
par(oPar)


# As an alternative, try Wards- linkage clustering (and read up on the
# options: single-, complete- and average-linkage clustering)
hc.ward <-hclust(distDat, method = "ward.D", members=NULL)

plot(hc.ward)


# draw rectangles
rect.hclust(hc.ward,k=9)

# This looks reasonable ...
# Now retrieve the actual indices and use them to generate
# paralell coordinate plots.

class.ward<-cutree(hc.ward, k = 9)
sort(table(class.ward))

# get some nice colors
if (!require(RColorBrewer, quietly=TRUE)) {
    install.packages("RColorBrewer")
    library(RColorBrewer)
}

# what spectra are there in the package .. ?
display.brewer.all()

niceCols <- brewer.pal(9, "Paired")

oPar <- par(mfrow=c(3,3))
for (i in 1:9) {
    matplot(t(dat[class == i,]),
            type="l", col=niceCols[i],
            xlab="time",ylab="log expression value")
}
par(oPar)


# ===   3.2.1  Setting Cut-Levels Dynamically 

# While this may be aesthetically somewhat satisfactory, it is clear that
# the clusters are not homogenous as we might need them for biological
# interpretation. This is a general problem with clustering methods that
# fix the number of cluster centres either directly as in Kmeans (see
# below), or indirectly by cutting trees at a fixed level. It is also
# a problem with the data, where differences in absolute values might
# override separation into clusters that might better be defined in terms
# of relative values. Rather than "cutting" at a fixed level, we could
# adjust the level dynamically according to some objective function that
# may give us "better" clusters.

# Here is a package that adresses the dynamic range problem.
# Read about it here:
# http://cran.r-project.org/web/packages/dynamicTreeCut/dynamicTreeCut.pdf
if (!require(dynamicTreeCut, quietly=TRUE)) {
    install.packages("dynamicTreeCut")
    library(dynamicTreeCut)
}

hc <- hclust(myDist)   # recreate hc
class.dynamic <- cutreeDynamic(dendro = hc,
                               distM = as.matrix(myDist),
                               cutHeight=100)
table(class.dynamic)

niceCols <- brewer.pal(7, "Spectral")


oPar <- par(mfrow=c(3,3))
for (i in 1:7) {
    matplot(t(dat[class.dynamic == i,]),
            type="l",
            col=niceCols[i],
            xlab="time",
            ylab="log expression value")
}
par(oPar)

# Note that the number of members in each class is now much more homogenous,
# while the internal groups do not seem at lot more noisy.

# One thing our clustering algorithms do is to pull apart profiles
# that have similar shape, but different absolute levels. This is
# because we have not normalized our data. Let's thus try
# clustering merely based on profile shape, i.e.
# relative expression levels, by scaling all rows between zero
# and one.

datNorm <- t(apply(dat, 1, function(x)(x-min(x))/(max(x)-min(x))))
distDatNorm <- dist(datNorm)

hc.Norm <-hclust(distDatNorm)

class.dynamic <- cutreeDynamic(dendro = hc.Norm,
                               distM = as.matrix(distDatNorm),
                               cutHeight=15)

table(class.dynamic)

niceCols <- brewer.pal(8, "Spectral")

oPar <- par(mfrow=c(3,3))
for (i in 1:8) {
    matplot(t(datNorm[class.dynamic == i,]),
            type="l",
            col=niceCols[i],
            xlab="time",
            ylab="log expression value")
}
par(oPar)



# =    4  PARTITIONING CLUSTERING  =============================================


# ==   4.1  K-means  ===========================================================

# K-means clusters by assigning elements to a fixed
# number of cluster centres, so that similarity
# within a cluster is maximized.

?kmeans

k <- 4
cl <- kmeans(dat, k)

niceCols <- brewer.pal(k, "Spectral")

plot(dat[,1], dat[,4], col = niceCols[cl$cluster])
points(cl$centers, col = niceCols[1:k], pch = 8, cex=2)

# But: be aware ...
# ... K-means does not guarantee a globally optimal solution,
# merely a locally converged one.

# ==   4.2  K-medoids  =========================================================

# load library "cluster" for K-medoid partitioning
if (!require(cluster, quietly=TRUE)) {
    install.packages("cluster")
    library(cluster)
}

set.seed(112358)
k <- 4
cl<-pam(dat, 4)
plot(dat[, 1],dat[, 4], col=niceCols[cl$cluster])
plot(cl) # shows boundary and silhouette plots


# =    5  AFFINITY PROPAGATION CLUSTERING  =====================================


# Based on B. J. Frey and D. Dueck. Clustering by
# passing messages between data points.
# Science, 315(5814):972–976, 2007

if (!require(apcluster, quietly=TRUE)) {
    install.packages("apcluster")
    library(apcluster)
}

apRes <- apcluster(negDistMat(r=2), dat)
apRes

heatmap(apRes)
length(apRes)
cutree(apRes)

oPar <- par(mfrow=c(3,3))
for (i in 1:9) {
  matplot(t(dat[unlist(apRes[i]),]),
          type="l",xlab="time",ylab="log expression value")
}
par(oPar)



# =    6  CLUSTER QUALITY METRICS  =============================================


# So .. which method should we use?
# I don't think that there is an obvious biological
# criterium to decide. Typically we should take some
# orthogonal information (e.g. shared transcription
# factor binding site), so if functionally related
# genes end up in similar clusters, and judge our
# cluster success based on its biological predictive
# value.
#
# But we can certainly say something about whether
# our clusters look good in a mathematical sense.
if (!require(clValid, quietly=TRUE)) {
    install.packages("clValid")
    library(clValid)
}
# Package information:
#  library(help =   clValid)     # basic information
#  browseVignettes("clValid")    # available vignettes
#  data(package =  "clValid")    # available datasets

if (!require(kohonen, quietly=TRUE)) {
    install.packages("kohonen")
    library(kohonen)
}
# Package information:
#  library(help =   kohonen)     # basic information
#  browseVignettes("kohonen")    # available vignettes
#  data(package =  "kohonen")    # available datasets

if (!require(mclust, quietly=TRUE)) {
    install.packages("mclust")
    library(mclust)
}
# Package information:
#  library(help =   mclust)     # basic information
#  browseVignettes("mclust")    # available vignettes
#  data(package =  "mclust")    # available datasets

?clValid

# This is pretty nice: we _can_ use biological
# knowledge for validation...
# But for our example, we'll try internal validation
# on all available methods.

valClust <- clValid(dat,
                    nClust = 2:20,
                    clMethods = c("hierarchical",
                                  "kmeans",
                                  "diana",
                                  "model",
                                  "sota",
                                  "pam",
                                  "clara",
                                  "agnes"),
                    validation = "internal")
summary(valClust)
plot(valClust)

vignette("clValid")

# Task:
# 1 - What appears to be the best clustering method?
# 2 - How can you actually apply it to the data?


# [End]
