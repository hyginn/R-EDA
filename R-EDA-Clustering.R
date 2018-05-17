# R-EDA-Clustering.R
#
# Purpose:  Introduction to clustering - clustering of expression data
#
# Version: 2
#
# Date:    2018  05
# Author:  Boris Steipe (boris.steipe@utoronto.ca)
#
# V 2      2018 restructuring
# V 1.1    2017 updates
# V 1.0    First code 2016
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
# Module 4: Clustering
#
# ==============================================================================


#TOC> ==========================================================================
#TOC> 
#TOC>   Section  Title                                           Line
#TOC> ---------------------------------------------------------------
#TOC>   1        LOAD DATA FROM GEO                                79
#TOC>   1.1        Exploring the data                             192
#TOC>   2        PREPARE DATA FOR ANALYSIS                        214
#TOC>   3        HEATMAPS                                         281
#TOC>   4        HIERARCHICAL CLUSTERING                          325
#TOC>   4.1        Explorig distance metrics                      351
#TOC>   4.2        Clusters From Dendrograms                      391
#TOC>   4.2.1          Setting Cut-Levels Dynamically             490
#TOC>   5        PARTITIONING CLUSTERING                          561
#TOC>   5.1        K-means                                        564
#TOC>   5.2        K-medoids                                      584
#TOC>   6        AFFINITY PROPAGATION CLUSTERING                  599
#TOC>   7        CLUSTER QUALITY METRICS                          638
#TOC>   8        T-STOCHASTIC NEIGBOUR EMBEDDING                  692
#TOC>   8.1        Digression: color scales                       749
#TOC>   8.2        Colouring tsne results                         788
#TOC>   8.3        Selecting from tsne results                    811
#TOC>   8.4        Interactive graphics with tsne results         871
#TOC>   8.5        A Polygon selection                            943
#TOC>   8.6        tsne 3D embedding                             1024
#TOC>   9        MORE PRACTICE                                   1062
#TOC> 
#TOC> ==========================================================================


# =    1  LOAD DATA FROM GEO  ==================================================
#
# GEO is the major repository of gene expression data.
# Let's find some cell-cycle data in GEO, for clustering.
# The goal is to identify coregulated genes, but we don't
# know what their response to time in the cell-cycle will
# be. Going up? Going down?

# The first part of code is slightly adapted from
# performing a standard GEO2R analyis on the NCBI website for
# "Cell cycle expression profiles in HeLa cells" (GSE26922)
# see: http://www.ncbi.nlm.nih.gov/geo/query/acc.cgi?acc=GSE26922
# The dataset contains triplicate measurements for t0 (blocked) and
# t = 2,4,6,8 and 12h post block-release.

# First, we need to install some analysis packages from bioconductor
# The following commands do for the bioconductor system what
# install.packages() does for CRAN.

source("http://bioconductor.org/biocLite.R")
biocLite("Biobase")
biocLite("GEOquery")
biocLite("limma")

# Then we load the libraries....
library(Biobase)
library(GEOquery)
library(limma)

# Then we try loading series and platform data from GEO ... that's sometimes
# a bit unpredictable, if this doesn't work, skip ahead and load the
# data set I have placed in the data directory instead.

gset <- getGEO("GSE26922", GSEMatrix =TRUE)

if (length(gset) > 1) {
    idx <- grep("GPL6244", attr(gset, "names"))
} else {
    idx <- 1
}
gset <- gset[[idx]]

# If this doesn't work without errors, use thie fallback dataset instead to
# greate the "gset" object:
#
# load("./data/GSE26922.RData")




# Check what we have

head(gset)
str(gset)


# The code below is pretty much verbatim GEO2R ...
# ===== (without detailed explanation) ===========
# Make proper column names to match toptable
fvarLabels(gset) <- make.names(fvarLabels(gset))

# Group names for all samples
sml <- c("G0","G0","G0","G1","G1","G1",
         "G2","G2","G2","G3","G3","G3",
         "G4","G4","G4","G5","G5","G5");

# log2 transform
ex <- exprs(gset)
qx <- as.numeric(quantile(ex, c(0., 0.25, 0.5, 0.75, 0.99, 1.0), na.rm=T))
LogC <- (qx[5] > 100) ||
    (qx[6]-qx[1] > 50 && qx[2] > 0) ||
    (qx[2] > 0 && qx[2] < 1 && qx[4] > 1 && qx[4] < 2)
if (LogC) {
    ex[which(ex <= 0)] <- NaN
    exprs(gset) <- log2(ex)
}

# Set up the data and proceed with analysis
fl <- as.factor(sml)
gset$description <- fl
design <- model.matrix(~ description + 0, gset)
colnames(design) <- levels(fl)
fit <- lmFit(gset, design)
cont.matrix <- makeContrasts(G5-G0, G1-G0, G2-G1,
                             G3-G2, G4-G3, G5-G4,
                             levels=design)
fit2 <- contrasts.fit(fit, cont.matrix)
fit2 <- eBayes(fit2, 0.01)
tT <- topTable(fit2, adjust="fdr", sort.by="B", number=250)

# load NCBI platform annotation
gpl <- annotation(gset)
platf <- getGEO(gpl, AnnotGPL=TRUE)
ncbifd <- data.frame(attr(dataTable(platf), "table"))

# replace original platform annotation
tT <- tT[setdiff(colnames(tT), setdiff(fvarLabels(gset), "ID"))]
tT <- merge(tT, ncbifd, by="ID")
tT <- tT[order(tT$P.Value), ]  # restore correct order

tT <- subset(tT, select=c("ID","adj.P.Val","P.Value",
                          "F","Gene.symbol","Gene.title"))


# so far, the GEO2R code ...

# It has returned to us the 250 top-differentially expressed
# genes across the groups we have defined. Note though, that
# the statitistics here implicitly treat the datapoints as
# independent, not as time-series. However for our purpose
# of demonstrating clustering methods this is fine.


# ==   1.1  Exploring the data  ================================================
#
# Let's familiarize ourselves a bit with the structure
# of the data.

head(tT)

# The top gene has the ID 8117594: what are the original values?

exprs(gset)["8117594",]
barplot(exprs(gset)["8117594",])

exprs(gset)["7919642", "GSM662899"]
exprs(gset)["7919642", 5]


# Note how we use a string constant to get a data row from the table
# of expression values. We can also use a vector - the expression below
# returns the data rows for the top three differentially expressed genes:
exprs(gset)[c("8117594","7900167", "8151871"),]


# =    2  PREPARE DATA FOR ANALYSIS  ===========================================


# For cluster analysis, it's useful to make a table from
# these data that contains only numbers, and just a single
# value (mean) for the biological replicates.

gSym <- tT$Gene.symbol

dat <- c()
for (i in 1:nrow(tT)) {
    v <- c()
    v  <- c(v,  mean(exprs(gset)[tT$ID[i], 1:3]))  # t = 0
    v  <- c(v,  mean(exprs(gset)[tT$ID[i], 4:6]))  # t = 2 hours
    v  <- c(v,  mean(exprs(gset)[tT$ID[i], 7:9]))  # etc...
    v  <- c(v,  mean(exprs(gset)[tT$ID[i], 10:12]))
    v  <- c(v,  mean(exprs(gset)[tT$ID[i], 13:15]))
    v  <- c(v,  mean(exprs(gset)[tT$ID[i], 16:18]))
    dat <- rbind(dat, v)
}
colnames(dat) <- c("t0", "t2", "t4", "t6", "t8", "t12")

# We could use the IDs as rownames, like so ...
#    rownames(dat) <- tT$ID
# ... or the gene symbols, since the IDs don't really
# tell us anything useful. But: are the gene symbols unique?
# If they are not unique, we'll have all sorts of trouble later
# on when we select by rowname...
# R has the function duplicated() to find repeated values
# in a vector...
as.character(gSym[duplicated(gSym)])

# Ha! There are eleven symbols that reappear. Some of them  are
# formatted like "FAM72A///FAM72D///FAM72B" which may mean that
# a spot on the microarray doesn't distinguish between three
# isoforms ... and some are simply the empty string "".
# Since duplicated() gives us a convenient logical vector to
# identify them, we can simply remove them. This is good enough
# for our clustering exercise, for "real" work we should go back
# to the platform information, find out why there are duplicated
# gene symbols, and address this issue.
dat <- dat[!duplicated(gSym), ]
rownames(dat) <- gSym[!duplicated(gSym)]

# We'll also remove all rows that have spots for isoforms.
dat <- dat[-(grep("/", rownames(dat))), ]

# This completes the creation of our expression dataset for clustering.

# You could store the data in a local file ...
write.csv(dat, file="GSE26922.dat")

# and then read it back in like so...
dat <- as.matrix(read.csv(file="GSE26922.dat",
                          row.names = 1,
                          header=TRUE))

# ... or, you could save the object as a binary object
# using the function saveRDS(). Then you can read it
# back in with readRDS(). Note that you can change the
# name when you read it back.
saveRDS(dat,  file="GSE26922.rds")

dat <- readRDS("GSE26922.rds")
identical(dat, dat2)  # has to be TRUE !


# =    3  HEATMAPS  ============================================================

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



# Study the heatmap, and consider what it tells you.
# For example, there seem to be genes that are low at t4 and t6
# but high at t0, t2 ...
set1 <- c("FNIP1", "MED13L", "NRIP1", "MSI2", "ZNFX1")
# ... and there are genes for which the inverse is true:
set2 <- c("FBXL20", "CCNE1", "ZBTB14", "HIST1H2AH")

# We can use a "parallel coordinates" plot - matplot()
# to look at the actual expression levels. Note that
# matplot expects the values column-wise ordered, thus
# we have to transpose - t() - the data!
matplot(t(dat[set1,]),
        type="l", lwd=2, col="skyblue", lty=1,
        ylim=c(8,14), xlab="time", ylab="log expression value")

# Then we can use lines() to superimpose the genes for set2.
# No transpose here :-)
for (i in 1:length(set2)) {
    lines(dat[set2[i], ], type="l", lwd=2, col="firebrick")
}

# Indeed, these genes - visibly different in the heatmap
# have similar expression profiles profiles within sets and different
# profiles between sets.

# =    4  HIERARCHICAL CLUSTERING  =============================================


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

# ==   4.1  Explorig distance metrics  =========================================

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

dCor <- function(x) as.dist(1 - abs(cor(t(x))))
heatmap(dat, distfun = dCor)

# ... and a similar one with the maximum information
# coefficient (MIC) as implemented in the minerva
# package
if (!require(minerva, quietly=TRUE)) {
    install.packages("minerva")
    library(minerva)
}
?mine

dMIC <- function(x) as.dist(mine(t(x))$MIC)
heatmap(dat, distfun = dMIC)

# I would choose dMax, but the differences are not
# really obvious ...

hc <- hclust(dMax(dat))


# ==   4.2  Clusters From Dendrograms  =========================================

# Back to our original dendrogram ...
plot(hc)

# To get clusters from a dendrogram, we need to "cut" it at some
# level. The tree then falls apart into sub-trees and each of these
# is one "cluster"...

# Draw rectangles at different cut-levels, to give the desired number
# of clusters.
rect.hclust(hc,k=2)
rect.hclust(hc,k=5)
rect.hclust(hc,k=10)
rect.hclust(hc,k=20)
rect.hclust(hc,k=50)

# Now retrieve the actual indices and use them to generate
# parallel coordinate plots.

class <-cutree(hc, k = 20)

# Explain the output...
class

# The table() function allows us to count the number of
# occurences in each class ...
table(class)
sort(table(class))

# Let's plot the four largest classes (in parallel, into the same window)
# Look at this carefully. See how the selection statement on class
# generates a logical vector: TRUE in all rows for which the statement is true,
# and how this is used to select the rows of dat that we want to plot ...

oPar <- par(mfrow=c(2,2))
matplot(t(dat[class==14,]),type="l", xlab="time",ylab="log expression value")
matplot(t(dat[class==2,]),type="l", xlab="time",ylab="log expression value")
matplot(t(dat[class==10,]),type="l", xlab="time",ylab="log expression value")
matplot(t(dat[class==4,]),type="l", xlab="time",ylab="log expression value")
par(oPar)


# As an alternative, try Wards- linkage clustering (and read up on the
# options: single-, complete- and average-linkage clustering)
hc.ward <-hclust(distDat, method = "ward", members=NULL)

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
    matplot(t(dat[class.ward == i,]),
            type="l", col=niceCols[i],
            xlab="time",ylab="log expression value")
}
par(oPar)

# trying the same with 1-cor(dat)

dCor <- as.dist((1 - cor(t(dat)))/2)
hCor <- hclust(dCor, method = "ward")
plot(hCor)


classCor <-cutree(hCor, k = 9)
sort(table(classCor))

niceCols <- brewer.pal(9, "Paired")

oPar <- par(mfrow=c(3,3))
for (i in 1:9) {
    matplot(t(dat[classCor == i,]),
            type="l", col=niceCols[i],
            xlab="time",ylab="log expression value")
}
par(oPar)


# ===   4.2.1  Setting Cut-Levels Dynamically        

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

class.dynamic <- cutreeDynamic(dendro = hc.ward,
                               distM = as.matrix(distDat),
                               cutHeight=100)

niceCols <- brewer.pal(8, "Spectral")

oPar <- par(mfrow=c(2,3))
for (i in 1:6) {
    matplot(t(dat[class.dynamic == i,]),
            type="l",
            col=niceCols[i],
            xlab="time",
            ylab="log expression value")
}
par(oPar)

# One thing our clustering algorithms do is to pull apart profiles
# that have similar shape, but different absolute levels. This is
# because we have not normalized our data. Let's thus try
# clustering merely based on profile shape, i.e.
# relative expression levels, by scaling all rows between zero
# and one.

datNorm <- t(apply(dat, 1, function(x)(x-min(x))/(max(x)-min(x))))
distDatNorm <-dist(datNorm)

hc.Norm <-hclust(distDatNorm)

class.dynamic <- cutreeDynamic(dendro = hc.Norm,
                               distM = as.matrix(distDatNorm),
                               cutHeight=15)

niceCols <- brewer.pal(6, "Spectral")

oPar <- par(mfrow=c(3,2))
for (i in 1:6) {
    matplot(t(datNorm[class.dynamic == i,]),
            type="l",
            col=niceCols[i],
            xlab="time",
            ylab="log expression value")
}
par(oPar)

# With hierarchical clustering, this is probably as good
# as we can get - the clusters are of reasonable size -
# but from a biological point of view one would argue
# that several of them are not really different.


# =    5  PARTITIONING CLUSTERING  =============================================


# ==   5.1  K-means  ===========================================================

# K-means clusters by assigning elements to a fixed
# number of cluster centres, so that similarity
# within a cluster is maximized.

?kmeans

k <- 4
cl <- kmeans(dat, k)

niceCols <- brewer.pal(k, "Spectral")

plot(dat[,"t2"], dat[,"t6"], col = niceCols[cl$cluster])
points(cl$centers, col = niceCols[1:k], pch = 8, cex=2)

# But: be aware ...
# ... K-means does not guarantee a globally optimal solution,
# merely a locally converged one.

# ==   5.2  K-medoids  =========================================================

# load library "cluster" for K-medoid partitioning
if (!require(cluster, quietly=TRUE)) {
    install.packages("cluster")
    library(cluster)
}

set.seed(112358)
k <- 4
cl<-pam(dat, 4)
plot(dat[,"t2"],dat[,"t6"], col=niceCols[cl$cluster])
plot(cl) # shows boundary and silhouette plots


# =    6  AFFINITY PROPAGATION CLUSTERING  =====================================


# Based on B. J. Frey and D. Dueck. Clustering by
# passing messages between data points.
# Science, 315(5814):972–976, 2007

if (!require(apcluster, quietly=TRUE)) {
    install.packages("apcluster")
    library(apcluster)
}

apRes <- apcluster(negDistMat(r=2), datNorm)
apRes

heatmap(apRes)

# The clear and pronounced block structure shows that this
# is a successful clustering...

length(apRes)
cutree(apRes)

oPar <- par(mfrow=c(3,2))
matplot(t(datNorm[unlist(apRes[ 2]),]),
        type="l",xlab="time",ylab="log expression value")
matplot(t(datNorm[unlist(apRes[ 3]),]),
        type="l",xlab="time",ylab="log expression value")
matplot(t(datNorm[unlist(apRes[ 8]),]),
        type="l",xlab="time",ylab="log expression value")
matplot(t(datNorm[unlist(apRes[14]),]),
        type="l",xlab="time",ylab="log expression value")
matplot(t(datNorm[unlist(apRes[15]),]),
        type="l",xlab="time",ylab="log expression value")
matplot(t(datNorm[unlist(apRes[ 9]),]),
        type="l",xlab="time",ylab="log expression value")
par(oPar)


# =    7  CLUSTER QUALITY METRICS  =============================================


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
if (!require(kohonen, quietly=TRUE)) {
    install.packages("kohonen")
    library(kohonen)
}
if (!require(mclust, quietly=TRUE)) {
    install.packages("mclust")
    library(mclust)
}
?clValid

# This is pretty nice: we _can_ use biological
# knowledge for validation...
# But for our example, we'll try internal validation
# on all available methods.

valClust <- clValid(dat,
                    nClust = 2:9,
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


# =    8  T-STOCHASTIC NEIGBOUR EMBEDDING  =====================================

# tsne - pioneered by Geoff Hinton - is an embedding
# procedure that guarantees that points in a high-
# dimensional feature space that are close together
# remain close together when projected into a low-
# dimensional space. It can give astoundingly good
# results that help figuring out the internal structure
# of a dataset.

if (!require(tsne, quietly=TRUE)) {
    install.packages("tsne")
    library(tsne)
}

# The clustering method uses a "callback" to  execute
# a plotting routine as it improves its embedding
# through many cycles.

# define a plotting routine
plotProgress <- function(x){
    plot(x, type='n');
    #	text(x, labels = rownames(dat), cex=0.5)
    points(x, pch=21, col="#6677FF", bg="firebrick")
}

set.seed(112358)
tsneDat <- tsne(datNorm, epoch_callback = plotProgress)


# I've run this many times to find a
# seed that gives a low error, and run until the iteration
# stabilizes, it's worthwhile experimenting a bit ...

# ... this will run a few minutes  :-)
set.seed(270745)
tsneRef <- tsne(dat,
                epoch = 200, max_iter=8000,
                epoch_callback = plotProgress, perplexity=10)


# We see that the normalized data is well behaved. And we see
# intriguing structure in the data. What does this mean though?
# How do the results compare to what we have seen previously?
# Lets plot this plot with the color values we got from our
# affinity-propagation (AP) clustering.

# This is nice data to have around. We'll store it in a local file ...
write.csv(tsneRef, file="tsneRef.dat")

# and then read it back in like so...
tsneRef <- as.matrix(read.csv(file="tsneRef.dat",
                              row.names=1,
                              header=TRUE))



# ==   8.1  Digression: color scales  ==========================================


# Our AP clustering had identified 16 clusters. The color brewer palette
# "Spectrum" supports only a max. of 11 colors - and there is
# some wisdom in not overloading your plot with colors. But lets define
# a color palette for more colors anyway. You could use one of the
# inbuilt palettes ...

n<-20
pie(rep(1,n), col=rainbow(n), main="rainbow()")
pie(rep(1,n), col=heat.colors(n), main="heat.colors()")
pie(rep(1,n), col=terrain.colors(n), main="terrain.colors()")
pie(rep(1,n), col=topo.colors(n), main="topo.colors()")
pie(rep(1,n), col=cm.colors(n), main="cm.colors()")

# ... or we could do something a bit more fancy: here is
# code that will generate a spectrum loosely based on the Munsell
# color wheel of perceptually equidistant colors (with some
# tweaks to increase the spread in the blue-green while
# keeping within the display gamut).

eqSpect <- colorRampPalette(
    c("#f2003c", "#F66900", "#F19100", "#F1B100",
      "#EFD300", "#f0ea00", "#CBDB00", "#9DD501",
      "#5ED108", "#00AF63", "#00A78E", "#00a3ac",
      "#0093af", "#0082b2", "#006ebf", "#4F37C2",
      "#8000D3", "#A001BF", "#BA00A4", "#D0007F"),
    space="rgb",
    interpolate="spline")

# Such a perceptually tuned spectrum is quite a bit more pleasing
# than one that is computed from extrapolating between rgb values.
# Adjacent colors are easier to distinguish, in particular hues that
# are close to the primary reds, greens and blues...

pie(rep(1,n), col=eqSpect(n), main="eqSpect()")


# ==   8.2  Colouring tsne results  ============================================


# Let's use our eqSpect() colors to plot our tsn-embedded points,
# and color them according to their AP class:

plot(tsneRef[,1], tsneRef[,2], type='n');  # set up the frame

n <- length(apRes)
for (i in 1:n) {
    points(tsneRef[unlist(apRes[i]),1],
           tsneRef[unlist(apRes[i]),2],
           pch=21,
           col="#444444",
           bg= eqSpect(n)[i])
}

# Nice. We see that there is quite a bit of overlap between
# the clusters that we "see" in the embedding, and those that
# AP has defined. There are also clusters that look mixed up
# and should probably be joined.


# ==   8.3  Selecting from tsne results  =======================================


# Time for something a little bit more advanced. Looking
# at this plot I would like to know how the visual clusters
# are structured internally, i.e.  I would like to identify
# some points, find out what what their row-numbers are and
# plot them separately. This is crucial, to be able to use
# your plotting analysis in a workflow, otherwise the image
# you create is a dead end.

# The simplest approach is just to plot the row-numbers using
# the text() function.

plot(tsneRef[,1], tsneRef[,2], type='n');
text(tsneRef[,1], tsneRef[,2],
     labels=as.character(1:nrow(tsneRef)));

# ... well lets make the labels a bit smaller, for less overlap

plot(tsneRef[,1], tsneRef[,2], type='n');
text(tsneRef[,1], tsneRef[,2],
     labels=as.character(1:nrow(tsneRef)),
     cex=0.4);

# ... and color the labels according to the 16 AP clusters:
#
n <- length(apRes)
colV <- rep("", nrow(dat))
for (i in 1:n) {
    colV[as.integer(unlist(apRes[i]))] <- eqSpect(n)[i]
}
plot(tsneRef[,1], tsneRef[,2], type='n');
text(tsneRef[,1], tsneRef[,2],
     labels=as.character(1:length(tsneRef[,1])),
     cex=0.5, col=colV);

# We could now use this information and get a matplot() of the
# clusters by hand ... like so:
# First record  a set of datapoints we are interested in -
# from the lower right corner of the plot:
set1 <- c(5, 13, 44, 12, 80, 64, 16, 10, 48, 102, 31, 109, 42, 106, 147, 57, 85)
set2 <- c(189, 54, 35, 202, 119, 20, 50, 130, 161, 167, 8, 77, 236, 1)
set3 <- c(156, 187, 27, 223, 132, 224, 209, 73)

# Plot these expression profiles
matplot(t(datNorm[set1,]),
        type="l", lwd=2, lty=1, col=eqSpect(16)[4],
        xlab="time",ylab="log expression value")
for (i in 1:length(set2)) {
    lines(datNorm[set2[i], ], type="l", lwd=1, lty=1, col=eqSpect(16)[3])
}
for (i in 1:length(set3)) {
    lines(datNorm[set3[i], ], type="l", lwd=1, lty=1, col=eqSpect(16)[15])
}

# Take some time to study this. It shows quite well how the internal
# structure within a set of similarly expressed genes is separated into
# clusters by AP, and how tsne gives us a good indication of that structure.

# ==   8.4  Interactive graphics with tsne results  ============================

# But typing all those numbers by hand is tedious! Wouldn't it be nice if we
# could select them interactively?

# Yes.

# R provides two functions for interactive work with 2D plots:
# identify(), and locator().

# identify() returns the row number of a picked point.
# In order to pick points, we have to plot them
plot(tsneRef[,1], tsneRef[,2], pch=19, cex=0.8, col=colV);

# Try this ...
identify(tsneRef)
# ... and press <esc> to stop (or a different mouse button)

# We can write a function to show us what we picked, display the
# actual profiles in a matplot() in a second window, and finally
# return the row numbers, so we can use them (e.g. to retrieve
# the gene symbols).

# First we open a separate window for the paralell
# coordinates plot ...
w1 <- dev.cur()   # remember which one is our main device
dev.new()         # open a second graphics window
w2  <- dev.cur()  # store its ID
dev.set(w1)       # return focus to the first window


# Then we define a function ...

pickGenes <- function(x) {
    # picks genes from our tsne output plot.
    # initialize:
    all <- c()
    dev.set(w2)  # set focus on a second window ...
    matplot(t(datNorm[set1,]), type="n",   # ... empty frame for this plot
            xlab="time",
            ylab="log expression value")
    dev.set(w1) # focus back on window 1
    # This is the main picking loop.
    while(TRUE) {
        pick <- identify(x, n=1, plot=FALSE) # Every pick assigns a single
        # number to variable "pick".
        if (!length(pick)) break             # If we press <esc>, "pick" has no
        # length. Then we abort the loop.
        # Otherwise, we just got the row-
        # number of the item we picked.
        # First, overwrite the point with
        # a semi-transparent white point...
        points(x[pick,1], x[pick,2], pch=19, cex=0.8, col="#FFFFFFAA")
        print(pick)                          # ...  print the row to console ...
        all <- c(all, pick)                  # ... and add it to the results.
        dev.set(w2)                          # Set focus to window 2 ...
        # ... and draw one expression profile
        # into the matplot() frame.
        lines(datNorm[pick, ], type="l", lwd=1, lty=1, col=colV[pick])
        dev.set(w1)                          # Focus back on window 1.
    }
    return(all)  # Done picking, return row of picked coordinates.
}

# Plot the points again ...
plot(tsneRef[,1], tsneRef[,2], pch=19, cex=0.8, col=colV);

# ... and start picking. Press <esc> to stop.
myPicked <- pickGenes(tsneRef)
rownames(dat[myPicked,])


# ==   8.5  A Polygon selection  ===============================================


# We can use a conceptually very similar procedure to
# draw a polygon and return all points that are within
# the polygon. The function locate() returns coordinate
# pairs of points - in units of the active device.
?locator

# Lets look at the data we get.
# Click into the plot a few time, then hit <esc>.
locator()

# Looks straightforward. We can use the "type" parameter,
# to show our picks on screen. Here we draw light-grey
# lines.
locator(type = "l", col="#BBBBBB")

# Note that the polygon we drew is not closed. Let's
#capture the coordinates, and add the closing line.
poly <- locator(type = "l", col="#BBBBBB")
lines(poly$x[c(1,length(poly$x))], poly$y[c(1,length(poly$y))], col="#BBBBBB")

# All we need to do is to loop through the data points and
# for each one determine whether it is inside or
# outside the polygon. There is a convenient function
# in the generalized additive modeling package mgcv:

if (!require(mgcv, quietly=TRUE)) {
    install.packages("mgcv")
    library(mgcv)
}
?in.out

# This makes our code very simple.
sel <- function(pp) {
    # draw the polygon
    pG <- locator(type = "o", cex=0.4, col="#BBBBBB")
    lines(pG$x[c(1,length(pG$x))], pG$y[c(1,length(pG$y))], col="#BBBBBB")

    # find the inside points
    selected <- which(in.out(cbind(pG$x, pG$y), pp))

    # mark the selected points
    points(tsneRef[selected,], pch=19, cex=0.8, col="#FFFFFFAA")

    # draw the paralell coordinates into the second window
    dev.set(w2)
    for (i in 1:length(selected)) {
        lines(datNorm[selected[i], ],
              type="l",
              lwd=1,
              lty=1,
              col=colV[selected[i]])
    }
    dev.set(w1)

    # done
    return(selected)
}

# So as not to get confused, close all graphics windows...
graphics.off()

# replot our points and the empty matplot() frame
dev.new()
w1 <- dev.cur()
dev.new()
w2  <- dev.cur()
dev.set(w1)
plot(tsneRef[,1], tsneRef[,2], pch=19, cex=0.8, col=colV);
dev.set(w2)
matplot(t(datNorm[]), type="n",   # ... empty frame for paralell coordinates
        xlab="time",ylab="log expression value")
dev.set(w1)

# Now select a cluster of points, finish by pressing escape
# and see them plotted.
(picks <- sel(tsneRef))


# ==   8.6  tsne 3D embedding  =================================================


# tsne can embed in 2- 3- or higher dimensions. There are
# inbuilt ways to work with 3D data in R - try ...
demo(persp)
# ... but for "real" work with 3D data, the standard
# is the rgl package. This may require you to update
# a few libraries on your machine ...
if (!require(rgl, quietly=TRUE)) {
    install.packages("rgl")
    library(rgl)
}

# rgl has its own demo function that you could explore
# demo(rgl)

# here we simply plot coloured points in 3D by calling
# the plot3d() function as our callback for tsne() ...

plotTsne3D <- function(x){
    plot3d(x[,1], x[,2], x[,3], type='p', col=colV)
    #	text3d(x[,1], x[,2], x[,3], text=names(colV), col=colV, cex=1.4)
}

# ... and setting k=3 as the number of dimensions for tsne to embed
# into.

set.seed(112358)
tsne3D <- tsne(datNorm,
               epoch = 100,
               k=3,
               max_iter=3000,
               epoch_callback = plotTsne3D,
               perplexity=10)



# =    9  MORE PRACTICE  =======================================================

# TASK: Based on a clustering of the PCA results (without dimension 1), cluster
#       the crabs data into four clusters of orange/blue males/females.
#       Then confirm the percentage of each category that was correctly
#       clustered by comparing the cluster IDs to the labels in the crabs
#       data set.

# ==================================================
# outlook: classification
# ==================================================
# http://www.win-vector.com/blog/2014/12/the-geometry-of-classifiers/



# That's all.

# [End]
