# EDAintroduction.R
#
# Purpose:
#
# Version: 3.0
#
# Date:    2019  05
# Author:  Boris Steipe (boris.steipe@utoronto.ca)
#
# V 3.0    Reconceptualized 2019
# V 2.0    Restructured for 2018 workshop
# V 1.1    2017 updates
# V 1.0    First code 2016
#
# TODO:
#
#
# == HOW TO WORK WITH THIS FILE ================================================
#
#  This file contains scenarios and tasks, we will discuss them in detail in
#  class. If this file is named "myEDAintroduction.R", then edit it profusely,
#  write code, experiment with options, or just play.
#  Especially play.
#
#  If this file
#
#  If there is anything you don't understand, use R's help system,
#  Google for an answer, or ask. Especially ask. Don't continue if you don't
#  understand what's going on. That's not how it works ...
#
# ==============================================================================
#
# Introduction to EDA concepts
#
# ==============================================================================


#TOC> ==========================================================================
#TOC> 
#TOC>   Section  Title                                               Line
#TOC> -------------------------------------------------------------------
#TOC>   1        SCENARIO AND SETUP                                    75
#TOC>   1.1        Data:                                               95
#TOC>   1.2        Optional tasks:                                    110
#TOC>   2        LOAD DATA                                            128
#TOC>   2.1        Task: Getting text data into R                     145
#TOC>   2.2        Task: Read an Excel table                          163
#TOC>   2.3        Digression: Factors in dataframes                  232
#TOC>   3        SUBSETTING                                           296
#TOC>   3.1        Subsetting data - Review of the principles         298
#TOC>   3.1.1          A sample dataset                               307
#TOC>   3.1.2          Subsetting by index                            350
#TOC>   3.1.2.1        Negative indices                               394
#TOC>   3.1.3          Subsetting by logical                          405
#TOC>   3.1.3.1        Filtering by string matching expressions       458
#TOC>   3.1.4          Subsetting by name                             475
#TOC>   3.1.5          The "$" operator                               509
#TOC>   3.2        Subsetting Task                                    529
#TOC>   4        DESCRIPTIVE STATISTICS AND SIMPLE PLOTS              554
#TOC>   4.1        Quantiles                                          571
#TOC>   4.1.1          Boxplot                                        590
#TOC>   4.1.2          Violin plot                                    616
#TOC>   4.2        Plotting principles                                655
#TOC>   4.3        QQ plots                                           675
#TOC>   5        EXPLORING SCATTERPLOTS                               698
#TOC>   5.1        Explore scatter plots                              737
#TOC>   5.2        Trellis plots: all against all                     796
#TOC> 
#TOC> ==========================================================================


updateTOC()  # <<< Execute to update the TOC


# =    1  SCENARIO AND SETUP  ==================================================


# EDA is exploration. The goal of EDA is to
#   - uncover underlying structure;
#   - define important variables;
#   - detect outliers and anomalies;
#   - detect trends;
#   - develop statistical models;
#   - test underlying assumptions.

# A cornerstone of EDA is graphics: producing plots that visualize aspects of
# your data: the observations that make up your data - viewed directly, under
# transformation, in comparison to expectations, and integrated with other data.

# Don't think of graphics as an afterthought: graphics are central to the story
# about biology that you are telling, based on the facts you discover. Make
# your story explicit, consider how good graphics supports it, get good at this.


# ==   1.1  Data:  =============================================================

# There are three datasets we will be working with in the next two days:
#
# - The "crabs" dataset is a staple of R data samples. It is part of the MASS
#   package and conatains morphological measurements of Australian crabs
#
# - The "single cell" dataset is supplemental data from one of the
#   earliest biological single-cell RNA seq studies, on immune cells, in
#   Ido Amit's lab at the Weizman Institute.
#
# - The "cell cycle" dataset is a high resolution RNA seq study of expression
#   changes in yeast, published in 2006 by the Breeden lab at the FHCRC.


# ==   1.2  Optional tasks:  ===================================================
# While you are waiting for others to finish a "Checkpoint"
# here are suggestions:
#
# - Read the paper by Weissgerber et al. (2015) "Beyond Barcharts ..."
#   in your "./assets" folder. How could you implement the suggestions
#   in the paper in an R function?
#
# - Study how the testthat package can be used to write tests for
#   code correctness as you develop code. Work through the script in
#   "./scripts/unitTesting.R"
#
# - Learn about regular expressions at
#    http://steipe.biochemistry.utoronto.ca/abc/index.php/Regular_Expressions
#
#   and work through the script in "./scripts/RPR-RegEx.R"


# =    2  LOAD DATA  ===========================================================

# We will explore gene expression data in this unit:

source("./sampleSolutions/EDAintroductionSampleSolutions-ShowPlot.R")

# Before we can do any kind of exploratory analysis, we first need to load data.
# There are many sources - the Web, text files, Excel spread sheets. Here, we'll
# explore data from the supplementary material published with a recent paper on
# single-cell RNAseq analysis.

# TASK: Unzip and browse the Jaitin et al. paper.


# Often the data we need can be copied and pasted from simple
# text files.

# ==   2.1  Task: Getting text data into R  ====================================

# TASK: open the text file for "./data/Fig_3-CharacteristicGenes.txt".
#       I have prepared this file from the text in a figure of
#       the actual paper.
#       How do we get this data into a vector in R?

#       First think of a way to do this by hand.
#       Then, figure out if there is an R function that does this:
#         - Open a textfile given its filename
#         - Return a vector of strings
#       Assign the resulting vector to the variable name "charGenes"


insertSnip("gave.fact") # <<< Execute (if needed) to insert sample solution code



# ==   2.2  Task: Read an Excel table  =========================================

# I have included a dataset with this project, a .csv file taken from
# suplementary data of a paper on tissue definition by single cell RNA seq, by
# Jaitin et al. (2014).

# http://www.ncbi.nlm.nih.gov/pubmed/24531970

# This data set contains log2 fold changes of gene expression enrichment in
# different cd11c+ cell populations, and their response to lipopolysaccharide
# stimulation. It was posted as an Excel file on the Science Website.

# A word on Excel: it's a very good spreadsheet program,
# but it is miserable and often wrong on statistics,
# and it makes horrible, horrible plots.

# To elaborate - see the link below:
# http://www.burns-stat.com/documents/tutorials/spreadsheet-addiction/
# ... these are not merely cosmetic problems!

# Therefore: It's oK to keep data in Excel spreadsheets
# if you must - but read it into R for any analysis!

# But be cautious: one of the problems of Excel is that
# it truncates numeric precision. Protip: convert all
# cells to "text" before export.

# There are many other "read" functions.
# Refer to the R Data Import / Export manual
# http://cran.r-project.org/doc/manuals/R-data.html
# See:
?read.table # ... includes read.csv and read.delim
?read.fwf   # ... for "fixed width format"
?readLines  # ... for reading in text-files line by line

# Excel spreadsheets should be converted to csv or tsv format. Alternatively the
# readxl package is available via CRAN, see
# https://cran.r-project.org/package=readxl ... but be very careful and double
# and triple check your data.


# TASK:
#  1 - load the supplementary data from the Jaitin et al. single cell study
#      ./data/table_S3.xls into Excel, and save it as
#      a .csv (comma separated values) file.
#  2 - Examine the file (a plain text file) in a
#      text-editor (such as the RStudio editor).
#  3 - Read the table into R, assigning it to a variable.
#      I usually give the first input of data the variable
#      name "rawDat" or "tmp" since it will be processed before
#      it becomes meaningful for analysis.
#  4 - Use head() to look at the beginning of the object.
#  5 - Remove any unneeded header rows.
#  6 - Fix the rownames so they start at 1
#  7 - Give the columns names that reflect the cell type (cf.
#      Figure 2c), and the stimulus status. Use cell abbreviations B, MF,
#      NK, Mo, pDC, DC1 and DC2 and distinguish ctrl and LPS conditions.
#      Call the last column "cluster".
#  8 - Change all expression values that were read as strings to "numeric"
#  9 - Use str() to validate that the data frame you have created
#      conforms to specifications. The first column is a character column,
#      the other columns are numeric.
# 10 - Assign the final object to the variable name "LPSdat"
# 11 - What tests can you devise to validate the correctness of the data?


insertSnip("file.dome") # <<< Execute (if needed) to insert sample solution code


# ==   2.3  Digression: Factors in dataframes  =================================

# Many of R's dataframe methods convert all strings
# into factors by default. Factors are special types:
# they are nominally strings - (m, f) or (agree, neutral,
# disagree) or such. But underlyingly they are coded as integers
# that identify the "levels" of the factor.

# To illustrate.
sex <- factor(c("m", "f", "f", "m", "f"))
sex
str(sex)
is.ordered(sex)


# We can define ordered factors by adding some details to
# our factor() command - e.g. tumor grades:

sampleGrades <- factor(c("G1", "G3", "G3", "G1", "G2", "G1"),
                       levels = c("G1", "G2", "G3", "G4"),
                       ordered = TRUE)
sampleGrades   # Note that G4 is a level although it
               # was not observed in the data
is.ordered(sampleGrades)


# Factors are useful since they support a number of analysis
# methods such as ordering boxplots, or calculating regression on categorical
# data.

# For more on factors, have a look at this factor tutorial
# by Jenny Bryan:
# http://www.stat.ubc.ca/~jenny/STAT545A/block08_bossYourFactors.html
# and this discussion on their use:
# http://stackoverflow.com/questions/3445316/factors-in-r-more-than-an-annoyance
#

# But for our purposes, the default behavior of R, to
# treat all strings as factors is usually unwanted
# and needs to be turned off. Always use the parameter
# as.is = TRUE to achieve this. If you don't
# you are in for some bad surprises if e.g. if there is
# a character "contaminant" such as "NA" in a numeric column.

# Note: you can change R's default behaviour and set a global
#       option to "turn factors off". Some advocate this,
#       I don't think this is a good idea, since you may
#       encounter packages or functions that make
#       assumptions about R's default behaviour and will thus
#       fail.

# But consider the following carefully: this illustrates how work with factors
# can go VERY wrong...
(myDF <- data.frame(data = c("N/A", 1, 1, 2, 3, 5, 8)))

str(myDF)

(myDF <- myDF[-1, ])

(myDF2 <- as.numeric(myDF))                # Whoa! what just happened ?

(myDF3 <- as.numeric(as.character(myDF)))  # :-)


# =    3  SUBSETTING  ==========================================================

# ==   3.1  Subsetting data - Review of the principles  ========================


# A significant portion of your efforts in any project will be spent on
# preparing data for analysis. This includes reading data from various sources,
# preprocessing it, and extracting subsets of interest. R has powerful functions
# that support these tasks.


# ===   3.1.1  A sample dataset                          

# Let's start with a small dataframe of synthetic data to go through the main
# principles of subsetting. The same principles apply to matrices and vectors -
# however, data frames are more flexible because their columns can contain data
# of different types (character, numeric, logical ...). Values in vectors and
# matrices must always have the same type.

# Imagine you are a naturalist who has collected some living things and keeps
# observations in a table ...

set.seed(112358)
N <- 10

dat <- data.frame(name = sample(LETTERS, N, replace = TRUE),
                  legs = sample(c(2 * (0:5), 100), N, replace = TRUE),
                  type = character(N),
                  matrix(rnorm(5 * N), ncol = 5),
                  stringsAsFactors=FALSE)

# Some auxiliary data ...
dict <- c("fish", "bird", "beast", "bug", "spider", "crab", "centipede")
names(dict) <- c(2 * (0:5), 100)
#... to populate the >>type<< column:
dat$type <- dict[as.character(dat$legs)]

# If you already understand the expression above, you're doing pretty well with
# the topic of this tutorial. If you don't, don't worry - by the end of the
# tutorial you will.

# Now let's see what we have:

head(dat)
str(dat)

# Note that we have given names to some columns, but R made names for the five
# columns of random values that were created as a matrix. Let us look at the
# basic ways to subset such objects. Basically, all these methods work with the
# subsetting operator "[".

?"["


# ===   3.1.2  Subsetting by index                       

# Elements can be uniquely identified by indices in the range of their length
# (for vectors), or their rows and columns (in dataframes and matrices). The
# order is row, column.

dat[2,3]   # one element
dat[2, ]   # empty columns: use all of them
dat[ , 3]  # empty rows, use all of them

# If you want a particular set of row and columns, pass a vector of positive
# integers.
dat[c(2, 3), c(1, 2, 3)]

# Any function that returns a vector of integers can be used. Most frequently we
# use the range operator ":" . Retrieving ranges of rows and/or columns from a
# matrix or data frame is also called "slicing".

dat[1:4, 1:3]
dat[4:1, 1:3]   # same in reverse order
dat[seq(2, N, by=2), ]   # even rows

# But we can do more interesting things, since the indices don't have to be
# unique, or in any order:

dat[c(1, 1, 1, 2, 2, 3), 1:3]

# In particular we can select random subsets...
dat[sample(1:N, 3), 1:3]
dat[sample(1:N, 3), 1:3]
dat[sample(1:N, 3), 1:3]

# ... or sort the dataframe. Sorting requires the order() function, not sort().

sort(dat[ , 2])    # ... gives us the sorted values

    order(dat[ , 2])        # ... tells us in which row the sorted values are
dat[order(dat[ , 2]), 1:3]  # ordered by number of legs
dat[order(dat[ , 1]), 1:3]  # ordered by lexical order of names

# Note: I am indenting expressions so you can first evaluate the expressions
# individually, then see how they fit into the brackets to subset the data.


# ====  3.1.2.1  Negative indices                          

# If you specify a negative index, that element is excluded.

dat[-1, 1:3]   # not the first row
dat[-N, 1:3]   # not the last row

dat[-1:-3,  1:3]
dat[-(1:3), 1:3]  # same effect


# ===   3.1.3  Subsetting by logical                     

# Instead of indices, we can specify sets of rows or columns by boolean values
# (type: logical): TRUE or FALSE. If we place a vector of logicals into the
# square brackets, only the rows resp. columns for which the expression is TRUE
# are returned.

dat[1:3, c(TRUE, FALSE, TRUE, FALSE, FALSE, FALSE, FALSE, FALSE)]

# You need to take care that the number of elements exactly matches the number
# of rows or columns, otherwise "vector recycling" will apply and this is
# usually unintended. Thus explicitly specifying a boolean selection like above
# is not all that useful. However, many R functions are "vectorized" and
# applying a logical expression or function to an entire column gives a vector
# of TRUE and FALSE values of the same length. If we place this vector into the
# square brackets, only the rows resp. columns for which the expression is TRUE
# are returned.

    dat[ , 2]
    dat[ , 2] > 4          # See how this creates a vector
dat[dat[ , 2] > 4, 1:3]

# Expressions can be combined with the "&" (and) and the "|" (or) operators.

    dat[ , 4] > 0
    dat[ , 5] < 0
    dat[ , 4] > 0 & dat[ , 5] < 0
dat[dat[ , 4] > 0 & dat[ , 5] < 0, ]

# In this context, the any() and all() functions may be useful. But take care -
# you can't simply apply them to a range of columns: that would apply the
# condition to all elements of a selection at once. You need to use the apply()
# function to first return a vector. apply()'s second argument switches between
# row-wise and column-wise evaluation. Here, 1 means operate on rows."

    apply(dat[ , 4:8], 1, max)           # row-wise, fetch the maximum
    apply(dat[ , 4:8], 1, max) > 1       # max() > 1 ?
dat[apply(dat[ , 4:8], 1, max) > 1, ]

# To use any() and all(), we define our own function."

myF <- function(x){ any(x > 1.5) }
myF(dat[3, 4:8])

    apply(dat[ , 4:8], 1, myF)
dat[apply(dat[ , 4:8], 1, myF), ]

# But we can also write the definition "in place"...
    apply(dat[ , 4:8], 1, function(x){ all(x < 0.5) })

dat[apply(dat[ , 4:8], 1, function(x){ all(x < 0.5)} ), ]


# ====  3.1.3.1  Filtering by string matching expressions  

# The function grep(), and the %in% operator can be used to subset via string
# matching:

    grep("r", dat[ , 3])          # types that contain "r"
dat[grep("r", dat[ , 3]), 1:3]

    grep("^c", dat[ , 3])         # types that begin with "c"
dat[grep("^c", dat[ , 3]), 1:3]


scary <- c("spider", "centipede")
    dat[ , 3] %in% scary
dat[dat[ , 3] %in% scary, 1:3]


# ===   3.1.4  Subsetting by name                        

# If rownames and/or columnnames have been defined, we can use these for
# selection. If not defined, they default to the row/column numbers as character
# strings(!).

rownames(dat)  # the row numbers, but note that they are strings!
colnames(dat)  # the ones we have defined

# If we place a string or a vector of strings into the brackets, R matches the
# corresponding row/ column names:"

dat[1:5, "name"]
dat[1:5, c("name", "legs")]
dat[1:5, "eyes"]   # error, that name does not exist

# We can combine the techniques e.g. to flexibly select columns. Here we select
# the X1 to X5 columns:

                                  colnames(dat)
                       grep("^X", colnames(dat))
         colnames(dat)[grep("^X", colnames(dat))]
dat[1:3, colnames(dat)[grep("^X", colnames(dat))]]

# This is very useful when the exact position of columns may have changed during
# the analysis. Actually, rows and columns should really never be selected by
# number even though we have done so above. Such numbers are "magic numbers" and
# code that relies on such magic numbers is heard to read and very hard to
# maintain. It is always better to expose the logic with which your columns are
# selected and to make the selection explicit and robust. An exception may be
# when you need a slice of the data for testing purposes, but even then it may
# be preferrable to use the head() or tail() functions.


# ===   3.1.5  The "$" operator                          

# The "$" operator returns a single column as a vector. It is not strictly
# necessary - the column can just as well be named in quotation marks within the
# brackets - but I think it makes for more readable code.
#
dat[1:3, "legs"]
dat$legs[1:3]    # same result. This is the preferred version.
dat$"legs"[1:3]  # works, but isn't necessary
dat[1:3, legs]   # this returns an error - hopefully; but if for any
                 # reason the object DOES exist, you'll get an un-
                 # expected result. Know when to quote!


# Three more functions that I use all the time for data manipulation:
?which
?unique
?duplicated


# ==   3.2  Subsetting Task  ===================================================

# TASK:
#   Write R expressions that get the following data from LPSdat:

#      - rows 1:10 of the first three columns in reverse order

#      - gene names and the expression values for Mo.LPS
#        for the top ten expression values.
#        ( hint: use order() )

#      - All genes for which B-cells are stimulated by LPS by
#        more than 2 log units.

#      - Expression values for all genes whose gene-names appear in Figure 3B.
#        (hint: use the  %in%  operator)


insertSnip("blot.pipe") # <<< Execute (if needed) to insert sample solution code


# With these simple subsetting and filtering operations, we are already
# "Exploring data".


# =    4  DESCRIPTIVE STATISTICS AND SIMPLE PLOTS  =============================

set.seed(100)
x <- rnorm(100, mean=0, sd=1)
mean(x)
median(x)
IQR(x)
var(x)
sd(x)
summary(x)

# TASK:
#  1 - Explore interesting characterizations of LPSdat.




# ==   4.1  Quantiles  =========================================================

# What is the threshold that has a given fraction of values above/below it?

x <- seq(-4, 4, 0.1)
f <- dnorm(x, mean=0, sd=1)
q90 <- qnorm(0.90, mean = 0, sd = 1)
plot(x, f, xlab="x", ylab="density", type="l", lwd=5)
abline(v=q90, col="red", lwd=5)

# empirical quantiles

set.seed(100)
x <- rnorm(100, mean=0, sd=1)
quantile(x)
quantile(x, probs=c(0.1, 0.2, 0.9))
abline(v=quantile(x, probs=c(0.9)), col="green", lwd=3)


# ===   4.1.1  Boxplot                                   

set.seed(100)
x <- rnorm(1000, mean=5, sd=2.5)
boxplot(x)
boxplot(x*x)

# Careful - a boxplot per se can obscure
# important structure in your data. Consider
# for example this bimodal distribution
x <- rnorm(100, mean=-2, sd=1)
x <- c(x, rnorm(100, mean=2, sd=1))
hist(x)
boxplot(x)

# you can use jitter() to show the actual observations

plot(jitter(rep(0.5, length(x))), x, xlim = c(0, 1))
plot(jitter(rep(0.5, length(x))), x,
     xlim=c(0, 1),
     pch=21,
     col="#00228855",
     bg="#0077FF44",
     cex = 1.4)


# ===   4.1.2  Violin plot                               
#
if (!requireNamespace("ggplot2", quietly=TRUE)) {
    install.packages("ggplot2")
}
library(ggplot2)

X <- as.data.frame(x)

p <- ggplot(X, aes(1,x))
p + geom_violin()

# See ggplot2 introductory tutorial at
#     http://www.r-bloggers.com/basic-introduction-to-ggplot2/
# and violin plot documentation at
#     http://docs.ggplot2.org/current/geom_violin.html


# Plotting more than one column with a boxplot places the plots side by side.

colnames(LPSdat)
boxplot(LPSdat[ ,c("NK.ctrl", "NK.LPS")])
boxplot(LPSdat[ , c(seq(2, 14, by = 2), seq(3, 15, by = 2))])
abline(v=7.5)

# TODO colours !


# TASK:
# 1 - What would be interesting quantiles
#     and boxplots in LPSdat?
#     Imagine, explore and share.
#
#     Interpret the results !

# TODO: quantile normalization

# ==============================================================================

# ==   4.2  Plotting principles  ===============================================

# Explore plot types  (Section 1 of ./scripts/PlottingReference.R)

# Explore lines (Section 3 of ./scripts/PlottingReference.R)

# =============================================
# Overlay a histogram with a line plot
myStim <- LPSdat$B.LPS - LPSdat$B.ctrl
hist(myStim,
     freq = FALSE,
     breaks = 40,
     col="#0066AA44",
     main = "B.LPS - B.ctrl",
     xlab = "stimulation (delta(log(expression))")
xVals <- seq(min(myStim),max(myStim),0.1)
lines(xVals, dnorm(xVals, sd = sd(myStim)), col="#CC000055")



# ==   4.3  QQ plots  ==========================================================

qqnorm(myStim, pch = 21, bg ="#0066AA22")
qqline(myStim, col="#CC000066")
# add a legend
legend("topleft",
       cex = 0.8,
       c("LPS effect", "normal distribution"),
       col = c("#0066AA22", "#CC000066"),
       lwd=5)

# =============================================
# QQ- plot: sample against sample

myStim <- LPSdat$B.LPS   - LPSdat$B.ctrl
myBase <- LPSdat$Mo.ctrl - LPSdat$B.ctrl

qqplot(myStim, myBase, pch =  21, bg = "#00AA6622")

# TASK: What other columns of LPSdat could be compared with
#       a qqplot? Explore this. Interpret the result.


# =    5  EXPLORING SCATTERPLOTS  ==============================================

# TODO Change this to to one of the three datasets - e.g.


# GvHD flow cytometry data is a sample dataset provided in the project.
gvhd <- read.table("./data/GvHD.txt", header=TRUE)
head(gvhd)

# Only extract the CD3 positive cells

hist(gvhd[,5])

gvhdCD3p <- as.data.frame(gvhd[gvhd[, 5]>280, 3:6])

plot(gvhdCD3p[, 1:2])

stimDC2 <- LPSdat$DC2.LPS - LPSdat$DC2.ctrl
stimNK  <- LPSdat$NK.LPS  - LPSdat$NK.ctrl

# A dataframe of differences:

stims <- data.frame(genes = LPSdat$genes,
                    dB   = LPSdat$B.LPS   - LPSdat$B.ctrl,
                    dMF  = LPSdat$MF.LPS  - LPSdat$MF.ctrl,
                    dNK  = LPSdat$NK.LPS  - LPSdat$NK.ctrl,
                    dMo  = LPSdat$Mo.LPS  - LPSdat$Mo.ctrl,
                    dpDC = LPSdat$pDC.LPS - LPSdat$pDC.ctrl,
                    dDC1 = LPSdat$DC1.LPS - LPSdat$DC1.ctrl,
                    dDC2 = LPSdat$DC2.LPS - LPSdat$DC2.ctrl,
                    stringsAsFactors = FALSE)

plot(stims$dMF, stims$dpDC,
     pch=21,
     col="#00228855",
     bg="#0077FF44",
     cex = 1.4)


# ==   5.1  Explore scatter plots  =============================================

# Topics:
# Section 6 - Plotting symbols and characters
#         2 - Colors
#         4 - Coordinates
# ... of ./scripts/PlottingReference.R)
# ==============================================================================

# Scatter plots are extremely useful, but learning
# a bit about R's plotting capabilities will help
# tremendously to create INFORMATIVE plots.


# Some special packages
# The overlap in the GvHD data can obscure
# data relationships. Here are some alternatives
# for dense plots:

# load "hexbin" package from CRAN
if (!requireNamespace(hexbin, quietly=TRUE)) {
    install.packages("hexbin")
}
library(hexbin)

# variant one: hexbin
hb <- hexbin::hexbin(stims$dMF, stims$dpDC, xbins = 20)
plot(hb)
plot(hb, colramp = colorRampPalette(c("#FFFFDD",
                                      "#77EE99",
                                      "#3377AA",
                                      "#0033BB")))

# === variant two: smoothScatter
smoothScatter(stims$dMF, stims$dpDC,
              nrpoints = 0,
              pch=20,
              cex=0.5,
              col="#6633BB55")

# === variant three: colors vary by density
plot(stims$dMF, stims$dpDC,
     col=densCols(stims$dMF, stims$dpDC),
     pch=16,
     cex=2)

# === variant four: analysis with specialized package
# load "prada" package from BioConductor
if (!requireNamespace("BiocManager", quietly=TRUE)) {
  install.packages("BiocManager")
}
if (!requireNamespace("prada", quietly=TRUE)) {
  BiocManager::install("prada")
}

# using functions ?fitNorm2 and plotNorm2 from the prada package
nfit <- prada::fitNorm2(stims$dMF, stims$dpDC)
prada::plotNorm2(nfit, selection=TRUE, ellipse=TRUE)

# ==   5.2  Trellis plots: all against all  ====================================

plot(stims[ , 2:8], pch=".", col = "#44BB6633")

boxplot(stims[ , 2:8])


oPar <- par(mfrow=c(2, 2))

hist(stims$dB,  breaks=40, main="dB",  freq=FALSE, ylim=c(0,1.5), col="#cffff1")
hist(stims$dNK, breaks=40, main="dNK", freq=FALSE, ylim=c(0,1.5), col="#cbe5ff")
hist(stims$dMo, breaks=40, main="dMo", freq=FALSE, ylim=c(0,1.5), col="#ecddff")
hist(stims$dMF, breaks=40, main="dMF", freq=FALSE, ylim=c(0,1.5), col="#c9f5ff")

par(oPar)





# [END]
