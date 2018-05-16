# R-EDA-Introduction.R
#
# Purpose:
#
# Version: 2.0
#
# Date:    2018  05
# Author:  Boris Steipe (boris.steipe@utoronto.ca)
#
# V 2.0    Restructured for 2018 workshop
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
# Module 1: EDA (Exploratory Data Analysis)
#
# ==============================================================================


#TOC> ==========================================================================
#TOC>
#TOC>   Section  Title                                                 Line
#TOC> ---------------------------------------------------------------------
#TOC>   1        Project files and setup                                 85
#TOC>   1.1        Optional tasks:                                      111
#TOC>   1.2        Confirm setup                                        128
#TOC>   2        Load Data                                              148
#TOC>   2.1        Task: Getting data into R                            161
#TOC>   2.2        Task: Read supplementary table S3 from Excel         211
#TOC>   2.3        Digression: Factors in dataframes                    233
#TOC>   3        Subsetting                                             294
#TOC>   3.1        Subsetting data - Review of the principles           296
#TOC>   3.1.1          A sample dataset                                 305
#TOC>   3.1.2          Subsetting by index                              348
#TOC>   3.1.2.1        Negative indices                                 392
#TOC>   3.1.3          Subsetting by logical                            403
#TOC>   3.1.3.1        Filtering by string matching expressions         456
#TOC>   3.1.4          Subsetting by name                               474
#TOC>   3.1.5          The "$" operator                                 507
#TOC>   3.2        Subsetting Task                                      527
#TOC>   4        Descriptive statistics and simple plots                552
#TOC>   4.1        Quantiles                                            578
#TOC>   4.1.1          Boxplot                                          596
#TOC>   4.1.2          Violin plot                                      613
#TOC>   4.2        Plotting principles                                  646
#TOC>   4.3        QQ plots                                             661
#TOC>   4.4        Lines and legends                                    668
#TOC>   5        Exploring flow cytometry data                          701
#TOC>   5.1        Explore scatter plots                                715
#TOC>   5.2        Trellis plots: all against all                       786
#TOC>
#TOC> ==========================================================================


# =    1  Project files and setup  =============================================
#
# What's in the box - overview of files specific to this module:

#     ./assets
#       Jaitin_paper.zip
#       Weissgerber_2015_BeyondBarcharts.zip
#
#     ./data
#        0TST.PDB
#        Fig_3-CharacteristicGenes.txt
#        table_S3.csv
#        table_S3.xls
#        GvHD.txt
#
#     ./R
#       objectInfo.R
#       functionTemplate
#
#     ./scripts
#        PlottingReference.R
#        RPR-RegEx.R
#        scriptTemplate.R
#        unitTesting.R
#

# ==   1.1  Optional tasks:  ===================================================
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

# ==   1.2  Confirm setup  =====================================================

# TASK: Confirm that everything is set up correctly:
        getwd()          # Confirm the correct directory
        list.files()     # Confirm that the right files are present.
        list.files(all.files = TRUE)
        list.dirs()

#        Check your "Environment pane": The functions:
#           biCode()
#           init()
#           objectInfo()
#           readFASTA
#           ... should be present. These have been loaded on startup,
#           thus confirming that the startup script has worked.
#
#       Check your "Files" pane: the file "myEDANotes.R" should exist.
#       Open it.


# =    2  Load Data  ===========================================================

# Before we can do any kind of exploratory analysis, we first need to load data.
# There are many sources - the Web, text files, Excel spread sheets. Here, we'll
# explore data from the supplementary material published with a recent paper on
# single-cell RNAseq analysis.

# TASK: Unzip and browse the Jaitin et al. paper.


# Often the data we need can be copied and pasted from simple
# text files.

# ==   2.1  Task: Getting data into R  =========================================

# TASK: open the text file for "./data/Fig_3-CharacteristicGenes.txt".
#       I have prepared this file from the text in a figure of
#       the actual paper.
#       How do we get this data into a vector in R?

#       First think of a way to do this by hand.
#       Then, figure out if there is an R function that does this:
#         - Open a textfile given its filename
#         - Return a vector of strings

#       (See "./sampleSolutions/sampleSolution-readText.R" if needed.)



# TASK: open supplementary table 3 of Jaitin et al. in Excel (it's in your
#       ./data folder ("./data/tableS3.xls"):


# A word on Excel: it's a very good spreadsheet program,
# but it is miserable and often wrong on statistics,
# and it makes horrible, horrible plots.

# To elaborate - see the two links below:
# http://www.practicalstats.com/xlsstats/excelstats.html
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
# package xlsreadwrite is available via CRAN ... see
# http://cran.r-project.org/web/packages/xlsReadWrite/ ... but I think this is
# unsound practice.


# ==   2.2  Task: Read supplementary table S3 from Excel  ======================

# TASK:
# 1 - load the data in table_S3.xls
#     into Excel, and save it as
#     a .csv (comma separated values) file.
# 2 - Examine the file (a plain text file) in a
#     text-editor (such as the RStudio editor).
# 3 - Read the table into R, assigning it to a variable.
#     I usually give the first input of data the variable
#     name "rawDat" since it will usually be processed before
#     it becomes meaningful for analysis.
# 4 - Use head() to look at the beginning of the object.
# 5 - Remove any unneeded header rows.
# 6 - Give the columns names that reflect the cell type (cf.
#     Figure 2c), and the stimulus status. Use cell abbreviations B, MF,
#     NK, Mo, pDC, DC1 and DC2 and distinguish ctrl and LPS conditions.
#     Call the last column "cluster".
# 7 - Use objectInfo() to validate that the data frame you have created
#     conforms to specifications. The first column is a character column,
#     the other columns are numeric.
# 8 - Call the final object "LPSdat"

# ==   2.3  Digression: Factors in dataframes  =================================

# Many of R's dataframe methods convert all strings
# into factors by default. Factors are special types:
# they are nominally strings - (m, f) or (agree, neutral,
# disagree) or such. But underlyingly they are coded as integers
# that identify the "levels" of the factor.

# To illustrate.
genders <- factor(c("m", "f", "f", "m", "f"))
genders
objectInfo(genders)
is.ordered(genders)

# We can define ordered factors by adding some details to
# our factor() command - e.g. tumor grades:


sampleGrades <- factor(c("G1", "G3", "G3", "G1", "G2", "G1"),
                       levels = c("G1", "G2", "G3", "G4"),
                       ordered = TRUE)
sampleGrades   # Note that G4 is a level although it
# was not observed in the data
is.ordered(sampleGrades)

# Factors are useful since they support a number of analysis
# methods such as ordering boxplots, or calculating

# For more on factors, have a look at this factor tutorial
# by Jenny Bryan:
# http://www.stat.ubc.ca/~jenny/STAT545A/block08_bossYourFactors.html
# and this discussion on their use:
# http://stackoverflow.com/questions/3445316/factors-in-r-more-than-an-annoyance
#

# But for our purposes, the default behavior of R, to
# treat all strings as factors is entirely unwanted
# and needs to be turned off. Always use the parameter
# stringsAsFactors = FALSE to achieve this. If you don't
# you are in for some bad surprises if e.g. there is
# a character "contaminant" such as "NA" in a numeric column.

# Note: you can change R's default behaviour and set a global
#       option to "turn factors off". Some advocate this,
#       I don't think this is a good idea, since you may
#       encounter packages or functions that make
#       assumptions about R's default behaviour and will thus
#       fail.

myDF <- data.frame(data = c("N/A", 1, 1, 2, 3, 5, 8))
objectInfo(myDF)
myDF <- myDF[-1, ]
myDF

myDF2 <- as.numeric(myDF)
myDF2     # Whoa! what just happened ?

myDF3 <- as.numeric(as.character(myDF))
myDF3     # :-)


# =    3  Subsetting  ==========================================================

# ==   3.1  Subsetting data - Review of the principles  ========================


# A significant portion of your efforts in any project will be spent on
# preparing data for analysis. This includes reading data from various sources,
# preprocessing it, and extracting subsets of interest. R has powerful functions
# that support these tasks.


# ===   3.1.1  A sample dataset

# Let's start with a small datframe of synthetic data to go through the main
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

order(dat[ , 2])   # ... tells us in which row the sorted values are
dat[order(dat[ , 2]), 1:3]  # ordered by number of legs
dat[order(dat[ , 1]), 1:3]  # ordered by lexical order of names

# Note: I am indenting expressions so you can first evaluate the expressions
# individually, then see how they fit into the brackets to subset the data.


# ====  3.1.2.1  Negative indices

# If you specify a negative index, that element is excluded.

dat[-1, 1:3]   # not the first row
dat[-N, 1:3]   # not the last row

dat[-1:-3, 1:3]
dat[-(1:3), 1:3]  # same effect


# ===   3.1.3  Subsetting by logical


# Instead of indices, we can specify sets of rows or columns by boolean values
# (type: logical): TRUE or FALSE. If we place a vector of logicals into the
# square brackets, only the rows resp. columns for which the expression is TRUE
# are returned.

dat[1:3, c(TRUE, FALSE, TRUE, FALSE, FALSE, FALSE, FALSE, FALSE)]

# You need to take care that the number of elements exactly matches the number
# of rows or columns, otherwise "vector recycling" will apply and this is
# probably unintended. Thus explicitly specifying a boolean selection like above
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

myF <- function(x){any(x > 1.5)}
myF(dat[3, 4:8])

apply(dat[ , 4:8], 1, myF)
dat[apply(dat[ , 4:8], 1, myF), ]

# But we can also write the definition "in place"...
apply(dat[ , 4:8], 1, function(x){all(x < 0.5)})

dat[apply(dat[ , 4:8], 1, function(x){all(x < 0.5)}), ]

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

# TASK: Write R expressions that get the following data from LPSdat:


#      - rows 1:10 of the first two columns in reverse order

#      - gene names and the expression values for Mo.LPS
#        for the top ten expression values.
#        ( hint: use order() )

#      - All genes for which B-cells are stimulated by LPS by
#        more than 2 log units.

#      - Expression values for all genes whose gene-names appear in Figure 3B.
#        (hint: use the  %in%  operator)


# With these simple subsetting and filtering operations, we are already
# "Exploring data".


# =    4  Descriptive statistics and simple plots  =============================

set.seed(100)
x <- rnorm(100, mean=0, sd=1)
mean(x)
median(x)
IQR(x)
var(x)
sd(x)
summary(x)

# Task:
# 1 - What would be interesting characterizations
#     of LPSdat? Explore them.

# Example: mean baseline expression of gene in row 5
a <- LPSdat[5, 2:15]
a
b <- as.numeric(a)
mean(b)
mean(as.numeric(LPSdat[5, seq(2, 14, by = 2)]))

# Example: What are the most responsive genes for two
# cell types?


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

# ===   4.1.2  Violin plot
#
if (!require(ggplot2, quietly=TRUE)) {
    install.packages("ggplot2")
    library(ggplot2)
}
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

# TASK:
# 1 - What would be interesting quantiles
#     and boxplots in LPSdat?
#     Imagine, explore and share.
#
#     Interpret the results !


# ==============================================================================

# ==   4.2  Plotting principles  ===============================================

# Explore plot types  (Section 1 of ./scripts/PlottingReference.R)

# Explore lines (Section 3 of ./scripts/PlottingReference.R)

# =============================================
# Overlay a histogram with a line plot
myStim <- LPSdat$B.LPS - LPSdat$B.ctrl
hist(myStim,
     col="#0066AA44",
     main = "B.LPS - B.ctrl",
     xlab = "stimulation (delta(log(expression))",
     freq = FALSE)
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


# =    5  Exploring flow cytometry data  =======================================

# GvHD flow cytometry data is a sample dataset provided in the project.
gvhd <- read.table("./data/GvHD.txt", header=TRUE)
head(gvhd)

# Only extract the CD3 positive cells

hist(gvhd[,5])

gvhdCD3p <- as.data.frame(gvhd[gvhd[, 5]>280, 3:6])

plot(gvhdCD3p[, 1:2])

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
if (!require(hexbin, quietly=TRUE)) {
    install.packages("hexbin")
    library(hexbin)
}

# variant one: hexbin
hb <- hexbin(gvhdCD3p[, 1:2], xbins = 20)
plot(hb, colramp = colorRampPalette(c("#FFFFDD",
                                      "#77EE99",
                                      "#3377AA",
                                      "#0033BB")))

# === variant two: smoothScatter
smoothScatter(gvhdCD3p[, 1:2],
              nrpoints = 0,
              pch=20,
              cex=0.5,
              col="#6633BB55")

# === variant three: colors vary by density
plot(gvhdCD3p[, c(1,2)],
     col=densCols(gvhdCD3p[, 1], gvhdCD3p[, 2]),
     pch=16,
     cex=2)

# === variant four: analysis with specialized package
# load "prada" package from BioConductor
if (!require(prada, quietly=TRUE)) {
    source("http://www.bioconductor.org/biocLite.R")
    biocLite("prada")
}

# using functions ?fitNorm2 and plotNorm2 from the prada package
nfit <- fitNorm2(gvhdCD3p[, 1], gvhdCD3p[, 2])
plotNorm2(nfit, selection=TRUE, ellipse=TRUE)

# ==============================================================================

# TASK:
# 1 - Create a scatterplot from differences in
#     LPS activation between a pair of cell types in LPSdat.
# 2 - Add a line that shows the situation if all
#     activations were equal.

plot(LPSdat[ ,"Mo.ctrl"] - LPSdat[ ,"Mo.LPS"],
     LPSdat[ ,"MF.ctrl"] - LPSdat[ ,"MF.LPS"])
abline(0,1,col = "red")

# 3 - Redo the plot with density coloring



# ==   5.2  Trellis plots: all against all  ====================================

plot(gvhdCD3p, pch=".")

boxplot(gvhdCD3p)


oPar <- par(mfrow=c(2, 2))

hist(gvhdCD3p[, 1], 50,
     main=names(gvhdCD3p)[1], xlab="fluorescent intensity", ylim=c(0, 120))
hist(gvhdCD3p[, 2], 50,
     main=names(gvhdCD3p)[2], xlab="fluorescent intensity", ylim=c(0, 120))
hist(gvhdCD3p[, 3], 50,
     main=names(gvhdCD3p)[3], xlab="fluorescent intensity", ylim=c(0, 120))
hist(gvhdCD3p[, 4], 50,
     main=names(gvhdCD3p)[4], xlab="fluorescent intensity", ylim=c(0, 120))

par(oPar)





# [END]
