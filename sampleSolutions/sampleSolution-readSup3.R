# readSup3.R
#
# Purpose: read supplementary table S3 from .csv into a dataframe
#
#
# ==============================================================================

# TASK - read Table_S3.csv

# I have included a dataset with this project, a .csv file taken from
# suplementary data of a paper on tissue definition by single cell RNA seq, by
# Jaitin et al. (2014).

# http://www.ncbi.nlm.nih.gov/pubmed/24531970

# This data set contains log2 fold changes of gene expression enrichment in
# different cd11c+ cell populations, and their response to lipopolysaccharide
# stimulation. It was posted as an Excel file on the Science Website.  I have
# simply opened that file, and saved it as .csv, unchanged.

# First we open the file and have a look what it contains. Then we will properly
# read it into an R object.

rawDat <- read.csv("./data/table_S3.csv",
header = FALSE,
stringsAsFactors = FALSE)

# The object "rawDat" should appear in the Data section of the Environment tab
# in the top-right pane. It has a spreadsheet symbol next to it. Click that - or
# type View(rawDat), and study the object. You should find:
# - all columns are named Vsomething
# - rows 1 to 6 do not contain data
# - there is not a single row that could be used for column names
# - type str(rawDat): all columns are characters.

# This all needs to be fixed.


LPSdat <- rawDat[-(1:6), ]  # drop first six rows
colnames(LPSdat) <- c("genes",      # gene names
                      "B.ctrl",     # Cell types are taken from
                      "B.LPS",      # Figure 4 of Jaitin et al.
                      "MF.ctrl",    # .ctrl and .LPS refer to control
                      "MF.LPS",     #   and LPS challenge
                      "NK.ctrl",    # The cell types are:
                      "NK.LPS",     #   B:    B-cell
                      "Mo.ctrl",    #   MF:   Macrophage
                      "Mo.LPS",     #   NK:   Natural killer cell
                      "pDC.ctrl",   #   Mo:   Monocyte
                      "pDC.LPS",    #   pDC:  plasmacytoid dendritic cell
                      "DC1.ctrl",   #   DC1:  dendritic cell subtype 1
                      "DC1.LPS",    #   DC2:  dendritic cell subtype 2
                      "DC2.ctrl",   #
                      "DC2.LPS",    #
                      "cluster")    # Gene assigned to cluster by authors
rownames(LPSdat) <- 1:nrow(LPSdat)

str(LPSdat)

for (i in 2:ncol(LPSdat)) { # convert number columns to numeric
    LPSdat[,i] <- as.numeric(LPSdat[ ,i])
}

rm(rawDat)

# confirm
head(LPSdat)
str(LPSdat)

objectInfo(LPSdat)

# Now, if everything is as it should be, let's save the object so we can
# easily reload it later.

save(LPSdat, file = "LPSdat.RData")

# rm(LPSdat)
# head(LPSdat)

# load("LPSdat.RData")
# head(LPSdat)


# [END]
