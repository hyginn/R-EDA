## Sample solution >>>                                               (file.dome)

## Read the csv file
tmp <- read.csv("./data/table_S3.csv",
                header = FALSE,
                as.is = TRUE)

## The object "tmp" should appear in the Data section of the Environment tab
## in the top-right pane. It has a spreadsheet symbol next to it. Click that -
## or type View(rawDat), and study the object. You should find:
## - all columns are named V<something>
## - rows 1 to 6 do not contain data
## - there is not a single row that could be used for column names
## - str(tmp) shows: all columns are characters

## This all needs to be fixed:

head(tmp, 10)                    # Use head() to inspect
tmp <- tmp[-(1:6), ]             # Remove unneeded header rows

colnames(tmp) <- c("genes",      # gene names
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

head(tmp)
rownames(tmp) <- 1:nrow(tmp)     # fix rownames

str(tmp)                         # next: fix the column types

for (i in 2:ncol(tmp)) {         # convert character columns to numeric
  tmp[,i] <- as.numeric(tmp[ ,i])
  if (any(is.na(tmp[,i]))) {
    message(sprintf("Caution: NA in column %d", i))   # always validate!
  }
}

str(tmp)

## if everything is good ...

LPSdat <- tmp  # assign to meaningful name
rm(tmp)        # always a good idea to clean up


## <<< Sample solution
# [END]
