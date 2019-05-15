## Sample solution >>>

## Solution 1: Enter data into code, element by element  ================
##
## 1. use a text-processor to replace each occurrence of a
##    paragraph-break with the string ", ".
## 2. wrap the string with c() and assign it.

charGenes <- c("Cd19", "Cd79b", "Cd22", "Cd37", "Ctsd",
               "Apoe", "C1qa", "C1qb", "C1qc", "Csf1r",
               "Slpi", "Tlr2", "Mmp13", "Marco", "Ifng",
               "Gzmb", "Myc", "Xcl1", "Ccl5", "Gzma",
               "Nkg7", "Spic", "Cebpb", "Lyz2", "Sfpi1",
               "Nfkbiz", "Bst2", "Siglech", "Ly6d", "Irf8",
               "Cst3", "Naaa", "Ccr7", "Cxcl9", "Traf1",
               "Relb", "Itgax", "Tmem176b", "Tnf", "Tnfaip3",
               "Nfkbia", "Il15", "Cxcl10", "Ifit1", "Isg15",
               "Irf7")

## Note: you need a text processor that does _not_ change quotationmarks, does
## _not_ add formatting, does _not_ play any of the evil tricks that are
## designed to support secretarial tasks, and does _not_ hide the essential
## details of your file from you (like the file extension). Fortunately, you
## already have one: you can use RStudio as a code editor, but also as a
## plain-text editor! It's actually a really good text editor, since it
## supports multi-line coliumn selections.


## Solution 2: Use the read.csv() function  =============================
charGenes <- read.csv("./data/Fig_3-CharacteristicGenes.txt",
                      header = FALSE,
                      stringsAsFactors = FALSE)

## Note: this produces a dataframe. This is super useful if we need to work
## with data from an Excel spreadsheet! Simply save the data as csv to then
## import it into R.

charGenes$V1
charGenes[ , "V1"]
unlist(charGenes)


##  Solution 2: Use the readLines() function

charGenes <- readLines("./data/Fig_3-CharacteristicGenes.txt")

## Note: this produces a vector of strings

## <<< Sample solution
# [END]
