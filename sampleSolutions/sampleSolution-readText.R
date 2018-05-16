# readText.R
#
# Purpose:  Read contents of a small text-file into R
#
# ==============================================================================

#TOC> ==========================================================================
#TOC>
#TOC>   Section  Title                                                   Line
#TOC> -----------------------------------------------------------------------
#TOC>   1        Solution 1: Enter data into code, element by element      19
#TOC>   2        Solution 2: Enter data "by hand" all at once              44
#TOC>   3        Solution 3: Use the readLines() function                 105
#TOC>   4        Solution 4: Use the read.csv() function                  113
#TOC>
#TOC> ==========================================================================


# =    1  Solution 1: Enter data into code, element by element  ================
#
# 1. use a text-processor to replace each occurrence of a
#    paragraph-break with the string ", ".
# 2. wrap the string with c() and assign it.

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

# Note: you need a text processor that does _not_ change quotationmarks, does
# _not_ add formatting, does _not_ play any of the shenanigans that are designed
# to support secretarial tasks, and does _not_ hide the essential details of
# your file from you (like the file extension). Fortunately, you already have
# one: you can use RStudio as a code editor, but also as a plain-text editor!



# =    2  Solution 2: Enter data "by hand" all at once  ========================

# 1. Copy/paste the contents of Fig_3-CharacteristicGenes.txt
# 2. Place it all into quotation marks
# 3. Assign this multi-line string a variable
# 4. Use strsplit() to split it into elements
# 5. unlist() the result
# 6. assign the result


s <- "Cd19
Cd79b
Cd22
Cd37
Ctsd
Apoe
C1qa
C1qb
C1qc
Csf1r
Slpi
Tlr2
Mmp13
Marco
Ifng
Gzmb
Myc
Xcl1
Ccl5
Gzma
Nkg7
Spic
Cebpb
Lyz2
Sfpi1
Nfkbiz
Bst2
Siglech
Ly6d
Irf8
Cst3
Naaa
Ccr7
Cxcl9
Traf1
Relb
Itgax
Tmem176b
Tnf
Tnfaip3
Nfkbia
Il15
Cxcl10
Ifit1
Isg15
Irf7"

charGenes <- unlist(strsplit(s, "\n"))        #... or
charGenes <-        strsplit(s, "\n")[[1]]



# =    3  Solution 3: Use the readLines() function  ============================

charGenes <- readLines("./data/Fig_3-CharacteristicGenes.txt")

# Note: this produces a vector of strings



# =    4  Solution 4: Use the read.csv() function  =============================
charGenes <- read.csv("./data/Fig_3-CharacteristicGenes.txt",
                      header = FALSE,
                      stringsAsFactors = FALSE)

# Note: this produces a dataframe. This is super useful if we need to work
# with data from an Excel spreadsheet! Simply save the data as csv to then
# import it into R.

charGenes$V1
charGenes[ , "V1"]
unlist(charGenes)

# [END]
