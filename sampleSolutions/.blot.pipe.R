## Sample solution >>>                                               (blot.pipe)

##      - rows 1:10 of the first two columns in reverse order

LPSdat[10:1, 1:3]

##      - gene names and the expression values for Mo.LPS
##         for the top ten expression values
LPSdat[order(LPSdat$Mo.LPS, decreasing = TRUE)[1:10], c("genes", "Mo.LPS")]

#      - All genes for which B-cells are stimulated by LPS by
#        more than 2 log units.
LPSdat[(LPSdat$B.LPS - LPSdat$B.ctrl) >= 2, "genes"]

#... Show the values too
x <- cbind(LPSdat$genes,(LPSdat$B.LPS - LPSdat$B.ctrl))
x[(LPSdat$B.LPS-LPSdat$B.ctrl) >= 2, ]

#      - Expression values for all genes whose gene-names appear in Figure 3B.
#        (hint: use the  %in%  operator)


## <<< Sample solution
# [END]
