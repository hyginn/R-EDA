# objectInfo.R
#
# Purpose:  Display R object information
#
# Author:   Boris Steipe (boris.steipe@utoronto.ca)
# License:  GPL-3
#
# Date:     2018-05-09
#
# Version:  1.1
#
# Version history:
#   1.1    Truncate ouput for large objects
#   1.0.1  Maintenance
#   1.0    First version
#
# ToDo:
#
# Notes:
#
# ==============================================================================

objectInfo <- function(x) {
	# Purpose:
	#     Print information items about an R object
    #
    # Parameters:
	#     x: an R object
    # Value:
	#     NA. Invoked for side-efect of printing information.
	# Details:
	#     Print: contents, structure, mode/type/class, and attributes.
	#     If length(x) is larger than PRINTMAX, show only head() and tail()

    PRINTMAX <- 8

    cat("object contents:\n")
    if (length(x) <= PRINTMAX) {
        print(x, digits = 22)  # print value(s) at maximal precision
    } else if (is.null(dim(x))) { # long vector, truncate
        l <- length(x)
        print(x[1:round(PRINTMAX/2)], digits=22)
        cat(sprintf("     ... %d more elements ...\n", l - PRINTMAX))
        print(x[(l - round((PRINTMAX/2) - 1)):l], digits=22)

    } else { # 2- or more dimensional object. Use head(), tail() at standard
             # precision.
        print(head(x, round(PRINTMAX/2)))
        cat("     ... \n")
        print(tail(x, round(PRINTMAX/2)))
    }


    cat("\nstructure of object:\n")
    str(x)

    if (! is.list(x)) { # Don't use cat() if x is a list. cat() can't handle lists.
        cat("\nmode:   ", mode(x), "\n")
        cat("typeof: ", typeof(x), "\n")
        cat("class:  ", class(x), "\n")
    }

    # if the object has attributes, print them too
    if (! is.null(attributes(x))) {
        cat("\nattributes:\n")
        attributes(x)
    }
    # Done
}


# ====  EXAMPLES  ==============================================================
# Write your usage examples here...

if (FALSE) {
    objectInfo(pi)
}

# ====  TESTS  =================================================================
# Enter your function tests here...

if (FALSE) {
    # compare printed output with expected:

    objectInfo(pi)                   # numeric
    objectInfo(runif(10))            # vector
    objectInfo(summary(runif(1000))) # complex object

    # data frame: note - the char vector is converted to factors
    objectInfo(data.frame(int  = c(13L, 17L, 19L),   # data frame:
                          num  = c((1 + sqrt(5))/2, exp(1), pi),
                          bool = c(TRUE, TRUE, FALSE),
                          char = c("Aardvark", "Armadillo", "Antelope")))
    #list
    objectInfo(list(int  = 57L,
                    num  = c((1 + sqrt(5))/2, exp(1)),
                    bool = c(TRUE, TRUE, FALSE),
                    char = c("Aardvark", "Armadillo", "Antelope", "Aulk")))

}

# [END]
