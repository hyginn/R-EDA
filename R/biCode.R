# biCode.R
#
# Purpose:  A 5-character code from a binomial name
#
# Author: Boris Steipe (boris.steipe@utoronto.ca)
# License: GPL-3
#
# Date: 2017-10
#
# Version: 1.0
# Version history:
#   1.0  Standard course version
#
# ToDo:
#
# Notes:
#
# ==============================================================================

biCode <- function(s) {
	# Purpose:
	#     Make a 5 character "biCode" from a binomial name by concatening
    #     the uppercased first three letter of the first word and the first
    #     two letters of the second word.
    #
    # Parameters:
	#     s: chr  vector of binomial species names
    # Value:
	#     chr  vector of biCodes, same length as s, NAs are preserved
	# Details:
	#     If there is only one word, we take the first five characters from
	#     that. Outputs are padded with "." if necessary. NAs in input are
	#     preserved.

    b <- character(length(s))
    s <- gsub("[^a-zA-Z ]", "", as.character(s)) # remove all non-alphabetic
                                                 # characters except space
    s <- toupper(s)

    for (i in seq_along(s)) {
        x <- unlist(strsplit(s[i], "\\s+"))
        if (length(x) == 0) { # empty string
            x <- c("", "")
        } else if (length(x) == 1) { # only one string
            x <- c(substr(x, 1, 3), substr(x, 4, 5)) # 3 + 2 with whatever is there
        }
        x <- paste0(x[1:2], "...")  # pad strings

        b[i] <- paste0(substr(x[1], 1, 3), substr(x[2], 1, 2))
    }

    b[is.na(s)] <- NA  # recover NAs from input

    return(b)
}


# ====  EXAMPLES  ==============================================================
# Write your usage examples here...

if (FALSE) {
    biCode(c("Homo sapiens", "Saccharomyces cerevisiae S288C"))
}

# ====  TESTS  =================================================================
# Enter your function tests here...

if (FALSE) {
    expect_equal(biCode(""), ".....")
    expect_equal(biCode(" "), ".....")
    expect_equal(biCode("123 12"), ".....")
    expect_equal(biCode("h sapiens"), "H..SA")
    expect_equal(biCode("homo sapiens"), "HOMSA")
    expect_equal(biCode("[homo sapiens neanderthaliensis]"), "HOMSA")
    expect_equal(biCode(c("Phascolarctos cinereus", "Macropus rufus")),
                 c("PHACI", "MACRU"))
    expect_error(biCode(), "argument \"s\" is missing, with no default")

    test_that("NA values are preserved", {
        expect_true(is.na((biCode(NA))))
        expect_equal(biCode(c("first", NA, "last")),
                     c("FIRST", NA, "LAST."))
    })
}

# [END]
