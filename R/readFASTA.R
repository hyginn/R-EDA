# readFASTA.R
#
# Purpose:  read a FASTA sequence file
#
# Author: Boris (boris.steipe@utoronto.ca)
# License: GPL-3
#
# Date: 2018-05-14
#
# Version:    0.1
#
# Version history:
#    0.1 Firt draft
#
# ToDo:
#   Check for valid FASTA format
#
# Notes:
#
# ==============================================================================

readFASTA <- function(fn) {
	# Purpose:
	#     Read a FASTA file, return a vector of single sequence letters
    #
    # Parameters:
	#     fn: chr   filename of the input file

    # Value:
	#     chr: single letters of the FASTA sequence
	# Details:
	#     We discard the header

	# read the file identified by fn
    tmp <- readLines(fn)
	# discard the header
    tmp <- tmp[-1]

	# collate it into a single string
    tmp <- paste(tmp, collapse = "")

    # break it into single characters
    v <- strsplit(tmp, split = "")[[1]]

	# return the result
	return(v)
}


# ====  EXAMPLES  ==============================================================
# Write your usage examples here...

if (FALSE) {
    myFile <- tempfile()
    writeLines(c("> test", "CAA", "ttg"), con = myFile)

    readFASTA(myFile)
}

# ====  TESTS  =================================================================
# Enter your function tests here...

if (FALSE) {
  # test ...
}

# [END]
