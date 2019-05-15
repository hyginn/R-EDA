# addTOC.R
#
# Purpose: add a table of contents (TOC) and canonically formatted section
#          headers to an R-script
#
# Version: 1.5
#
# Date:    2016 07 - 2019 05
# Author:  Boris Steipe (boris.steipe@utoronto.ca)
#
# V 1.5    Don't add path if empty (needed to correctly use fully qualified
#            path returned from rstudioapi::getActiveDocumentContext()$path)
# V 1.4    Indent subheadings in the TOC
# V 1.3    Made TOC replacement space-safe with defined two empty lines
#            before and after.
# V 1.2    Add ABCDIR as default path
# V 1.0    First code
#
# TODO:    - Add entry for last line (stop if marker has text following)
#          - Change logic to decimal level indicators
#          - Handle multi-line section level formatting
#          - Validate source file
#          - Capitalize level 1 but not if it's a function name.
#
# ==============================================================================

addTOC <- function(inFile,
                   outFile,
                   path = getwd(),
                   lineWidth = 80,
                   tocMarker = "#TOC> ",
                   srcTocMarker = "#TOC>") {
    # Purpose:
    #     Describe ...
    # Parameters:
    #     inFile: fully qualified file name for input src
    #     outFile: fully qualified file name for input src; if missing, it is
    #              set to inFile
    #     path: directory name of path. Default is getwd().
    #     lineWidth: pad lines to this width
    #     tocMarker: marks TOC blocks in output
    #     srcTocMarker: lines beginning with this marker are droppped before
    #                   processing.
    # Value:
    #     NULL (invisible)
    # Postcondition:
    #     A new version of the input source is created with formatted
    #     and numbered section headers and a TOC, and written to disk.

    # ====  PARAMETERS  ========================================================
    #

    minSectionNumberWidth <- 7   # Need to set a minimum width to account for
                                 # the word "Number" in the TOC header.
    minSectionTitleWidth <- 5    # Same for section title.

    # output filename ...
    if (missing(outFile)) {
        outFile <- inFile
    }

    if (path != "") {
      inFile  <- file.path(path, inFile)
      outFile <- file.path(path, outFile)
    }

    # Regular expression that triggers recognition of a section-header in the
    # source. Matches e.g.
    #     "# = 1  HEADER                               "
    #     "# === 1.1.1 Header text"
    #     "# = 1.2.3.4.       Header text              "
    sectionHeaderRegex <- paste0("^#",         #  Line begins with #
                                 "\\s*",       #  optional white space
                                 "=+",         #  one or more =
                                 "\\s*",       #  optional white space
                                 "([0-9.]+)",  #  capture section number con-
                                               #    sisting of digit(s) and
                                               #    period(s)
                                 "\\s*",       #  optional white space
                                 "(.+)$"       #  capture remaining as header
                                )

    # sprintf() template to format a section entry in the TOC:
    tocEntryTemplate <- "%s  %s  %s   %5s"


    # === Helper functions =====================================================
    strPadRight <- function(s, ch, width) {
        # Pad string "s" to a width of "width" with character "ch"
        # and return it. Return "s" unchanged if it is not shorter than
        # "width".
        if (nchar(s) < width) {
            s <- sprintf("%s%s",
                         s,
                         strrep(ch, width - nchar(s)))
        }
        return(s)
    }

    strRev <- function(s) {
        # Return the reverse of a string.
        s <- paste(rev(strsplit(s, split = "")[[1]]), collapse = "")
        return(s)
    }

    makeSectionNumber <- function(preceding, level) {
        # Return a "." separated string of a hierarchical section number,
        # appropriately incremented from the "preceding" number and having
        # "level" number of components.
        # To make a first section, set "preceding" to "0" and "level" to 1.
        preceding <- unlist(strsplit(preceding, "\\."))
        if (length(preceding) >= level) {
            number <- preceding[1:level]         # truncate to current level
            number[level] <- as.numeric(number[level]) + 1  # increment last component
        } else {
            # add a new level at arbitrary depth
            number <- c(preceding, rep("1", level - length(preceding)))
        }
        return(paste(number, collapse = "."))
    }


    # Formatting functions:
    # formatLevel functions specify how the section heading for
    # a particular level is to be formatted.
    #   == Level 1
    formatLevel      <- list( function(row, lmk = " =    ") {
        l <- sprintf("#%s%s%s",
                     lmk,
                     gsub("\\s*$", "  ", row$number),
                     gsub("\\s*$", "  ", row$heading))
        l <- strPadRight(l, "=", lineWidth)
        return(l)
    })

    #   == Level 2
    formatLevel[[2]] <- function(row, lmk = " ==   ") {
        l <- sprintf("#%s%s%s",
                     lmk,
                     gsub("\\s*$", "  ", row$number),
                     gsub("\\s*$", "  ", row$heading))
        l <- strPadRight(l, "=", lineWidth)
        return(l)
    }

    #   == Level 3
    formatLevel[[3]] <- function(row, lmk = " ===   ") {
        l <- sprintf("#%s%s%s",
                     lmk,
                     gsub("\\s*$", "  ", row$number),
                     row$heading)
        return(l)
    }

    #   == Level 4 and greater
    formatLevel[[4]] <- function(row, lmk = " ====  ") {
        l <- sprintf("#%s%s%s",
                     lmk,
                     gsub("\\s*$", "  ", row$number),
                     row$heading)
        return(l)
    }

    # formatTOCstring functions specify how the section heading for
    # a particular level is to be formatted.

    formatTOCstring <- list()

    #   == Level 1
    formatTOCstring[[1]] <- function(h) {
        s <- sprintf("%s      ", h)
        return(s)
    }

    #   == Level 2
    formatTOCstring[[2]] <- function(h) {
        s <- sprintf("  %s    ", h)
        return(s)
    }

    #   == Level 3
    formatTOCstring[[3]] <- function(h) {
        s <- sprintf("    %s  ", h)
        return(s)
    }

    #   == Level 4 and greater
    formatTOCstring[[3]] <- function(h) {
        s <- sprintf("      %s", h)
        return(s)
    }






    # ==== Initializations =====================================================

    # formatting to  precede and follow the TOC:
    tocPrefix <- c("",  # two empty lines before TOC
                   "",
                   strPadRight(tocMarker, "=", lineWidth),
                   tocMarker)

    tocSuffix <- c(tocMarker,
                   strPadRight(tocMarker, "=", lineWidth),
                   "",  # two empty lines after TOC
                   "")


    # ==== Process =============================================================

    #  === Read file
    src <- readLines(inFile)

    #
    #  === Drop old TOC
    src <- src[! grepl(paste0("^", srcTocMarker), src)]

    #  === Extract all section headings and put them in "sections"
    sections <- data.frame()
    for (i in seq_along(src)) {
        if(grepl(sectionHeaderRegex, src[i])) {
            # process pattern match
            lvl <- regmatches(src[i], regexec(sectionHeaderRegex, src[i]))[[1]][2]
            lvl <- length(unlist(strsplit(lvl, "\\.")))
            txt <- regmatches(src[i], regexec(sectionHeaderRegex, src[i]))[[1]][3]
            sections <- rbind(sections,
                              data.frame(line = i,
                                         level = lvl,
                                         number = "",
                                         heading = txt,
                                         stringsAsFactors = FALSE))
            N <- nrow(sections)
            if (N == 1) {
                sections$number[N] <- makeSectionNumber("0",
                                                        sections$level[N])
            } else {
                sections$number[N] <- makeSectionNumber(sections$number[N-1],
                                                        sections$level[N])
            }
        }
    }

    # remove trailing "=" and space
    for (i in 1:nrow(sections)) {
    	sections$heading[i] <- gsub("[= ]+$", "", sections$heading[i])
    }


    #  === Pad numbers with leading zeroes wherever the max. number of digits
    #      requires it.
    maxSectionLevel <- max(sections$level)

    maxNumberDigits <- numeric(maxSectionLevel)
    for (i in 1:nrow(sections)) {
        num <- as.numeric(unlist(strsplit(sections$number[i], "\\.")))
        for (j in 1:length(num)) {
            maxNumberDigits[j] <- max(maxNumberDigits[j], num[j])
        }
    }

    maxNumberDigits <- nchar(maxNumberDigits)

    for (i in 1:nrow(sections)) {
        num <- unlist(strsplit(sections$number[i], "\\."))
        for (j in 1:length(num)) {
            num[j] <- sprintf("%0*d", maxNumberDigits[j], as.numeric(num[j]))
        }
        sections$number[i] <- paste(num, collapse = ".")
    }

    #  === Normalize widths
    # Bring all numbers and all headings to the same resp. width by right-
    # padding with " ".
    maxNumberWidth <- max(nchar(sections$number), minSectionNumberWidth)
    maxHeadingWidth <- max(nchar(sections$heading), minSectionTitleWidth)

    for (i in 1:nrow(sections)) {
        sections$number[i]  <- strPadRight(sections$number[i],  " ", maxNumberWidth)
        sections$heading[i] <- strPadRight(sections$heading[i], " ", maxHeadingWidth)
    }


    #  === Reformat section headings in output

    for (i in 1:nrow(sections)) {
        l <- min(sections$level[i], length(formatLevel))
        o <- sections$line[i]
        src[o] <- formatLevel[[l]](sections[i, ])
    }


    #   === Create TOC block
    toc <- tocPrefix

    #    == Make header line
    lvl1 <- min(sections$level[1], length(formatTOCstring))
    lTxt <- nchar(formatTOCstring[[lvl1]](sections$heading[1]))

    toc <- c(toc,
             sprintf(tocEntryTemplate,
                     tocMarker,
                     strPadRight("Section", " ", nchar(sections$number[1])),
                     strPadRight("Title",   " ", lTxt),
                     strPadRight("Line",    " ", nchar(sections$line[1]))))

    #    == Make separator line
    toc <- c(toc,
             strPadRight(tocMarker, "-", nchar(toc[length(toc)])))

    #    == calculate section number offset
    offset <- length(toc) + nrow(sections) + length(tocSuffix) - 4
    # subtract four for the four empty lines added in tocPrefix and tocSuffix,
    # but not removed from source when we deleted the old TOC.

    #    == write all TOC entries
    for (i in 1:nrow((sections))) {

        l <- min(sections$level[i], length(formatTOCstring))
        txt <- formatTOCstring[[l]](sections$heading[i])
        toc <- c(toc,
                 sprintf(tocEntryTemplate,
                         tocMarker,
                         sections$number[i],
                         txt,
                         sections$line[i] + offset))
    }

    #   == Add suffix
    toc <- c(toc, tocSuffix)


    #  === Insert TOC into source
    #   == Find insertion point: right after first comment block
    TOCAfter <- rle(grepl("^#", src))[[1]][1]
    runs <- rle(grepl("^\\s*$", src))
    i <- which(runs$values)[1]  # index of runs of TRUE
    TOCBefore <- TOCAfter + runs$lengths[i]  + 1 # length of first TRUE run

    #   == Splice TOC into source
    src <- c(src[1:TOCAfter],
             toc,
             src[TOCBefore:length(src)])

    #  === Write source to output
    writeLines(src, outFile)

    # ==== Done ================================================================
    return(invisible(NULL))
}



# ====  TESTS  =================================================================
# ...


# [END]
