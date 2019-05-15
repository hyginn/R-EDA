# grepSrc.R

# cf. https://gist.github.com/hyginn/80bffa8ad1428e8b84591c760e96e875

#' grepSrc
#'
#' \code{grepSrc} Utility to grep source files for the presence of a string.
#'
#' @section Details: The function checks all files in directories \code{path}
#'                   with extensions \code{ext} for the presence of matches
#'                   to the regular expression \code{patt}. For each match
#'                   the line and filename are printed.
#'
#' @param patt (char)  a regular expression that should match a string we are
#'                     looking for.
#' @param ext  (char)  a vector of regular expressions. Files with filenames
#'                     matching any of the expressions will be included.
#'                     Defaults to \code{c("\\.R$")}. If the word "all" is
#'                     entered instead of a regualr expression, all files on
#'                     the path that the system considers "text" files are
#'                     scanned.
#' @param paths (char) a vector of paths. Defaults to
#'                     \code{c(".", "./R", "./dev", "./inst/scripts",
#'                             "./doc", "./src", "./tests/testthat")}.
#' @return NULL (invisible). Invoked for the side-effect of printing a report
#'                           to console.
#'
#' @author \href{https://orcid.org/0000-0002-1134-6758}{Boris Steipe} (aut)
#'
#' @examples
#' # Find all occurrences of a variable named "var" -  i.e. the string "var"
#' # between two word boundaries "\\b" - in ".R" files in source directories.
#' grepSrc("\\bvar\\b")
#'

grepSrc <- function(patt,
                    ext   = c("\\.R$"),
                    paths = c(".", "./R", "./dev", "./inst/scripts",
                             "./doc", "./src", "./tests/testthat")) {

  N <- 80  # max numbers of characters to print from matching line

  checkText <- function(f) {
    return(grepl("^.*: .+\\btext\\b", system(sprintf("file %s", f), intern = TRUE)))
  }

  grepAllText <- FALSE
  if (ext == "all") {
    grepAllText <- TRUE
    ext <- "*"
  }


  # make vector of files to process
  myFiles <- character()
  for (myPath in paths) {
    for (myExt in ext) {
      myFiles <- c(myFiles, list.files(path = gsub("/$", "", myPath),
                                       pattern = myExt,
                                       full.names = TRUE))
    }
  }

  if (grepAllText) {
    sel <- sapply(myFiles, FUN = checkText)
    myFiles <- myFiles[sel]
  }

  for (myFile in myFiles) {                   # For all requested files ...
    src <- readLines(myFile)                  #   read the sourcecode,
    indices <- grep(patt, src)                #   get indices for matches ...
    if (length(indices) > 0) {                #     If there are any ...
      cat(sprintf("File: \"%s\"\n", myFile))  #       print the filename ...
      for (idx in indices) {                  #       and N characters.
        cat(sprintf("# %d\t%s\n", idx, substr(src[idx], 1, N)))
      }
    }
  }

  return(invisible(NULL))
}

# [END]
