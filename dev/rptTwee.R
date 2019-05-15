# rptTwee.R

#' rptTwee
#'
#' \code{rptTwee} A simple file-and-directory tree
#'
#' A simple utility to print a tree of a directory's files and subdirectories
#' to the console, suitable to be included in documentation.
#'
#' @section Details: this function was inspired by Jenny Bryan's
#'\href{https://gist.github.com/jennybc/2bf1dbe6eb1f261dfe60}{Twee Gist},
#' enhanced to include hidden files and exclude some files and directories
#' from listing. It's primary purpose is to print a directory tree of an
#' RStudio project, including the parent directory, but without the
#' .git and .Rproj.user directories (since RStudio does not include those
#' two in the files pane). \code{rptTwee()} also removes the OS specific
#' .DS_store files.
#'
#' @param path (char) Path of the root directory of the tree. Defaults to the
#'                    return value of \code{getwd()}.
#' @param showHidden (bool) if TRUE, show hidden files and directories.
#' @param showRd  (bool) if TRUE, show the root directoryof the tree.
#' @param lev (int) Limit depth of recursive listing to \code{lev}. Default
#'                  to \code{Inf}: don't limit depth.
#' @param excl (char) Vector of regular expressions for files and directories
#'                    that are excluded from the tree. Defaults to exclude
#'                    the files and directories that are not shown in the
#'                    RStudio files pane.
#' @return NULL (invisibly)  Invoked for the side-effect of printing the
#'                           tree to console.
#'
#' @author \href{https://orcid.org/0000-0002-1134-6758}{Boris Steipe} (aut)
#'
#' @seealso \code{\link{list.files}}
#'
#' @examples
#' # Tree of the working directory, limited to two levels
#' rptTwee(levels = 2)
#'

rptTwee <- function(path = getwd(),
                    showHidden = TRUE,
                    showRd = TRUE,
                    lev = Inf,
                    excl = c("^\\.git$",
                             "^\\.git/",
                             "^\\.Rproj.user",
                             "\\.DS_Store$",
                             "\\.o$",
                             "\\.so$")) {

  fad <-  list.files(path = path,
                     recursive = TRUE,
                     no.. = TRUE,
                     all.files = showHidden,
                     include.dirs = TRUE)

  # remove files and directories listed in excl
  sel <- grepl(paste("(", excl, ")", sep = "", collapse = "|"), fad)
  fad <- fad[! sel]

  # remove entries that are too deep
  sel <- unlist(lapply(regmatches(fad, gregexpr("/", fad)), length))  >= lev
  fad <- fad[! sel]

  if (showRd) {
    # prepend root directory
    Rd <- gsub("^.*/(.+)$", "\\1", path)
    fad <- paste(Rd, fad, sep = "/")
    fad <- c(Rd, fad)
  }

  # prepare to add a backslash to directories
  dirMarks <- ifelse(dir.exists(paste0("../", fad)), "/", "")

  # add dirMarks to force correct order for directories and containing files,
  # then reorder fad and dirMarks
  fad <- paste0(fad, dirMarks)
  oFad <- order(fad)
  fad <- fad[oFad]
  dirMarks <- dirMarks[oFad]

  # split fad and replace levels with lines
  fad <- strsplit(fad, "/")

  makeLines <- function(x) {
    l <- length(x)
    if (l == 1) {
      x <- c(" --", x)
    } else {
      x <- c(rep("   ", l - 1), "|__", x[l])
    }
    return(paste0(x, collapse = ""))
  }

  fad <- unlist(lapply(fad, makeLines))
  fad <- paste0(fad, dirMarks)  # add dirmarks back

  cat(fad, sep = "\n")   # cat result

  return(invisible(NULL))

}

# [END]
