# insertSnip.R

#' insertSnip
#'
#' \code{updateTOC} Insert a code snip to replace the current line in the
#'                  focus document
#'
#' @section Details:
#'   Somewhat experimental. Writes a backup copy to tempfile.
#'
#' @param id   character    The ID of the snip. (Usually two Q-words.)
#'                          Expanded to "./sampleSolutions/.<id>.R"
#'
#' @param updateTOC   logical    Update the TOC. Default: TRUE
#'
#' @return NULL (invisible). Invoked for the side-effect of modifying the
#'                           focus document.
#'
#' @author \href{https://orcid.org/0000-0002-1134-6758}{Boris Steipe} (aut)
#'

insertSnip <- function(id, updateTOC = TRUE) {

  rstudioapi::documentSave()
  thisScript <- rstudioapi::getActiveDocumentContext()
  pos <- grep(id, thisScript$contents)
  stopifnot(length(pos) == 1)

  thisSnip <- file.path(getwd(), "sampleSolutions", paste0(".", id, ".R"))
  stopifnot(file.exists(thisSnip))
  snip <- readLines(thisSnip)
  snip <- snip[-length(snip)] # drop "# [END]" marker

  # make a backup copy
  writeLines(thisScript$contents, tempfile())

  writeLines(c(thisScript$contents[1:(pos - 1)],
               snip,
               thisScript$contents[(pos + 1):length(thisScript$contents)]),
               thisScript$path)

  if (updateTOC) {
    addTOC(inFile = thisScript$path, path = "")
  }

  return(invisible(NULL))

}

# [END]
