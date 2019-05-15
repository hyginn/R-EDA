# updateTOC.R


#' updateTOC
#'
#' \code{updateTOC} Utility to use the RStudio API to save the file in
#' current focus and run addTOC on it.
#'
#' @section Details: Somewhat experimental.
#'
#' @return NULL (invisible). Invoked for the side-effect of updating the TOC.
#'
#' @author \href{https://orcid.org/0000-0002-1134-6758}{Boris Steipe} (aut)
#'

updateTOC <- function() {

  rstudioapi::documentSave()
  addTOC(inFile = rstudioapi::getActiveDocumentContext()$path, path = "")
  return(invisible(NULL))

}

# [END]
