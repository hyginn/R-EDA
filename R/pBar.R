#pBar.R

#' \code{pBar} draws a progress bar in the console.
#'
#' @param i (int) The current iteration
#'
#' @param l (int) The total number of iterations
#'
#' @param nCh (int) The width of the progress bar (number of characters).
#' Defaults to 50.
#'
#' @return None. Side effect is to draw a progress bar in the console.
#'
#' @examples
#' for (i in 1:100) {
#'   pBar(i, 100)
#'   Sys.sleep(0.07)
#' }
#'
#' @export

pBar <- function(i, l, nCh = 50) {
  ticks <- round(seq(1, l-1, length.out = nCh))
  if (i < l) {
    if (any(i == ticks)) {
      p <- which(i == ticks)
      p1 <- paste(rep("#", p), collapse = "")
      p2 <- paste(rep("-", nCh - p), collapse = "")
      cat(sprintf("\r|%s%s|", p1, p2))
      utils::flush.console()
    }
  }
  else { # done
    cat("\n")
  }
}
