#' Run an example
#' @description Used for convenience rather than wrapping everything
#' in \code{requireNamespace}.
#' @param expr An expression
#' @export eg

eg <- function(expr) {
  if (requireNamespace("sudoku", quietly = TRUE)) {
    eval.parent(substitute(expr))
  }
}


