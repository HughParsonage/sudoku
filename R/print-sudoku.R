#' Print a sudoku
#' @param X Numeric \eqn{9\times 9} matrix with integer values representing
#' a sudoku puzzle.
#' @param H Logical \eqn{9\times 9} matrix with logical values, \code{TRUE} for
#'  \strong{h}ighlighted entries.
#' @param hi,hj Alternative to \code{H} to highlight specific cells
#' @param filename Where to print. If \code{filename = ""}, the default,
#' printing is done to the console. Otherwise a filename.
#'
#' @examples
#' eg({
#'   M2 <- sudoku::generateSudoku(Nblank = 25)
#'   print_sudoku(M2)
#' })
#'
#' eg({
#'   M3 <- sudoku::solveSudoku(M2)
#'   print_sudoku(M2, M3 != M2)
#' })
#'
#' @export

print_sudoku <- function(X, H = NULL, hi = NULL, hj = NULL, filename = "") {
  if (is.null(H)) {
    H <- matrix(FALSE, nrow = 9, ncol = 9)
  }
  stopifnot(#is.integer(X),
    is.logical(H),
    is.matrix(X),
    is.matrix(H),
    !anyNA(H))
  if (is.null(filename)) {
    cat <- function(...) invisible(NULL)
  } else if (nzchar(filename) || !requireNamespace("crayon", quietly = TRUE)) {
    cat <- function(...) base::cat(..., file = filename, append = TRUE)
    Red <- identity
  } else {
    Red <- crayon::red
  }

  for (i in 1:9) {
    if ({i %% 3L} == 1L) {
      cat("\n")
    }
    for (j in 1:9) {
      if ({j %% 3L} == 1L) {
        cat(" ")
      }
      if (is.na(X[i, j]) || X[i, j] == 0) {
        if (H[i, j] || {i %in% hi && {j[i %in% hi] %in% hj}}) {
          cat(Red(" X "))
        } else {
          cat(" . ")
        }
      } else if (H[i, j] || {i %in% hi && {j[i %in% hi] %in% hj}}) {
        cat(" ", Red(X[i, j]), " ", sep = "")
      } else {
        cat(" ", X[i, j], " ", sep = "")
      }
      if (j == 9L) {
        cat("\n")
      }

    }
  }
}

#' @rdname print_sudoku
#' @export
print.sudoku <- function(x, ..., H = NULL, hi = NULL, hj = NULL) {
  print_sudoku(x, H, hi, hj)
  invisible(x)
}



