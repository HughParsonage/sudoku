#' Standardize a sudoku
#' @param X A matrix.
#' @return X but NAs for missing values, (not zeroes).

set_sudoku <- function(X) {
  eval.parent(substitute({
    if (min(X, na.rm = TRUE) == 0L) {
      # 0's should be zero
      X[X == 0] <- NA
    }
    if (is.double(X)) {
      # make integer
      storage.mode(X) <- "integer"
    }
    class(X) <- "sudoku"
    X
  }))
}
