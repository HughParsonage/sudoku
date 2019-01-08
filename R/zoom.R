#' Zoom into a square
#' @param M A sudoku.
#' @param I,J Single integers, in \eqn{\{-1, 0, 1\}}. The origin is the centre square.
#' @param check Run-time assert.
#' @return The submatrix.
#' @export zoom


zoom <- function(M, I, J, check = TRUE) {
  if (check) {
    stopifnot(is.matrix(M),
              all(M %in% c(NA, 1:9), na.rm = TRUE),
              nrow(M) == 9L,
              ncol(M) == 9L,
              length(I) == 1L,
              length(J) == 1L,
              I == as.integer(I),
              J == as.integer(J),
              I %in% -1:1,
              J %in% -1:1)
  }

  ii <- 3 * I + 3 + 1:3
  jj <- 3 * J + 3 + 1:3

  M[ii, jj]
}

zoom2 <- function(M, i, j) {
  I <- if_else(i <= 3L,
               -1L,
               as.integer(i > 6L))
  J <- if_else(j <= 3L,
               -1L,
               as.integer(j > 6L))
  zoom(M, I, J, check = FALSE)
}
