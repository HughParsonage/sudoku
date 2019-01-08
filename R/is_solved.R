#' Is a puzzle (completely) solved?
#' @param M A candidate puzzle.
#' @param quiet Don't emit messages.
#' @export

is_solved <- function(M, quiet = TRUE) {
  out <-
    is.matrix(M) &&
    !anyNA(M) &&
    min(M) == 1 &&
    max(M) == 9 &&
    colSums(M) == sum(1:9) &&
    rowSums(M) == sum(1:9)

  if (quiet) {
    message <- function(...) {
      invisible(NULL)
    }
  }

  if (!out) {
    return(FALSE)
  }

  for (i in 1:9) {
    if (length(xMi <- setdiff(1:9, M[i, ]))) {
      message("xMi ", i, xMi)
      return(FALSE)
    }
    if (length(xMj <- setdiff(1:9, M[, i]))) {
      message("xMj ", i, xMj)
      return(FALSE)
    }
  }

  for (i in -1:1) {
    for (j in -1:1) {
      if (sum(zoom(M, I = i, J = j)) != 45) {
        message("zoom ", i, j)
        return(FALSE)
      }
    }
  }
  TRUE
}

cells_ok <- function(X, na.ok = TRUE) {
  for (I in -1:1) {
    for (J in -1:1) {
      Z <- zoom(X, I, J)
      if (na.ok && anyNA(Z)) {
        next
      }
      if (sum(Z) != 45) {
        return(FALSE)
      }
      if (anyDuplicated(Z)) {
        return(FALSE)
      }
    }
  }
  TRUE
}

is_unsolved <- function(M) {
  anyNA(M) ||
    any(colSums(M) != 45L) ||
    any(rowSums(M) != 45L) ||
    max(apply(M, 1L, anyDuplicated, 0L)) ||
    max(apply(M, 2L, anyDuplicated, 0L)) ||
    !cells_ok(M)
}

has_failed <- function(M) {
  if (isTRUE(attr(M, "Impossible"))) {
    return(TRUE)
  }
  any(colSums(M, na.rm = TRUE) != 45L, na.rm = TRUE) ||
    any(rowSums(M, na.rm = TRUE) != 45L, na.rm = TRUE) ||
    max(apply(M, 1L, anyDuplicated, incomparables = NA, FUN.VALUE = 0L)) ||
    max(apply(M, 2L, anyDuplicated, incomparables = NA, FUN.VALUE = 0L)) ||
    !cells_ok(M)
}


