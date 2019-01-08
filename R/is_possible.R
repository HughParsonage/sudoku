#' Is a particular value possible?
#' @return \code{FALSE} if the value is impossible, \code{TRUE} if possible but not certain.
#' @export is_possible

is_possible <- function(X, i, j, v, depth = 1L) {
  v <- as.integer(v)
  # If value already present, value must match
  if (!is.na(X[i, j])) {
    return(X[i, j] == v)
  }

  allowed <- setdiff(1:9, c(X[i, ], X[, j], zoom2(X, i, j)))

  # Test row and column
  if (v %notin% allowed) {
    return(FALSE)
  }
  M <- X
  M[i, j] <- v
  M <- tryCatch(solve(M, depth = depth - 1L),
                error = function(e) {
                  attr(X, "Impossible") <- TRUE
                  X
                },
                warning = function(e) {
                  attr(X, "Impossible") <- TRUE
                  X
                })
  if (has_failed(M)) {
    return(FALSE)
  }

  TRUE

}

