#' Solve M
#' @param M An unsolved puzzle. Zeroes or \code{NA} values represent unsolved
#' values.
#' @param depth A nonnegative integer to limit the depth to which \code{\link{solve}}
#' will be attempted. If negative, a stack overflow may occur.
#' @return The solved puzzle
#' @export solve


solve <- function(M, depth = 3L) {
  X <- M
  class(X) <- "sudoku"
  X[X == 0L] <- NA
  if (is_solved(X)) {
    return(X)
  }
  Y <- NULL
  while (anyNA(X) &&
         !identical(X, Y)) {
    Y <- X
    X <- solve_byrowcol(X)
    X <- solve_zoom(X)
    X <- solve_both(X)
    if (is_almost_solved(X)) {
      X <- solve_almost_solved(X)
    }
    X <- solve_almost_solved3(X)
  }

  if (depth && !is_solved(X)) {
    X <- tryCatch(solve_almost_solved(X),
                  error = function(e) {
                    X
                  },
                  warning = function(e) {
                    X
                  })


    if (!is_solved(X)) {
      warning("Unsolved.")
    }
  }
  X
}

solve_byrowcol <- function(M) {
  stopifnot(anyNA(M))

  X <- M
  for (d in 1:2) {  # do twice in case gap appears
    for (i in 1:9) {
      if (sum(is.na(X[i, ])) == 1) {
        ij <- which(is.na(X[i, ]))
        X[i, ij] <- setdiff(1:9, X[i, ])
      }
      if (sum(is.na(X[, i])) == 1) {
        ji <- which(is.na(X[, i]))
        X[ji, i] <- setdiff(1:9, X[, i])
      }
    }
  }


  for (d in 1:2) {
    for (i in 1:9) {
      for (j in 1:9) {
        if (anyNA(Xij <- X[i, j])) {
          Xj <- X[, j]
          Xi <- X[i, ]

          # setdiff removes NAs
          if (length(Insert <- setdiff(1:9, c(Xj, Xi))) == 1L) {
            X[i, j] <- Insert
          }
        }
      }
    }
  }


  X
}

solve_zoom <- function(M) {
  for (z1 in -1:1) {
    for (z2 in -1:1) {
      z12 <- zoom(M, I = z1, J = z2, check = FALSE)
      if (sum(is.na(z12)) == 1L) {
        iz <- which(is.na(z12), arr.ind = TRUE)[, "row"]
        jz <- which(is.na(z12), arr.ind = TRUE)[, "col"]

        i <- 3L * z1 + 3L + iz
        j <- 3L * z2 + 3L + jz
        M[i, j] <- setdiff(1:9, z12)
      }
    }
  }
  M
}

solve_both <- function(M) {
  for (i in 1:9) {
    for (j in 1:9) {
      if (is.na(M[i, j])) {
        allowed <- setdiff(1:9, c(M[i, ], M[, j], zoom2(M, i, j)))
        if (length(allowed) == 1L) {
          M[i, j] <- allowed
        }
      }
    }
  }
  M
}

nAllowed <- function(M, i, j) {
  if (!is.na(M[i, j])) {
    return(1L)
  }
  allowed <- setdiff(1:9, c(M[i, ], M[, j], zoom2(M, i, j)))
  length(allowed)
}

firstAllowed <- function(M, i, j) {
  allowed <- setdiff(1:9, c(M[i, ], M[, j], zoom2(M, i, j)))
  allowed[1]
}

# AA <- as.data.frame(which(is.na(solve(M45)), arr.ind = TRUE))
# for (i in 1:NROW(AA)) {
#   AA$nAllowed[i] <- nAllowed(solve(M45), AA[i, "row"], AA[i, "col"])
# }
# for (i in 1:nrow(AA)) {
#   AA$firstAllowed[i] <- firstAllowed(solve(M45), AA[i, "row"], AA[i, "col"])
# }

#' @title Solve an almost-solved \code{X}
#' @param X An 'almost solved' puzzle, i.e. one in which 7 / 9 columns or rows
#' are solved.
#'
#'

solve_almost_solved <- function(X) {
  allowed_by_row <- apply(X, 1, function(x) setdiff(1:9, x))
  allowed_by_col <- apply(X, 2, function(x) setdiff(1:9, x))

  M <- X
  if (max(vapply(allowed_by_col, length, 0L)) <
      max(vapply(allowed_by_row, length, 0L))) {
    j <- which.max(colSums(is.na(X)) == 2L)
    i <- which.max(is.na(X[, j]))
    allowed <- setdiff(1:9, X[, j])[1]
  } else {
    i <- which.max(rowSums(is.na(X)) == 2L)
    j <- which.max(is.na(X[i, ]))
    allowed <- setdiff(1:9, X[i, ])[1]
  }

  M[i, j] <- allowed
  solve(M)
}

is_almost_solved <- function(X) {
  anyNA(X) &&
    OR(sum(colSums(is.na(X)) == 0) == 7L,
       sum(rowSums(is.na(X)) == 0) == 7L)
}

solve_almost_solved3 <- function(X) {
  colGaps <- colSums(is.na(X))
  rowGaps <- rowSums(is.na(X))

  Y <- X
  if (3L %in% colGaps) {
    j <- which.max(colGaps == 3L)
    i <- which.max(is.na(X[, j]))
  } else if (3L %in% rowGaps) {
    i <- which.max(rowGaps == 3L)
    j <- which.max(is.na(X[i, ]))
  } else {
    return(X)
  }
  for (kk in setdiff(1:9, c(X[, j], X[i, ]))) {
    Y[i, j] <- kk
    continue <-
      tryCatch(solve(Y), error = function(e) {
        TRUE
      })
    if (isTRUE(continue)) {
      Y[i, j] <- NA
      next
    }
    return(continue)
  }
  X
}












