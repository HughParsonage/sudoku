


provide_array <- function(X, force = FALSE) {
  set_sudoku(X)
  DT <- attr(X, "SolveArray")
  if (is.null(DT) || force) {
    vi <- NULL
    DT <- CJ(row = 1:9,
             col = 1:9,
             vi = 1:9)

    square <- value <- NULL
    DT[, "square" := " 0, 0"]
    for (i in 1:9) {
      z1 <- if (i <= 3) "-1" else if (i > 6) "+1" else " 0"
      for (j in 1:9) {
        z2 <- if (j <= 3) "-1" else if (j > 6) "+1" else " 0"
        if (!is.null(z2) || !is.null(z1)) {
          DT[row == i & col == j, "square" := paste0(z1, ",", z2)]
        }
        DT[row == i & col == j, "value" := X[i, j]]
      }
    }

    solved <- NULL
    DT[, "solved" := NA]
    DT[!is.na(value), "solved" := value == vi]
    DT <- DT[is.na(value) | solved == TRUE]

    isPossible <- NULL
    DT[, "isPossible" := NA]
    for (k in 1:nrow(DT)) {
      if (is.na(DT[["solved"]][k])) {
        set(DT,
            i = k,
            j = "isPossible",
            value = is_possible(X,
                                i = DT[["row"]][k],
                                j = DT[["col"]][k],
                                DT[["vi"]][k],
                                depth = 2L))
      }
    }

    setattr(X, "SolveArray", DT)
  }
  X
}

array2matrix <- function(DT) {
  DT0 <- unique(DT, by = c("row", "col"))
  X <- matrix(.subset2(DT0, "value"), byrow = TRUE, nrow = 9L, ncol = 9L)
  set_sudoku(X)
  X
}
