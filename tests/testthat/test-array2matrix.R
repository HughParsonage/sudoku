context("test-array2matrix")

test_that("array2matrix works", {
  x <- c(3, 9, 5, 0, 0, 4, 0, 0, 0, 0, 2, 0, 0, 7, 0, 3, 5, 9, 0, 0,
         0, 0, 0, 0, 8, 0, 0, 0, 0, 0, 0, 0, 0, 6, 0, 7, 6, 0, 0, 3, 5,
         0, 4, 0, 8, 4, 8, 1, 7, 0, 0, 0, 0, 0, 1, 4, 0, 0, 9, 0, 0, 8,
         5, 0, 0, 0, 5, 2, 0, 1, 0, 0, 0, 0, 0, 0, 0, 7, 0, 3, 0)
  M3 <- matrix(x, nrow = 9, ncol = 9)
  m3 <- matrix(x, nrow = 9, ncol = 9)
  set_sudoku(M3)
  M3 <- provide_array(M3)

  expect_equal(array2matrix(attr(M3, "SolveArray")),
               M3,
               check.attributes = FALSE)

})
