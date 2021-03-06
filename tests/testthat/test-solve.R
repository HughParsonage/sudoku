context("test-solve")

test_that("solves known cases", {
  skip_if_not_installed("sudoku")
  MM2 <-
    matrix(as.integer(c(0, 0, 6, 7, 8, 0, 3, 2, 4,
                        4, 2, 3, 9, 1, 6, 0, 0, 0,
                        7, 8, 5, 0, 0, 3, 6, 0, 0,
                        0, 4, 8, 0, 0, 2, 1, 0, 6,
                        0, 7, 1, 5, 4, 8, 0, 9, 3,
                        3, 0, 2, 6, 7, 1, 8, 4, 5,
                        8, 0, 4, 2, 6, 9, 7, 5, 1,
                        2, 6, 9, 0, 5, 7, 0, 0, 0,
                        1, 0, 0, 0, 3, 4, 9, 6, 2)),
           nrow = 9,
           ncol = 9)
  expect_equal(sudoku::solveSudoku(MM2, print.it = FALSE),
               unclass(solve(MM2)),
               check.attributes = FALSE)

  S48 <-
    matrix(as.integer(c(0, 0, 6, 0, 0, 8, 0, 9, 0,
                        8, 0, 5, 2, 0, 0, 0, 0, 0,
                        3, 0, 0, 0, 6, 0, 8, 0, 1,

                        0, 0, 7, 0, 0, 9, 0, 2, 0,
                        6, 0, 0, 0, 0, 5, 9, 0, 0,
                        0, 0, 1, 3, 0, 6, 5, 7, 0,

                        2, 0, 0, 6, 3, 7, 1, 0, 0,
                        0, 0, 0, 0, 0, 0, 0, 8, 0,
                        1, 5, 4, 0, 8, 0, 7, 0, 6)),
           nrow = 9,
           ncol = 9)

  expect_true(is_solved(solve(S48)))
  expect_false(has_failed(solve(S48)))



})
