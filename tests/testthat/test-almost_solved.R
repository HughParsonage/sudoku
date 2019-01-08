context("test-almost_solved")

test_that("solve almost solved", {

  # A
  AS <- structure(c(8L, NA, 4L, 1L, 6L, NA, NA, 9L, 5L,
                    9L, NA, 5L, 4L, 8L, NA, NA, 6L, 1L,
                    6L, 7L, 1L, 5L, 9L, 2L, 3L, 8L, 4L,

                    3L, 5L, 9L, 8L, 7L, 4L, 1L, 2L, 6L,
                    7L, 4L, 8L, 6L, 2L, 1L, 5L, 3L, 9L,
                    2L, 1L, 6L, 9L, 3L, 5L, 4L, 7L, 8L,

                    4L, 9L, 3L, 7L, 1L, 8L, 6L, 5L, 2L,
                    1L, 8L, 7L, 2L, 5L, 6L, 9L, 4L, 3L,
                    5L, 6L, 2L, 3L, 4L, 9L, 8L, 1L, 7L),

                  .Dim = c(9L, 9L), class = "sudoku")
  # 8  9  6   3  7  2   4  1  5
  # .  .  7   5  4  1   9  8  6
  # 4  5  1   9  8  6   3  7  2
  #
  # 1  4  5   8  6  9   7  2  3
  # 6  8  9   7  2  3   1  5  4
  # .  .  2   4  1  5   8  6  9
  #
  # .  .  3   1  5  4   6  9  8
  # 9  6  8   2  3  7   5  4  1
  # 5  1  4   6  9  8   2  3  7

  expect_true(is_almost_solved(AS))
  expect_true(is_solved(solve(AS)))
})
