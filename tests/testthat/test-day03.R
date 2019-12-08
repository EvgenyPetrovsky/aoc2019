test_that("day 03 puzzle", {
  w1 <- c("R75","D30","R83","U83","L12","D49","R71","U7","L72")
  w2 <- c("U62","R66","U55","R34","D71","R55","D58","R83")

  f <- day03
  expect_equal(f(w1. w2), 159)
})

test_that("central point has (0, 0) coordinates", {
  expect_equal(day03_centralpoint, data.frame(x = 0, y = 0)
})

test_that("current position is last row of wire-path", {
  df <- data.frame
  expect_equal(df(x = 0, y = 0), c(0,0))
  expect_equal(df(x = 1, y = 3), c(1,3))
  expect_equal(df(x = c(1, 3, 7), y = c(11, 13, 17)), c(7, 17))
})

test_that("direction and number of steps translated into direction vector", {
  expect_equal(day02_direction("R1"), c(1, 0))
  expect_equal(day02_direction("U1"), c(0, 1))
  expect_equal(day02_direction("L1"), c(-1, 0))
  expect_equal(day02_direction("D1"), c(0, -1))
})

test_that("delta-path returns every step and its relative position (0, 0)", {

})