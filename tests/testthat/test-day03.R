test_that("day 03 part 1 puzzle", {
  f <- day03_part1

  w1 <- c("R75","D30","R83","U83","L12","D49","R71","U7","L72")
  w2 <- c("U62","R66","U55","R34","D71","R55","D58","R83")
  expect_equal(f(w1, w2), 159)

  w1 <- c("R98","U47","R26","D63","R33","U87","L62","D20","R33","U53","R51")
  w2 <- c("U98","R91","D20","R16","D67","R40","U7","R15","U6","R7")
  expect_equal(f(w1, w2), 135)

})

test_that("day 03 part 2 puzzle", {
  f <- day03_part2

  w1 <- c("R75","D30","R83","U83","L12","D49","R71","U7","L72")
  w2 <- c("U62","R66","U55","R34","D71","R55","D58","R83")
  expect_equal(f(w1, w2), 610)

  w1 <- c("R98","U47","R26","D63","R33","U87","L62","D20","R33","U53","R51")
  w2 <- c("U98","R91","D20","R16","D67","R40","U7","R15","U6","R7")
  expect_equal(f(w1, w2), 410)

})

test_that("central point has (0, 0) coordinates", {
  expect_equal(day03_centralpoint, c(0, 0))
})

test_that("current position is last row of wire-path", {
  df <- data.frame
  expect_equal(day03_position(df(x = 0, y = 0)), c(0,0))
  expect_equal(day03_position(df(x = 1, y = 3)), c(1,3))
  expect_equal(day03_position(df(x = c(1, 3, 7), y = c(11, 13, 17))), c(7, 17))
})

test_that("instruction translated into direction vector", {
  expect_equal(day03_path_direction("R1"), c(1, 0))
  expect_equal(day03_path_direction("U1"), c(0, 1))
  expect_equal(day03_path_direction("L1"), c(-1, 0))
  expect_equal(day03_path_direction("D1"), c(0, -1))
})

test_that("instruction translated into number of steps", {
  expect_equal(day03_path_length("R1"), 1)
  expect_equal(day03_path_length("U2"), 2)
  expect_equal(day03_path_length("L3"), 3)
  expect_equal(day03_path_length("D4"), 4)
})

test_that("current position and direction and number of steps give delta path", {
  d <- day03_path_direction
  l <- day03_path_length
  f <- function(x, y) {data.frame(x = x, y = y)}
  expect_equal(day03_deltapath(c(10, 10), d("R1"), l("R1")), f(11, 10))
  expect_equal(day03_deltapath(c(10, 10), d("U1"), l("U1")), f(10, 11))
  expect_equal(day03_deltapath(c(10, 10), d("L1"), l("L1")), f(09, 10))
  expect_equal(day03_deltapath(c(10, 10), d("D1"), l("D1")), f(10, 09))
})

test_that("manhettan distance is abs( x - x_ref ) + abs ( y - y_ref )", {
  expect_equal(day03_mhtdist(c(1,1), c(0,0)), 2)
  expect_equal(day03_mhtdist(c(1,1), c(0,1)), 1)
  expect_equal(day03_mhtdist(c(1,1), c(1,0)), 1)
  expect_equal(day03_mhtdist(c(1,1), c(1,1)), 0)
  expect_equal(day03_mhtdist(c(1,1), c(2,2)), 2)
  expect_equal(day03_mhtdist(c(1,1), c(-2,-2)), 6)
  expect_equal(day03_mhtdist(c(1,1)), 2)
  expect_equal(day03_mhtdist(c(1,-1)), 2)
})

test_that("full path is build properly", {
  expect_equal(
    day03_fullpath(c("R2", "U1"), c(0, 0)),
    data.frame(x = c(0, 1, 2, 2), y = c(0, 0, 0, 1))
  )
  expect_equal(
    day03_fullpath(c("R2", "U1")),
    data.frame(x = c(0, 1, 2, 2), y = c(0, 0, 0, 1))
  )
  expect_equal(
    day03_fullpath(c("R2", "U1"), c(10, 10)),
    data.frame(x = c(10, 11, 12, 12), y = c(10, 10, 10, 11))
  )
})

