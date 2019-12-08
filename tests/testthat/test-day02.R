test_that("day 02 puzzle", {
  input <- c(1,9,10,3,
             2,3,11,0,
             99,
             30,40,50)
  output <- c(3500,9,10,70,
              2,3,11,0,
              99,
              30,40,50)
  expect_equal(day02(input), output)
  expect_equal(day02(c(1,0,0,0,99)), c(2,0,0,0,99))
  expect_equal(day02(c(2,3,0,3,99)), c(2,3,0,6,99))
  expect_equal(day02(c(2,4,4,5,99,0)), c(2,4,4,5,99,9801))
  expect_equal(day02(c(1,1,1,4,99,5,6,0,99)), c(30,1,1,4,2,5,6,0,99))
})
