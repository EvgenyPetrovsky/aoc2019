test_that("connect lists with element", {
  x <- sample.int(1000, 1)
  f = da07_add_value_to_list_elements
  expect_equal(f(list(1), x), list(c(1,x)))
  expect_equal(f(list(1, 2), x), list(c(1,x), c(2,x)))
  l <- list(a = sample.int(1000, 10), b = sample.int(1000, 10))
  expect_equal(
    f(l[c("a","b")], x), 
    list(a = c(l$a,x), b = c(l$b,x)))
})

test_that("permutations work", {
  f = day07_permutations
  expect_equal(f(1), 1)
  expect_error(f(c()), message = "nothing to permutate")
  expect_equal(f(1:2), list(c(2,1), c(1,2)))
})

test_that("run curcuit returns results as in example tests", {
  # example 1
  array <- c(3,15,3,16,1002,16,10,16,1,16,15,15,4,15,99,0,0)
  input <- c(4,3,2,1,0)
  expect_equal(day07_run_curcuit(input, array), 43210)
  # example 2
  array <- c(3,23,3,24,1002,24,10,24,1002,23,-1,23,101,5,23,23,1,24,23,23,4,23,99,0,0)
  input <- c(0,1,2,3,4)
  expect_equal(day07_run_curcuit(input, array), 54321)
  # example 3
  array <- c(3,31,3,32,1002,32,10,32,1001,31,-2,31,1007,31,0,33,1002,33,7,33,1,33,31,31,1,32,31,31,4,31,99,0,0,0)
  input <- c(1,0,4,3,2)
  expect_equal(day07_run_curcuit(input, array), 65210)
})

test_that("solution part 1 works correctly", {
  expect_equal(day07_part1_solution(), 422858)
})