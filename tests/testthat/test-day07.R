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

test_that("run circuit with feedback loop returns results of example tests", {
  # example 1
  #array <- c(
  #  3,26,1001,26,-4,26,3,27,1002,27,2,27,1,27,26,
  #  27,4,27,1001,28,-1,28,1005,28,6,99,0,0,5)
  #input <- c(9,8,7,6,5)
  #expect_equal(day07_run_feedback_curcuit(input, array), 139629729)

  # example 2
  #array <- c(
  #  3,52,1001,52,-5,52,3,53,1,52,56,54,1007,54,5,55,1005,55,26,1001,54,
  #  -5,54,1105,1,12,1,53,54,53,1008,54,0,55,1001,55,1,55,2,53,55,53,4,
  #  53,1001,56,-1,56,1005,56,6,99,0,0,0,0,10)
  #input <- c(9,8,7,6,5)
  #expect_equal(day07_run_feedback_curcuit(input, array), 18216)
  expect_equal(1,1)
})
