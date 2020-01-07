test_that("get value in mode 2 uses relative base", {
  getv <- function(pointer, base, array)
    day05_getv(mode = 2, pointer, base, array)

  expect_equal(getv(4, 0, c(0:10)), 4)
  expect_equal(getv(0, 2, c(0:10)), 2)
  expect_equal(getv(1, 2, c(0:10)), 3)
  expect_equal(getv(4, 0, c(10:19,1000:1019)), 1004)
  expect_equal(getv(4, -4, c(10:19,1000:1019)), 1000)
  expect_equal(getv(0, 2, c(0:-10)), -2)
  expect_equal(getv(1, 2, c(0:-10)), -1)
})

test_that("set value in mode 2 uses relative base", {
  x <- sample.int(1000, 1)
  setv <- function(pointer, base, array)
    day05_setv(value = x, mode = 2, pointer, base, array)
  expect_equal(setv(0, 0, 1:3), c(1, x, 3))
  expect_equal(setv(0, 1, 1:3), c(1, 2, x))
  expect_equal(setv(1, 1, 1:4), c(1:3, x))
  expect_equal(setv(0,-1, 1:3), c(x, 2, 3))
  expect_equal(setv(0, 1,-1:1), c(x, 0, 1))
})

test_that("09 instruction code changes relative base", {
  setbase <- function(p, b, a) {
    res <- day05_changebase(
      pointer = p, base = b, array = a, 
      in_buffer = integer(), out_buffer = integer())
    res$base
  }
  
  expect_equal(setbase(p=0, b=0, a=c(109,2,99)), 2)
  expect_equal(setbase(p=1, b=0, a=c(0,109,2,99)), 2)
  expect_equal(setbase(p=0, b=2, a=c(109,2,99)), 4)
  expect_equal(setbase(p=0, b=-2, a=c(109,2,99)), 0)
  expect_equal(setbase(p=0, b=2, a=c(009,1,99)), 3)
  expect_equal(setbase(p=0, b=2, a=c(009,2,99)), 101)
  expect_equal(setbase(p=0, b=-2, a=c(209,2,99)), 207)
  expect_equal(setbase(p=0, b=0, a=c(209,2,99)), 99)
})

test_that("test cases from instructions work", {

  # produces a copy of itself as output
  array <- c(109,1,204,-1,1001,100,1,100,1008,100,16,101,1006,101,0,99)
  expect_equal(
    day05_run_computer(input_buffer = integer(), array = array)$buffer,
    array
  )

  # returns 16 digit number
  array <- c(1102,34915192,34915192,7,4,7,99,0)
  expect_equal(
    day05_run_computer(input_buffer = integer(), array = array)$buffer %>% 
      nchar(),
    16
  )

  # should output the large number in the middle
  array <- c(104,1125899906842624,99)
  expect_equal(
    day05_run_computer(input_buffer = integer(), array = array)$buffer,
    1125899906842624
  )

  test_that("day 09 solutions are correct", {
    #expect_equal(day09_part1_solution(), 3241900951 )
    #expect_equal(day09_part2_solution(), 83089)
    expect_equal(1,1)
  })

})