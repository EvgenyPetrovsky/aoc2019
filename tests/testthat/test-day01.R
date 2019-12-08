test_that("simple fuel function", {
  f <- day01_fuel
  expect_equal(f(12), 2)
  expect_equal(f(14), 2)
  expect_equal(f(1969), 654)
  expect_equal(f(100756), 33583)
})

test_that("recursive fuel function", {
  f <- function(x) {x %>% day01_fuel() %>% day01_fuelrec()}
  expect_equal(f(12), 2)
  expect_equal(f(14), 2)
  expect_equal(f(1969), 654 + 216 + 70 + 21 + 5)
  expect_equal(f(100756), 33583 + 11192 + 3728 + 1240 + 411 + 135 + 43 + 12 + 2)
})
