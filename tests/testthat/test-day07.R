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
  f = day07_perm
  expect_equal(f(1), 1)
  expect_error(f(c()), message = "nothing to permutate")
  expect_equal(f(1:2), list(c(2,1), c(1,2)))
})