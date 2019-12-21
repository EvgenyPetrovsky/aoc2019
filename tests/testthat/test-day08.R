test_that("day08_text2char splits text into characters", {
  f <- day08_text2char
  expect_equal(f("ABC"), LETTERS[1:3])
  expect_equal(f(paste(letters, collapse = "")), letters)
  characters <- c(LETTERS, c("!", "?", ",", "."), replicate(5, " "))
  text <- sample(characters, 1000, replace = T)
  expect_equal(f(paste(characters, collapse = "")), characters)
})

test_that("day08_split2chunks properly cuts vector into chunks", {
  expect_equal(day08_split2chunks(1:7,3), list(1:3, 4:6))
  expect_equal(day08_split2chunks(1:100,50), list(1:50, 51:100))
})

test_that("day08_part1_solution gives proper results", {
  expect_equal(day08_part1_solution(), 2760)
})

test_that("day08_trace_pixel calculates visibility of below layer properly", {
  f <- day08_trace_pixel
  expect_equal(f(0, 0), 0)
  expect_equal(f(0, 1), 0)
  expect_equal(f(0, 2), 0)
  expect_equal(f(1, 0), 1)
  expect_equal(f(1, 1), 1)
  expect_equal(f(1, 2), 1)
  expect_equal(f(2, 0), 0)
  expect_equal(f(2, 1), 1)
  expect_equal(f(2, 2), 2)
  expect_equal(f(
    c(0,0,0,1,1,1,2,2,2), 
    c(0,1,2,0,1,2,0,1,2)), 
    c(0,0,0,1,1,1,0,1,2))
})

test_that("visible pixels are properly identified for stack of layers", {
  l1 <- c(0,2,2,2)
  l2 <- c(1,1,2,2)
  l3 <- c(2,2,1,2)
  l4 <- c(0,0,0,0)
  lr <- c(0,1,1,0)
  expect_equal(day08_top_visible_pixel(list(l1,l2,l3,l4)), lr)
})

test_that("day08_part2_solution gives proper results", {
  picture <- paste0(
    " 11   11  1  1 1111 111  ",
    "1  1 1  1 1  1 1    1  1 ",
    "1  1 1    1  1 111  111  ",
    "1111 1 11 1  1 1    1  1 ",
    "1  1 1  1 1  1 1    1  1 ",
    "1  1  111  11  1111 111  "
  )
  expect_equal(
    day08_part2_solution(), 
    gsub(" ", "0", picture, fixed = FALSE))
})
