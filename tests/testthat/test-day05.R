test_that("decode operation knows sum, mult, read, write", {
  expect_equal(day05_decodeop("01"), day05_opsum)
  expect_equal(day05_decodeop("02"), day05_opmul)
  expect_equal(day05_decodeop("03"), day05_opread)
  expect_equal(day05_decodeop("04"), day05_opwrite)
})

test_that("instruction operation is correctly identified", {
  op <- day05_intruct_op
  expect_equal(op(1101), "01")
  expect_equal(op(3), "03")
  expect_equal(op(1102), "02")
  expect_equal(op(104), "04")
  expect_equal(op(99), "99")
  expect_error(op(NULL), message = "invalid instruction")
  expect_error(op(177), message = "unknown operation")
  expect_error(op(7), message = "unknown operation")
})

test_that("instruction length is correctly identified", {
  len <- day05_intruct_length
  expect_equal(len(1101), 4)
  expect_equal(len(3), 2)
  expect_equal(len(1102), 4)
  expect_equal(len(104), 2)
  expect_equal(len(99), 1)
  expect_error(len(177), message = "unknown operation")
  expect_error(len(7), message = "unknown operation")
})

test_that("instruction parameter modes are properly identified", {
  mode <- day05_instruct_par_mode
  expect_equal(mode(1101), c(1,1,0))
  expect_equal(mode(1001), c(0,1,0))
  expect_equal(mode(101),  c(1,0,0))
  expect_equal(mode(1),    c(0,0,0))

  expect_equal(mode(1102), c(1,1,0))
  expect_equal(mode(1002), c(0,1,0))
  expect_equal(mode(102),  c(1,0,0))
  expect_equal(mode(2),    c(0,0,0))


  expect_error(mode(11001), message = "invalid mode")
  expect_error(mode(11002), message = "invalid mode")
  expect_error(mode(103), message = "invalid mode")
})

test_that("sum operation performs as a sum", {
  expect_equal(day05_opsum(0, 1), 1)
  expect_equal(day05_opsum(1, 1), 2)
  expect_equal(day05_opsum(0, -1), -1)
  expect_equal(day05_opsum(-1, -1), -2)
  expect_equal(day05_opsum(123, -456), -333)
})

test_that("mul operation performs as a sum", {
  expect_equal(day05_opmul(0, 1), 0 * 1)
  expect_equal(day05_opmul(1, 1), 1 * 1)
  expect_equal(day05_opmul(1, -1), 1 * -1)
  expect_equal(day05_opmul(-1, -1), -1 * -1)
  expect_equal(day05_opmul(123, -456), 123 * -456)
})

test_that("read operation reads from buffer", {
  expect_equal(day05_opread(c(3,4,5,6)), list(3, c(4,5,6)))
  expect_equal(day05_opread(c(3,4,5,6)), list(4, c(4,5)))
  expect_equal(day05_opread(c(3,4,5,6)), list(5, c(6)))
  expect_equal(day05_opread(c(3,4,5,6)), list(6, c()))
})

test_that("write operation writes to buffer", {
  expect_equal(day05_opwrite(3, c()), c(3))
  expect_equal(day05_opwrite(4, c(3)), c(3,4))
  expect_equal(day05_opwrite(5, c(3,4)), c(3,4,5))
  expect_equal(day05_opwrite(6, c(3,4,5)), c(3,4,5,6))
})

test_that("mode in read operation works as expected", {
  test_set <- c(11, 12, 13, 14, 15)
  expect_equal(day05_getv(mode = 0, pointer = 1, array = test_set), 11)
  expect_equal(day05_getv(mode = 0, pointer = 2, array = test_set), 12)
  expect_equal(day05_getv(mode = 1, pointer = 3, array = test_set), 3)
  expect_equal(day05_getv(mode = 1, pointer = 4, array = test_set), 4)
  expect_error(
    day05_getv(mode = 0, pointer = 9, array = test_set),
    message = "out of range")
})

test_that("mode in write operation works as expected", {
  test_set <- c(11, 12, 13, 14, 15)
  expect_equal(
    day05_setv(mode = 0, value = -11, pointer = 1, array = test_set),
    c(-11, 12, 13, 14, 15))
  expect_equal(
    day05_setv(mode = 0, value = -12, pointer = 1, array = test_set),
    c(11, -12, 13, 14, 15))
  expect_error(
    day05_setv(mode = 1, pointer = 3, array = test_set),
    message = "invalid mode")
  expect_error(
    day05_setv(mode = 1, pointer = 4, array = test_set),
    message = "invalid mode")
  expect_error(
    day05_setv(mode = 0, pointer = 9, array = test_set),
    message = "out of range")
})
