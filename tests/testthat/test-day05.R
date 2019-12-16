test_that("decode operation knows sum, mult, read, write", {
  expect_equal(day05_decodeop("01"), day05_opsum)
  expect_equal(day05_decodeop("02"), day05_opmul)
  expect_equal(day05_decodeop("03"), day05_opinput)
  expect_equal(day05_decodeop("04"), day05_opoutput)
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
  expect_equal(len(1), 4)
  expect_equal(len(101), 4)
  expect_equal(len(1102), 4)
  expect_equal(len(2), 4)
  expect_equal(len(102), 4)
  expect_equal(len(3), 2)
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

test_that("sum operation in intcode array performs correctly", {
  make_sum <- function(pointer, in_buffer, out_buffer) {
    function(array) {
      v <- day05_opsum(pointer, array, in_buffer, out_buffer)
      v$array
    }
  }
  opsum <- make_sum(0, c(1), c(1))
  # 1 + 1
  expect_equal(opsum(c(1101,1,1,5,99,0)), c(1101,1,1,5,99,2))
  # 5 + 4
  expect_equal(opsum(c(1101,5,4,5,99,0)), c(1101,5,4,5,99,9))
  # 5 + 5
  expect_equal(opsum(c(1,3,3,5,99,0)), c(1,3,3,5,99,10))
  # 1 + 99
  expect_equal(opsum(c(1,0,4,5,99,0)), c(1,0,4,5,99,100))
})

test_that("mul operation performs as a sum", {
  make_mul <- function(pointer, in_buffer, out_buffer) {
    function(array) {
      v <- day05_opmul(pointer, array, in_buffer, out_buffer)
      v$array
    }
  }
  opmul <- make_mul(0, c(1), c(1))
  # 1 * 1
  expect_equal(opmul(c(1101,1,1,5,99,0)), c(1101,1,1,5,99,1))
  # 5 * 4
  expect_equal(opmul(c(1101,5,4,5,99,0)), c(1101,5,4,5,99,20))
  # 5 * 5
  expect_equal(opmul(c(1,3,3,5,99,0)), c(1,3,3,5,99,25))
  # 1 * 99
  expect_equal(opmul(c(1,0,4,5,99,0)), c(1,0,4,5,99,99))
})

test_that("read operation reads from buffer", {
  expect_equal(
    day05_opinput(0, array = c(3,4,99,0,0), in_buffer = c(13,14,15,16), c(1)),
    list(array = c(3,4,99,0,13), in_buffer = c(14,15,16), out_buffer = c(1)))
  expect_equal(
    day05_opinput(0, array = c(3,4,99,0,0), in_buffer = c(14,15,16), c(1)), 
    list(array = c(3,4,99,0,14), in_buffer = c(15,16), out_buffer = c(1)))
  expect_equal(
    day05_opinput(0, array = c(3,4,99,0,0), in_buffer = c(15,16), c(1)), 
    list(array = c(3,4,99,0,15), in_buffer = c(16), out_buffer = c(1)))
  expect_equal(
    day05_opinput(0, array = c(3,4,99,0,0), in_buffer = c(16), c(1)), 
    list(array = c(3,4,99,0,16), in_buffer = numeric(), out_buffer = c(1)))
  expect_error(
    day05_opinput(0, array = c(3,4,99,0,0), in_buffer = numeric(), c(1)),
    message = "input buffer"
  )
})

test_that("write operation writes to buffer", {
  expect_equal(
    day05_opoutput(0, array = c(104,4,99,0,0), c(1), numeric()), 
    list(array = c(104,4,99,0,0), in_buffer = c(1), out_buffer = c(4)))
  expect_equal(
    day05_opoutput(0, array = c(4,4,99,0,2), c(1), c(4)), 
    list(array = c(4,4,99,0,2), in_buffer = c(1), out_buffer = c(4,2)))
  expect_equal(
    day05_opoutput(0, array = c(4,2,99,0,2), c(1), c(4,2)), 
    list(array = c(4,2,99,0,2), in_buffer = c(1), out_buffer = c(4,2,99)))
  expect_equal(
    day05_opoutput(0, array = c(4,0,99,0,2), c(1), c(4,2,99)), 
    list(array = c(4,0,99,0,2), in_buffer = c(1), out_buffer = c(4,2,99,4)))
})

test_that("get value operation works as expected", {
  test_set <- c(5, 6, 7, 8, 9, 11, 12, 13, 14, 15)
  expect_equal(day05_getv(mode = 0, pointer = 0, array = test_set), 11)
  expect_equal(day05_getv(mode = 0, pointer = 1, array = test_set), 12)
  expect_equal(day05_getv(mode = 0, pointer = 2, array = test_set), 13)
  expect_equal(day05_getv(mode = 1, pointer = 0, array = test_set), 5)
  expect_equal(day05_getv(mode = 1, pointer = 3, array = test_set), 8)
  expect_equal(day05_getv(mode = 1, pointer = 4, array = test_set), 9)
  expect_error(
    day05_getv(mode = 0, pointer = 5, array = test_set),
    message = "out of range")
  expect_error(
    day05_getv(mode = 1, pointer = 19, array = test_set),
    message = "out of range")
})

test_that("mode in write operation works as expected", {
  test_set <- c(11, 12, 13, 14, 15)
  expect_equal(
    day05_setv(value = -11, pointer = 0, array = test_set),
    c(-11, 12, 13, 14, 15))
  expect_equal(
    day05_setv(value = -12, pointer = 1, array = test_set),
    c(11, -12, 13, 14, 15))
  expect_error(
    day05_setv(value = 1, pointer = -1, array = test_set),
    message = "out of range")
  expect_error(
    day05_setv(value = 1, pointer = 5, array = test_set),
    message = "out of range")
})
