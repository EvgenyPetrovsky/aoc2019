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
  expect_equal(op(5), "05")
  expect_equal(op(106), "06")
  expect_equal(op(1007), "07")
  expect_equal(op(1108), "08")
  expect_error(op(NULL), message = "invalid instruction")
  expect_error(op(177), message = "unknown operation")
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
  expect_equal(len(5), 3)
  expect_equal(len(6), 3)
  expect_equal(len(7), 4)
  expect_equal(len(8), 4)
  expect_error(len(177), message = "unknown operation")
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

  expect_equal(mode(3),    c(0))
  expect_equal(mode(4),    c(0))
  expect_equal(mode(104),  c(1))

  expect_equal(mode(0005), c(0,0))
  expect_equal(mode(0105), c(1,0))
  expect_equal(mode(1005), c(0,1))
  expect_equal(mode(1105), c(1,1))

  expect_equal(mode(0006), c(0,0))
  expect_equal(mode(0106), c(1,0))
  expect_equal(mode(1006), c(0,1))
  expect_equal(mode(1106), c(1,1))

  expect_equal(mode(1107), c(1,1,0))
  expect_equal(mode(1007), c(0,1,0))
  expect_equal(mode(107),  c(1,0,0))
  expect_equal(mode(7),    c(0,0,0))

  expect_equal(mode(1108), c(1,1,0))
  expect_equal(mode(1008), c(0,1,0))
  expect_equal(mode(108),  c(1,0,0))
  expect_equal(mode(8),    c(0,0,0))

  expect_error(mode(11001), message = "invalid mode")
  expect_error(mode(11002), message = "invalid mode")
  expect_error(mode(103), message = "invalid mode")

})

test_that("sum operation in intcode array performs correctly", {
  make_sum <- function(pointer, in_buffer, out_buffer) {
    function(array) {
      v <- day05_opsum(pointer, 0, array, in_buffer, out_buffer)
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

test_that("sum operation returns correct pointer", {
  make_sum <- function(in_buffer, out_buffer) {
    function(pointer, array) {
      v <- day05_opsum(pointer, 0, array, in_buffer, out_buffer)
      v$pointer
    }
  }
  opsum <- make_sum(c(1), c(1))
  # 0 -> 4
  expect_equal(opsum(0, c(1101,1,1,5,99,0)), 4)
  # 2 -> 6
  expect_equal(opsum(2, c(0,0,1101,5,4,5,99,0)), 6)
})

test_that("mul operation in intcode array performs correctly", {
  make_mul <- function(pointer, in_buffer, out_buffer) {
    function(array) {
      v <- day05_opmul(pointer, base = 0, array, in_buffer, out_buffer)
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

test_that("mul operation returns correct pointer", {
  make_mul <- function(in_buffer, out_buffer) {
    function(pointer, array) {
      v <- day05_opmul(pointer, base = 0, array, in_buffer, out_buffer)
      v$pointer
    }
  }
  opmul <- make_mul(c(1), c(1))
  # 0 -> 4
  expect_equal(opmul(0, c(1101,1,1,5,99,0)), 4)
  # 2 -> 6
  expect_equal(opmul(2, c(0,0,1101,5,4,5,99,0)), 6)
})


test_that("read operation reads from buffer", {
  opin <- function(array, in_buffer) {
    res <- day05_opinput(pointer = 0, base = 0, array, in_buffer, integer())
    res[c("array", "in_buffer")]
  }
  expect_equal(
    opin(array = c(3,4,99,0,0), in_buffer = c(13,14,15,16)),
    list(array = c(3,4,99,0,13), in_buffer = c(14,15,16)))
  expect_equal(
    opin(array = c(3,4,99,0,0), in_buffer = c(14,15,16)),
    list(array = c(3,4,99,0,14), in_buffer = c(15,16)))
  expect_equal(
    opin(array = c(3,4,99,0,0), in_buffer = c(15,16)),
    list(array = c(3,4,99,0,15), in_buffer = c(16)))
  expect_equal(
    opin(array = c(3,4,99,0,0), in_buffer = c(16)),
    list(array = c(3,4,99,0,16), in_buffer = numeric()))
  expect_error(
    opin(array = c(3,4,99,0,0), in_buffer = numeric()),
    message = "input buffer")
})

test_that("pointer moves properly on read operation", {
  x <- sample.int(1000,1)
  opin <- function(array, pointer) {
    res <- day05_opinput(pointer, base = 0, array, c(x), integer())
    res[c("pointer", "array")]
  }
  expect_equal(
    opin(pointer = 0, array = c(3,0,99)),
    list(pointer = 2, array = c(x,0,99))
  )
  expect_equal(
    opin(pointer = 3, array = c(3,0,99,3,1,99)),
    list(pointer = 5, array = c(3,x,99,3,1,99))
  )
})

test_that("write operation writes to buffer", {
  x <- sample.int(1000,1)
  opwr <- function(array, out_buffer) {
    res <- day05_opoutput(0, 0, array = array, integer(), out_buffer)
    res[c("array", "out_buffer")]
  }
  expect_equal(
    opwr(array = c(104,x,99,0,0), out_buffer = integer()),
    list(array = c(104,x,99,0,0), out_buffer = c(x)))
  expect_equal(
    opwr(array = c(4,4,99,0,x
    ), c(4)),
    list(array = c(4,4,99,0,x), out_buffer = c(4,x)))
  expect_equal(
    opwr(array = c(4,2,99,0,x), c(4,x)),
    list(array = c(4,2,99,0,x), out_buffer = c(4,x,99)))
  expect_equal(
    opwr(array = c(4,0,99,0,2), c(4,2,99)),
    list(array = c(4,0,99,0,2), out_buffer = c(4,2,99,4)))
})

test_that("get value operation works as expected", {
  test_set <- c(5, 6, 7, 8, 9, 11, 12, 13, 14, 15)
  getv <- function(mode, pointer, array) {
    day05_getv(mode, pointer, base = 0, array)
  }
  expect_equal(getv(mode = 0, pointer = 0, array = test_set), 11)
  expect_equal(getv(mode = 0, pointer = 1, array = test_set), 12)
  expect_equal(getv(mode = 0, pointer = 2, array = test_set), 13)
  expect_equal(getv(mode = 1, pointer = 0, array = test_set), 5)
  expect_equal(getv(mode = 1, pointer = 3, array = test_set), 8)
  expect_equal(getv(mode = 1, pointer = 4, array = test_set), 9)
})

test_that("get value returns zero when pointer refers out of range", {
  test_set <- c(5, 6, 7, 8, 9, 11, 12, 13, 14, 15)
  getv <- function(mode, pointer, array) {
    day05_getv(mode, pointer, base = 0, array)
  }
  expect_equal(getv(mode = 0, pointer = 5, array = test_set), 0)
  expect_equal(getv(mode = 1, pointer = 19, array = test_set), 0)
  expect_equal(getv(mode = 0, pointer = 19, array = test_set), 5)
})


test_that("get value returns zero when pointer refers out of range", {
  test_set <- c(5, 6, -7, 8, 9, 11, 12, 13, 14, 15)
  getv <- function(mode, pointer, array) {
    day05_getv(mode, pointer, base = 0, array)
  }
  expect_error(
    getv(mode = 1, pointer = -19, array = test_set),
    message = "out of range")
  expect_error(
    getv(mode = 0, pointer = -19, array = test_set),
    message = "out of range")
  expect_error(
    getv(mode = 0, pointer = 2, array = test_set),
    message = "out of range")
})


test_that("set value operation works as expected", {
  test_set <- c(1, 2, 11, 12, 13, 14, 15)
  setv <- function(value, pointer, array) {
    day05_setv(
      value = value, mode = 0,
      pointer = pointer, base = 0,
      array = array)
  }

  expect_equal(
    setv(value = -11, pointer = 0, array = test_set),
    c(1, -11, 11, 12, 13, 14, 15))
  expect_equal(
    setv(value = -12, pointer = 1, array = test_set),
    c(1, 2, -12, 12, 13, 14, 15))
})

test_that("set value raises error for negative pointer", {
  test_set <- c(1, 2, 11, 12, 13, 14, 15)
  setv <- function(value, pointer, array) {
    day05_setv(
      value = value, mode = 0,
      pointer = pointer, base = 0,
      array = array)
  }
  expect_error(
    setv(value = 1, pointer = -1, array = test_set),
    message = "out of range")
})

test_that("set value extends array for pointer that exceeds array length", {
  test_set <- c(1, 2, 11, 12, 13, 14, 15)
  setv <- function(value, pointer, array) {
    day05_setv(
      value = value, mode = 0,
      pointer = pointer, base = 0,
      array = array)
  }
  expect_equal(
    setv(value = 1, pointer = 5, array = test_set),
    {test_set[15] <- 1; test_set})
})

test_that("solution part 1 returns correct result", {
  expect_equal(day05_part1_solution(), c(0,0,0,0,0,0,0,0,0,13087969))
})

test_that("jump-if-true function updates pointer properly", {
  jump <- function(array) {
    res <- day05_jumpiftrue(0, 0, array, integer(), integer())
    res$pointer
  }
  expect_equal(jump(c(1005,4,0,99,0)), 3)
  expect_equal(jump(c(1005,4,0,99,1)), 0)
  expect_equal(jump(c(1005,4,4,99,1)), 4)
  expect_equal(jump(c(1105,4,0,99,0)), 0)
  expect_equal(jump(c(1105,0,0,99,0)), 3)
  expect_equal(jump(c(1105,4,4,99,0)), 4)
})

test_that("jump-if-false function updates pointer properly", {
  jump <- function(array) {
    res <- day05_jumpiffalse(0, 0, array, integer(), integer())
    res$pointer
  }
  expect_equal(jump(c(1006,4,0,99,0)), 0)
  expect_equal(jump(c(1006,4,4,99,0)), 4)
  expect_equal(jump(c(1006,4,0,99,1)), 3)
  expect_equal(jump(c(1106,0,0,99,0)), 0)
  expect_equal(jump(c(1106,0,4,99,0)), 4)
  expect_equal(jump(c(1106,4,0,99,0)), 3)
})

test_that("one-if-less function updates array properly", {
  x <- sample.int(1000,1)
  one <- function(array) {
    res <- day05_oneifless(0, 0, array, integer(), integer())
    res$array
  }
  expect_equal(one(c(0007,1,2,5,99,x)), c(0007,1,2,5,99,1))
  expect_equal(one(c(0007,0,1,5,99,x)), c(0007,0,1,5,99,0))
  expect_equal(one(c(0007,1,1,5,99,x)), c(0007,1,1,5,99,0))

  expect_equal(one(c(0107,2,1,5,99,x)), c(0107,2,1,5,99,0))
  expect_equal(one(c(0107,x,5,5,99,x+1)), c(0107,x,5,5,99,1))
  expect_equal(one(c(0107,1,2,5,99,x)), c(0107,1,2,5,99,1))

  expect_equal(one(c(1007,2,1,5,99,x)), c(1007,2,1,5,99,0))
  expect_equal(one(c(1007,5,x+1,5,99,x)), c(1007,5,x+1,5,99,1))
  expect_equal(one(c(1007,1,2,5,99,x)), c(1007,1,2,5,99,1))

  expect_equal(one(c(1107,1,2,5,99,x)), c(1107,1,2,5,99,1))
  expect_equal(one(c(1107,x,x+1,5,99,x)), c(1107,x,x+1,5,99,1))
  expect_equal(one(c(1107,2,1,5,99,x)), c(1107,2,1,5,99,0))
})

test_that("one-if-less function updates pointer properly", {
  x <- sample.int(1000,1)
  one <- function(pointer, array) {
    res <- day05_oneifless(pointer, 0, array, integer(), integer())
    res$pointer
  }
  expect_equal(one(0,c(integer()     , 7,1,2,5,99,x)), 0+4)
  expect_equal(one(9,c(replicate(9,x), 7,1,1,5,99,x)), 9+4)
  expect_equal(one(x,c(replicate(x,x), 7,1,1,5,99,x)), x+4)
})

test_that("one-if-equal function updates array properly", {
  x <- sample.int(1000,1)
  one <- function(array) {
    res <- day05_oneifequal(0, 0, array, integer(), integer())
    res$array
  }
  expect_equal(one(c(0008,1,2,5,99,x)), c(0008,1,2,5,99,0))
  expect_equal(one(c(0008,0,1,5,99,x)), c(0008,0,1,5,99,0))
  expect_equal(one(c(0008,1,1,5,99,x)), c(0008,1,1,5,99,1))

  expect_equal(one(c(0108,2,1,5,99,x)), c(0108,2,1,5,99,1))
  expect_equal(one(c(0108,x,5,5,99,x)), c(0108,x,5,5,99,1))
  expect_equal(one(c(0108,1,2,5,99,x)), c(0108,1,2,5,99,0))

  expect_equal(one(c(1008,2,1,5,99,x)), c(1008,2,1,5,99,1))
  expect_equal(one(c(1008,5,x,5,99,x)), c(1008,5,x,5,99,1))
  expect_equal(one(c(1008,1,2,5,99,x)), c(1008,1,2,5,99,0))

  expect_equal(one(c(1108,1,1,5,99,x)), c(1108,1,1,5,99,1))
  expect_equal(one(c(1108,x,x,5,99,x)), c(1108,x,x,5,99,1))
  expect_equal(one(c(1108,1,2,5,99,x)), c(1108,1,2,5,99,0))
})

test_that("one-if-equal function moves pointer properly", {
  x <- sample.int(1000,1)
  one <- function(pointer, array) {
    res <- day05_oneifequal(pointer, 0, array, integer(), integer())
    res$pointer
  }
  expect_equal(one(0,c(integer()     , 8,1,2,5,99,x)), 0+4)
  expect_equal(one(7,c(replicate(7,x), 8,1,1,5,99,x)), 7+4)
  expect_equal(one(x,c(replicate(x,x), 8,1,1,5,99,x)), x+4)
})

test_that("jump tests works", {
  # position mode instruction set
  op_array <- c(3,12,6,12,15,1,13,14,13,4,13,99,-1,0,1,9)
  expect_equal(day05_diagnostic(input_buffer =  0, array = op_array), 0)
  expect_equal(day05_diagnostic(input_buffer =  1, array = op_array), 1)
  expect_equal(day05_diagnostic(input_buffer = -1, array = op_array), 1)

  # immediate mode instruction set
  op_array <- c(3,3,1105,-1,9,1101,0,0,12,4,12,99,1)
  expect_equal(day05_diagnostic(input_buffer =  0, array = op_array), 0)
  expect_equal(day05_diagnostic(input_buffer =  1, array = op_array), 1)
  expect_equal(day05_diagnostic(input_buffer = -1, array = op_array), 1)

  # larger example
  op_array <- c(3,21,1008,21,8,20,1005,20,22,107,8,21,20,1006,20,31,
                1106,0,36,98,0,0,1002,21,125,20,4,20,1105,1,46,104,
                999,1105,1,46,1101,1000,1,20,4,20,1105,1,46,98,99)
  expect_equal(day05_diagnostic(input_buffer =  0, array = op_array), 0999)
  expect_equal(day05_diagnostic(input_buffer =  7, array = op_array), 0999)
  expect_equal(day05_diagnostic(input_buffer =  8, array = op_array), 1000)
  expect_equal(day05_diagnostic(input_buffer =  9, array = op_array), 1001)
  expect_equal(day05_diagnostic(input_buffer = 99, array = op_array), 1001)

})

test_that("solution part 2 returns correct result", {
  expect_equal(day05_part2_solution(), 14110739)
})
