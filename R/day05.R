#' decode instruction to identify operation
#'
#' @param instruction instruction code: integer value or digital code
day05_intruct_op <- function(instruction) {
  instruction_code <- paste0("0",instruction)
  n <- nchar(instruction_code)
  if (n < 2) {
    message = paste("invalid instruction:", instruction)
    stop(message)
  }
  op_code <- substr(instruction_code, n-1, n)
  if (!op_code %in% names(aoc19::INTCODE$opcode)) {
    message <- paste("invalid operation code:", op_code)
    stop(message)
  }
  op_code
}

#' add two numbers
#'
#' @param pointer pointer
#' @param base relative base of pointer for parameter mode 2
#' @param array array
#' @param in_buffer input array
#' @param out_buffer output array
day05_opsum   <- function(pointer, base, array, in_buffer, out_buffer) {
  instruction_code <- day05_getv(mode = 1, pointer + 0, base, array)
  modes <- day05_instruct_par_mode(instruction_code)
  v1 <- day05_getv(mode = modes[1], pointer + 1, base, array)
  v2 <- day05_getv(mode = modes[2], pointer + 2, base, array)
  rp <- pointer + 3

  new_array <- day05_setv(value = (v1 + v2), mode = modes[3], rp, base, array)
  new_pointer <- pointer + day05_intruct_length(instruction_code)
  # result
  list(
    pointer = new_pointer, base = base, array = new_array,
    in_buffer = in_buffer, out_buffer = out_buffer)
}

#' multiply two numbers
#'
#' @param pointer current position of instruction
#' @param base relative base of pointer for parameter mode 2
#' @param array incode array
#' @param in_buffer vector of values to be read
#' @param out_buffer vector of output
day05_opmul   <- function(pointer, base, array, in_buffer, out_buffer) {
  instruction_code <- day05_getv(mode = 1, pointer + 0, base, array)
  modes <- day05_instruct_par_mode(instruction_code)
  v1 <- day05_getv(mode = modes[1], pointer + 1, base, array)
  v2 <- day05_getv(mode = modes[2], pointer + 2, base, array)
  rp <- pointer + 3

  new_array <- day05_setv(value = (v1 * v2), mode = modes[3], rp, base, array)
  new_pointer <- pointer + day05_intruct_length(instruction_code)
  # result
  list(
    pointer = new_pointer, base = base, array = new_array,
    in_buffer = in_buffer, out_buffer = out_buffer)
}

#' read input
#'
#' @param pointer current position of instruction
#' @param base relative base of pointer for parameter mode 2
#' @param array incode array
#' @param in_buffer vector of values to be read
#' @param out_buffer vector of output
day05_opinput  <- function(pointer, base, array, in_buffer, out_buffer) {
  instruction_code <- day05_getv(mode = 1, pointer + 0, base, array)
  modes <- day05_instruct_par_mode(instruction_code)

  # check whether buffer is empty
  if (length(in_buffer) == 0) {
    message <- "empty input buffer"
    stop(message)
  }

  v <- in_buffer[1]
  new_in_buffer <- in_buffer[-1]
  rp <- pointer + 1

  new_array <- day05_setv(
    value = v, mode = modes[1], pointer = rp, base, array)
  new_pointer <- pointer + day05_intruct_length(instruction_code)
  # result
  list(
    pointer = new_pointer, base = base, array = new_array,
    in_buffer = new_in_buffer, out_buffer = out_buffer)

}

#' return result (print output)
#'
#' @param pointer current position of instruction
#' @param base relative base of pointer for parameter mode 2
#' @param array incode array
#' @param in_buffer vector of values to be read
#' @param out_buffer vector of output
day05_opoutput <- function(pointer, base, array, in_buffer, out_buffer) {
  instruction_code <- day05_getv(mode = 1, pointer + 0, base, array)
  modes <- day05_instruct_par_mode(instruction_code)
  v <- day05_getv(mode = modes[1], pointer + 1, base, array)
  new_out_buffer <- c(out_buffer, v)
  new_pointer <- pointer + day05_intruct_length(instruction_code)
  # result
  list(
    pointer = new_pointer, base = base, array = array,
    in_buffer = in_buffer, out_buffer = new_out_buffer)
}

#' move pointer if frist parameter is non-zero
#'
#' @param pointer pointer
#' @param base relative base of pointer for parameter mode 2
#' @param array array
#' @param in_buffer input array
#' @param out_buffer output array
day05_jumpiftrue <- function(pointer, base, array, in_buffer, out_buffer) {
  instruction_code <- day05_getv(mode = 1, pointer + 0, base, array)
  modes <- day05_instruct_par_mode(instruction_code)
  # instruction parameter values
  v1 <- day05_getv(mode = modes[1], pointer + 1, base, array)
  v2 <- day05_getv(mode = modes[2], pointer + 2, base, array)
  # move pointer if 1st parameter non-zero
  new_pointer <-
    if (v1 != 0) {v2} else {pointer + day05_intruct_length(instruction_code)}
  # result
  list(
    pointer = new_pointer, base = base, array = array,
    in_buffer = in_buffer, out_buffer = out_buffer)
}

#' move pointer if frist parameter is zero
#'
#' @param pointer pointer
#' @param base relative base of pointer for parameter mode 2
#' @param array array
#' @param in_buffer input array
#' @param out_buffer output array
day05_jumpiffalse <- function(pointer, base, array, in_buffer, out_buffer) {
  instruction_code <- day05_getv(mode = 1, pointer + 0, base, array)
  modes <- day05_instruct_par_mode(instruction_code)
  # instruction parameter values
  v1 <- day05_getv(mode = modes[1], pointer + 1, base, array)
  v2 <- day05_getv(mode = modes[2], pointer + 2, base, array)
  # move pointer if 1st parameter non-zero
  new_pointer <-
    if (v1 == 0) {v2} else {pointer + day05_intruct_length(instruction_code)}
  # result
  list(
    pointer = new_pointer, base = base, array = array,
    in_buffer = in_buffer, out_buffer = out_buffer)
}


#' move pointer if frist parameter is less than second
#'
#' @param pointer pointer
#' @param base relative base of pointer for parameter mode 2
#' @param array array
#' @param in_buffer input array
#' @param out_buffer output array
day05_oneifless <- function(pointer, base, array, in_buffer, out_buffer) {
  instruction_code <- day05_getv(mode = 1, pointer + 0, base, array)
  modes <- day05_instruct_par_mode(instruction_code)
  # instruction parameter values
  v1 <- day05_getv(mode = modes[1], pointer + 1, base, array)
  v2 <- day05_getv(mode = modes[2], pointer + 2, base, array)
  rp <- pointer + 3

  new_array <- day05_setv(
    value = 1 * (v1 < v2), mode = modes[3], rp, base, array)
  new_pointer <- pointer + day05_intruct_length(instruction_code)
  # result
  list(
    pointer = new_pointer, base = base, array = new_array,
    in_buffer = in_buffer, out_buffer = out_buffer)
}

#' move pointer if frist parameter is equal to second
#'
#' @param pointer pointer
#' @param base relative base of pointer for parameter mode 2
#' @param array array
#' @param in_buffer input array
#' @param out_buffer output array
day05_oneifequal <- function(pointer, base, array, in_buffer, out_buffer) {
  instruction_code <- day05_getv(mode = 1, pointer + 0, base, array)
  modes <- day05_instruct_par_mode(instruction_code)
  # instruction parameter values
  v1 <- day05_getv(mode = modes[1], pointer + 1, base, array)
  v2 <- day05_getv(mode = modes[2], pointer + 2, base, array)
  rp <- pointer + 3

  new_array <- day05_setv(
    value = 1 * (v1 == v2), mode = modes[3], rp, base, array)
  new_pointer <- pointer + day05_intruct_length(instruction_code)
  # result
  list(
    pointer = new_pointer, base = base, array = new_array,
    in_buffer = in_buffer, out_buffer = out_buffer)
}

#' change relative base
#'
#' @param pointer pointer
#' @param base relative base of pointer for parameter mode 2
#' @param array array
#' @param in_buffer input array
#' @param out_buffer output array
day05_changebase <- function(pointer, base, array, in_buffer, out_buffer) {
  instruction_code <- day05_getv(mode = 1, pointer + 0, base, array)
  modes <- day05_instruct_par_mode(instruction_code)
  # instruction parameter values
  offset <- day05_getv(mode = modes[1], pointer + 1, base, array)
  new_base <- base + offset

  new_pointer <- pointer + day05_intruct_length(instruction_code)
  # result
  list(
    pointer = new_pointer, base = new_base, array = array,
    in_buffer = in_buffer, out_buffer = out_buffer)
}

#' Decode operation
#'
#' @param opcode two digit operation code
day05_decodeop <- function(opcode) {
  decode_table <- list(
    "01" = day05_opsum,
    "02" = day05_opmul,
    "03" = day05_opinput,
    "04" = day05_opoutput,
    "05" = day05_jumpiftrue,
    "06" = day05_jumpiffalse,
    "07" = day05_oneifless,
    "08" = day05_oneifequal,
    "09" = day05_changebase,
    "99" = NULL
  )
  decode_table[[opcode]]
}

#' Length of instruction - how many registers belong to instruction
#'
#' @param instruction instruction integer code
day05_intruct_length <- function(instruction) {
  op_code <- day05_intruct_op(instruction)
  aoc19::INTCODE$opcode[[op_code]]
}

#' instruction parameter modes
#'
#' @param instruction instruction integer code
day05_instruct_par_mode <- function(instruction) {
  calculate_mode <- function(par_position) {
    # parameter modes go in reversed order and start at position 3
    u <- 10^(par_position + 1 + 1)
    d <- 10^(par_position + 1)
    (instruction %% u) %/% d
  }
  op_code <- day05_intruct_op(instruction)
  numpar <- day05_intruct_length(instruction) - 1
  res <- sapply(
    FUN = calculate_mode,
    X = 1:numpar, USE.NAMES = F)
  validate <- function(par_position, mode) {
    valid_mode <- aoc19::INTCODE$valid_mode
    if (mode %in% valid_mode[[op_code]][[par_position]]) {
      mode
    } else {
      message <- paste(
        "invalid mode:", mode,
        "for parameter position", par_position,
        "in operation", op_code,
        "instruction", instruction
        )
      stop(message)
    }
  }
  1:length(res) %>% sapply(
    FUN = function(x) validate(x, res[x]),
    USE.NAMES = FALSE)
}

#' Get value
#'
#' @param mode parameter mode (read from array or show exact value)
#' @param pointer pointer to element in array or exact value
#' @param base relative base of pointer for parameter mode 2
#' @param array array of intcodes
day05_getv <- function(mode, pointer, base, array) {

  check_pointer <- function(p) {
    #if (p < 0 | p >= length(array)) {
    if (p < 0) {
      message <- paste(
        "pointer =", p,
        "base =", base,
        "mode =", mode,
        "refers out of range")
      stop(message)
    }
  }
  check_pointer(pointer)
  value <-
    if (pointer >= length(array)) 0
    else if (is.na(array[pointer + 1])) 0
    else array[pointer + 1]
  if (mode == 1) {
    value
  } else if (mode == 0 | mode == 2) {
    new_pointer <-
      if (mode == 0) value
      else if (mode == 2) base + value
      else integer()
    check_pointer(new_pointer)
    value <- day05_getv(mode = 1, new_pointer, base, array)
    value
  }
}

#' Set value
#'
#' takes value and puts it into right place in array returning updated array
#'
#' @param value parameter value
#' @param mode parameter mode (read from array or show exact value)
#' @param pointer pointer to element in array or exact value
#' @param base relative base of pointer for parameter mode 2
#' @param array array of intcodes
day05_setv <- function(value, mode, pointer, base, array) {
  if (! mode %in% c(0,2)) {
    stop("set operation supports only positional mode")
  }

  check_pointer <- function(p) {
    if (p < 0) {
      message <- paste(
        "pointer =", p,
        "base =", base,
        "mode =", mode,
        "refers out of range")
      stop(message)
    }
  }

  check_pointer(pointer)
  pos <-
    if (mode == 0) array[pointer + 1]
    else if (mode == 2) array[pointer + 1] + base

  check_pointer(pos)
  array[pos + 1] <- value
  array
}

#' run diagnostics on INTCODE computer
#'
#' @export
#'
#' @param input_buffer buffer of input values
#' @param array INCODE array
day05_run_computer <- function(input_buffer, array) {
  pos <- 0
  rel_base <- 0
  iteration <- 0
  temp <- array
  output_buffer <- integer()

  #
  next_pos <- function(pos, instruct_code) {
    pos + day05_intruct_length(instruct_code)
  }
  getinstr <- function(pos) {
    day05_getv(mode = 1, pointer = pos, base = rel_base, array = temp)
  }
  getopcode <- day05_intruct_op
  decodeop <- day05_decodeop

  # main loop via intructions
  while (pos %>% getinstr() %>% getopcode() != 99) {

    instruction_code <- getinstr(pos + 0)
    op_code <- getopcode(instruction_code)
    operation <- decodeop(op_code)

    # incocate specialized operation
    res <- operation(
      pointer = pos, base = rel_base, array = temp,
      in_buffer = input_buffer, out_buffer = output_buffer)

    temp          <- res$array
    input_buffer  <- if (length(res$out_buffer) > 0) {
      buffer <- c(res$in_buffer, res$out_buffer)
      buffer
    } else {
      res$in_buffer
    }
    output_buffer <- integer()

    # and finally new pointer and relative base
    pos <- res$pointer
    rel_base <- res$base

  }
  out <- list(
    array = temp,
    buffer = input_buffer
  )

  out

}

#' run diagnostics on INTCODE computer
#'
#' @param input_buffer buffer of input values
#' @param array INCODE array
day05_diagnostic <- function(input_buffer, array) {
  run_results <- day05_run_computer(input_buffer, array)
  run_results$buffer
}

#' Day 05 part 1 solution
#'
#' @export
day05_part1_solution <- function() {
  input_buffer <- c(1)
  input_array  <- aoc19::DATASET$day05
  day05_diagnostic(input_buffer, input_array)
}

#' Day 05 part 2 solution
#'
#' @export
day05_part2_solution <- function() {
  input_buffer <- c(5)
  input_array  <- aoc19::DATASET$day05
  day05_diagnostic(input_buffer, input_array)
}
