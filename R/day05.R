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
#' @param array array
#' @param in_buffer input array
#' @param out_buffer output array
day05_opsum   <- function(pointer, array, in_buffer, out_buffer) {
  instruction_code <- day05_getv(mode = 1, pointer + 0, array)
  modes <- day05_instruct_par_mode(instruction_code)
  v1 <- day05_getv(mode = modes[1], pointer + 1, array)
  v2 <- day05_getv(mode = modes[2], pointer + 2, array)
  rp <- day05_getv(mode = 1, pointer + 3, array)
  new_array <- day05_setv(value = (v1 + v2), rp, array)
  list(array = new_array, in_biffer = in_buffer, out_buffer = out_buffer)
}

#' multiply two numbers
#'
#' @param pointer current position of instruction
#' @param array incode array
#' @param in_buffer vector of values to be read
#' @param out_buffer vector of output
day05_opmul   <- function(pointer, array, in_buffer, out_buffer) {
  instruction_code <- day05_getv(mode = 1, pointer + 0, array)
  modes <- day05_instruct_par_mode(instruction_code)
  v1 <- day05_getv(mode = modes[1], pointer + 1, array)
  v2 <- day05_getv(mode = modes[2], pointer + 2, array)
  rp <- day05_getv(mode = 1, pointer + 3, array)
  new_array <- day05_setv(value = (v1 * v2), rp, array)
  list(array = new_array, in_biffer = in_buffer, out_buffer = out_buffer)
}

#' read input
#'
#' @param pointer current position of instruction
#' @param array incode array
#' @param in_buffer vector of values to be read
#' @param out_buffer vector of output
day05_opinput  <- function(pointer, array, in_buffer, out_buffer) {
  if (length(in_buffer) >= 1) {
    v <- in_buffer[1]
    new_in_buffer <- in_buffer[-1]
    rp <- day05_getv(mode = 1, pointer + 1, array)
    new_array <- day05_setv(value = v, pointer = rp, array)
    list(array = new_array, in_buffer = new_in_buffer, out_buffer = out_buffer)
  } else {
    message <- "empty input buffer"
    stop(message)
  }
}

#' return result (print output)
#'
#' @param pointer current position of instruction
#' @param array incode array
#' @param in_buffer vector of values to be read
#' @param out_buffer vector of output
day05_opoutput <- function(pointer, array, in_buffer, out_buffer) {
  instruction_code <- day05_getv(mode = 1, pointer + 0, array)
  modes <- day05_instruct_par_mode(instruction_code)
  v <- day05_getv(mode = modes[1], pointer + 1, array)
  new_out_buffer <- c(out_buffer, v)
    list(array = array, in_buffer = in_buffer, out_buffer = new_out_buffer)
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
        "invalide mode:", mode,
        "for parameter position", par_position,
        "in operation", op_code
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
#' @param array array of intcodes
day05_getv <- function(mode, pointer, array) {
  if (pointer < 0 | pointer >= length(array)) {
    stop("pointer =", pointer, "refers out of range")
  }
  value <- array[pointer + 1]
  if (mode == 1) {
    value
  } else if (mode == 0) {
    new_pointer <- value
    day05_getv(mode = 1, new_pointer, array)
  }
}

#' Set value
#'
#' @param value parameter value
#' @param pointer pointer to element in array or exact value
#' @param array array of intcodes
day05_setv <- function(value, pointer, array) {
  if (pointer < 0 | pointer >= length(array)) {
    stop("pointer =", pointer, "refers out of range")
  } else {
    array[pointer + 1] <- value
    array
  }
}

#' run diagnostics on INTCODE computer
#'
#' @param input_buffer buffer of input values
#' @param array INCODE array
day05_diagnostic <- function(input_buffer, array) {
  pos <- 0
  temp <- array
  output_buffer <- c()

  #
  next_pos <- function(pos, instruct_code) {
    pos + day05_intruct_length(instruct_code)
  }
  getinstr <- function(pos) day05_getv(mode = 1, pointer = pos, array = temp)
  getopcode <- day05_intruct_op
  decodeop <- day05_decodeop
  getparmodes <- day05_instruct_par_mode

  #
  while (pos %>% getinstr() %>% getopcode() != 99) {

    instruction_code <- getinstr(pos + 0)
    op_code <- getopcode(instruction_code)

    operation <- decodeop(op_code)
    res <- operation(
      pointer = pos, array = temp,
      in_buffer = input_buffer, out_buffer = output_buffer)
    temp <- res$array
    input_buffer <- res$in_buffer
    output_buffer <- res$out_buffer

    pos <- next_pos(pos, instruction_code)

  }

  output_buffer
}

#' Day 05 part 1 solution
#'
#' @export
day05_part1_solution <- function() {
  input_buffer <- c(1)
  input_array  <- aoc19::DATASET$day05
  day05_diagnostic(input_buffer, input_array)
}
