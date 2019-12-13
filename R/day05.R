
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
#' @param v1 value 1
#' @param v2 value 2
day05_opsum   <- function(v1, v2) {
  v1 + v2
}

#' multiply two numbers
#'
#' @param v1 value 1
#' @param v2 value 2
day05_opmul   <- function(v1, v2) {
  v1 * v2
}

#' read input
#'
#' @param in_buffer vector of values to be read
day05_opinput  <- function(in_buffer) {
  if (length(in_buffer) >= 1) {
    v <- in_buffer[1]
    new_buffer <- in_buffer[-1]
    list(v, new_buffer)
  }
}

# return result (print output)
#'
#' @param v value
#' @param out_buffer destination vector of values
day05_opoutput <- function(v, out_buffer) {
  c(out_buffer, v)
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


day05_diagnostic <- function() {
    NULL
}

#' Length of instruction - how many registers belong to instruction
#'
#' @param instruction instruction integer code
day05_intruct_length <- function(instruction) {
  op_code <- day05_intruct_op(instruction)
  INTCODE$opcode[[op_code]]
}

#' instruction parameter modes
#'
#' @export
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
  if (mode == 1) {
    pointer
  } else if (mode == 0) {
    if (pointer < 0 | pointer >= length(array)) {
      stop("pointer =", pointer, "refers out of range")
    } else {
      array[pointer + 1]
    }
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