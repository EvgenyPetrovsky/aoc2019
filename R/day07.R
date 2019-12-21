#' permutations of vector ov values
#'
#' @export
#' @param values vector of values to be permutated
day07_permutations <- function(values) {
  if (length(values) == 0) {
    stop("nothing to permutate")
  }
  # takes vector, returns lists
  iterate <- function(v) {
    if (length(v) == 1) {
      v
    } else {
      n <- length(v)
      l0 <- list()
      for (i in 1:n) {
        v0 <- v[i]; remain <- v[-i]
        lists <- iterate(remain)
        res <- da07_add_value_to_list_elements(lists, v0)
      l0 <- c(l0, res)
      }
      l0
    }
  }
  iterate(values)
}

#' add value into every element of list
#'
#' This is supplementary function for permutations
#'
#' @param list_of_values input list that needs to be extended with value
#' @param value value to all into every element of list
da07_add_value_to_list_elements <- function(list_of_values, value) {
  list_of_values %>% Map(f = function(x) c(x, value))
}

#' Run circuit of amplifiers
#'
#' @param input_instructions vector of input instructions for every amplifier
#' @param array INTCODE array
day07_run_curcuit <- function(input_instructions, array) {
  amp_input <- 0
  for (i in 1:5) {
    in_buffer <- c(input_instructions[i], amp_input)
    out_buffer <- day05_diagnostic(in_buffer, array)
    amp_input <- out_buffer[1]
  }
  out_buffer[1]
}

#' Day 7 part 1 solution
#'
#' @export
day07_part1_solution <- function() {
  array <- aoc19::DATASET$day07
  # run circuit for all combinations
  perm  <- day07_permutations(0:4)
  outputs <- perm %>% Map(f = function(i) day07_run_curcuit(i, array))
  # find maximum output
  idx_max_s <- which.max(outputs)
  i_max_sig <- perm[[idx_max_s]]
  max_sig <- outputs[[idx_max_s]]
  max_sig
}

#' Run circuit of amplifiers with feedback loop
#'
#' @export
#' @param input_instructions vector of input instructions for every amplifier
#' @param array INTCODE array
day07_run_feedback_curcuit <- function(input_instructions, array) {
  amp_input <- 0
  out_buffer <- integer()
  amp_software <- 1:5 %>% Map (f = function(x) array)
  while (!is.null(amp_input)) {
    last_out <- amp_input
    for (i in 1:5) {
      # restore software of amplifier
      array <- amp_software[[i]]
      # run program
      buffer <- c(input_instructions[i], amp_input)
      run_res <- day05_run_computer(buffer, array)
      buffer <- run_res$buffer
      # store sofware state of amplifier
      amp_software[[i]] <- run_res$array
      amp_input <- buffer[1]
      if (length(amp_input) == 0) {
        break
      }
    }
  }
  last_out
}
