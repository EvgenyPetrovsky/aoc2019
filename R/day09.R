#' Day 09 part 1 solution
#'
#' @export
day09_part1_solution <- function() {
  input_buffer <- c(1)
  input_array  <- aoc19::DATASET$day09

  res <- day05_run_computer(input_buffer, input_array)
  res$buffer
}

#' Day 09 part 2 solution
#'
#' @export
day09_part2_solution <- function() {
  input_buffer <- c(2)
  input_array  <- aoc19::DATASET$day09

  res <- day05_run_computer(input_buffer, input_array)
  res$buffer
}
