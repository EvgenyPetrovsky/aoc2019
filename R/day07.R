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

day07_run_curcuit <- function(input_instructions) {
  NULL
}
