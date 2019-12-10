#' Generate sequence of numbers based on value of low and high bound
#'
#' Convert numbers into string format after validation
#'
#' @param low low bound of range
#' @param high high bound of range
day04_makesequence <- function(low, high) {
  numbers <- seq(from = low, to = high, by = 1)
  as.character(numbers)
}

#' Function that generalizes 2 types of checks using binary check operator and
#'
#' function that folds outcomes to say whether check passes or not
#' @param number value to analyze
#' @param check_fun function to apply to check values
#' @param fold_fun function to apply to fold results of check
day04_docheck <- function(number, check_fun, fold_fun) {
  n <- nchar(number)
  v <- sapply(
    FUN = function(x) substr(number,x,x),
    X = 1:n, USE.NAMES = F, simplify = T)
  if (n == 1) {
    TRUE
  } else if (n > 1) {
    b <- sapply(
      FUN = function(pos) check_fun(v[pos-1], v[pos]),
      X = 2:n, USE.NAMES = F, simplify = T
    )
    fold_fun(b)
  } else {
    stop(paste("invalid number", number))
  }

}

#' Check whether digits of number all go in asscending order
#'
#' Where 00111 and 12345 are valid and 12321 is not. Function works with atomic
#' values only
#' @param number number to be checked
day04_filterasc <- function(number) {
  check_fun <- function(x, y) x <= y
  fold_fun <- all
  day04_docheck(number, check_fun, fold_fun)
}

#' Check whether number has at least to adjacent digits
#'
#' where 00111 and 11322 are valid and 12321 is not. Function works with atomic
#' values only
#' @export
#' @param number number to be checked
day04_filteradj <- function(number) {
  check_fun <- function(x, y) x == y
  fold_fun <- any
  day04_docheck(number, check_fun, fold_fun)
}

#' Day 04 part 1 solution
#' @export
day04_part1_solution <- function() {
  ns <- strsplit(aoc19::DATASET$day04, split = "-",fixed = T)[[1]]
  rs <- day04_makesequence(ns[1], ns[2]) %>%
    Filter(f = function(x) all(day04_filterasc(x), day04_filteradj(x)))
  length(rs)
}

#' Check whether adjacent digits are not part of bigger group
#'
#' where 00111 and 11322 are valid and 12321 is not. Function works with atomic
#' values only
#'
#' @param number number to be checked
day04_filteradj2 <- function(number) {
  n <- nchar(number)
  v <- sapply(
    FUN = function(x) substr(number,x,x),
    X = 1:n, USE.NAMES = F, simplify = T)
  v <- c("X", v, "X")
  checkfun <- function(pos) {
    all(
      v[pos-1] != v[pos],
      v[pos+1] == v[pos],
      v[pos+2] != v[pos]
    )
  }
  if (n >= 4) {
    b <- sapply(
      FUN = function(pos) checkfun(pos),
      X = 2:(length(v)-2), USE.NAMES = F, simplify = T
    )
    # if any of digits go in pair (no triple) - TRUE
    any(b)
  } else {
    stop(paste("invalid number", number))
  }

}

#' Day 04 part 1 solution
#'
#' @export
day04_part2_solution <- function() {
  ns <- strsplit(aoc19::DATASET$day04, split = "-",fixed = T)[[1]]
  rs <- day04_makesequence(ns[1], ns[2]) %>%
    Filter(f = day04_filterasc) %>%
    Filter(f = day04_filteradj) %>%
    Filter(f = day04_filteradj2)
  length(rs)
}
