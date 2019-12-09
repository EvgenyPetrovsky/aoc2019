#' Day 2 solution
#'
#' Function takes vector of values and returns modified vector
#'
#' @param input vector of integer values
day02 <- function(input) {
  #
  temp <- input

  #
  setv <- function(value, pos) temp[pos+1] <<- value
  getv <- function(pos) temp[pos+1]
  nexp <- function(pos) pos + 4

  #
  decodeop <- function(opcode) {
    op <-
      if (opcode == 1) {
        sum
      } else if (opcode == 2) {
        prod
      } else {
        stop(paste("Invalid operation code:", opcode))
      }
    op
  }

  #
  pos <- 0
  while (getv(pos) != 99) {
    opcode <- getv(pos + 0)
    respos <- (pos + 3) %>% getv()

    op <- decodeop(opcode)
    v1 <- (pos + 1) %>% getv() %>% getv()
    v2 <- (pos + 2) %>% getv() %>% getv()

    res <- op(v1, v2)

    setv(res, respos)

    pos <- nexp(pos)

  }

  res <- temp

  res
}

#' puzzle requires input modification
#'
#' @param input input vector of integers
#' @param v1 value to place into 2nd element of input vector
#' @param v2 value to place into 3rd element of input vector
day02_modify_input <- function(input, v1, v2) {
  temp <- input

  setv <- function(value, pos) temp[pos+1] <<- value

  setv(v1, 1); setv(v2, 2)

  temp
}

#' solution of Day 2 part 1
#'
#' @export
day02_part1_solution <- function() {
  aoc19::DATASET$day02 %>% day02_modify_input(12, 2) %>% day02()
}

#' find combination of word and noun
#'
#' @param input input
#' @param expected_result expected result
day02_noun_verb <- function(input, expected_result) {
  grid <- expand.grid(noun = 0:99, verb = 0:99)
  fun <- function(noun, verb) {
    input <- aoc19::DATASET$day02 %>% day02_modify_input(noun, verb)
    res <- day02(input)
    res[1]
  }
  grid$res <- mapply(FUN = fun, grid$noun, grid$verb, USE.NAMES = F)
  res <- subset(grid, res == expected_result) %>% utils::head(1)
  c(res$noun, res$verb)
}

#' solution of Day 2 part 1
#'
#' @export
day02_part2_solution <- function() {
  verb_noun <- aoc19::DATASET$day02 %>% day02_noun_verb(expected_result = 19690720)
  noun <- verb_noun[1]; verb <- verb_noun[2]
  res <- 100 * noun + verb
  res
}
