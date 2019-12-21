#' split text into characters
#'
#' @param text text
day08_text2char <- function(text) {
  sapply(
    FUN = function(n) substr(text, n, n),
    X = 1:nchar(text), simplify = TRUE, USE.NAMES = FALSE)
}

#' Split vector of values into chunks of fixed size
#'
#' @param value vector of values
#' @param chunk_size size of one chunk
day08_split2chunks <- function(value, chunk_size) {
  count <- length(value) %/% prod(chunk_size)
  chunks <- (1:count) %>%
    Map(f = function(n) {
      start_pos <- (n-1)*prod(chunk_size) + 1
      end_pos <- (n)*prod(chunk_size)
      value[start_pos:end_pos]})
  chunks
}

#' Day 8 part 1 solution
#'
#' @export
day08_part1_solution <- function() {
  image_size <- c(25,6)
  text <- aoc19::DATASET$day08
  digits <- day08_text2char(text) %>% as.integer()
  layers <- day08_split2chunks(digits, chunk_size = prod(image_size))
  ##
  min_0_cnt <- layers %>%
    Map(f = function(x) sum(x == 0)) %>%
    which.min()
  num_1 <- sum(layers[[min_0_cnt]] == 1)
  num_2 <- sum(layers[[min_0_cnt]] == 2)

  num_1 * num_2
}


#' Look at pixel trhough layeers and decide what you see
#'
#' Pixel values are 0 - black, 1 - white, 0 - transparent
#' @param prev_val - value of pixel created on previous layers
#' @param lower_val - value of pixel in current layer
day08_trace_pixel <- function(prev_val, lower_val) {
  ifelse(prev_val == 2, lower_val, prev_val)
}

#' determine which pixels are visible for stack of layers
#'
#' @param layers layers
day08_top_visible_pixel <- function(layers) {
  Reduce(f = day08_trace_pixel, x = layers)
} 

#' Day 8 part 2 solution
#'
#' @export
day08_part2_solution <- function() {
  image_size <- c(25,6)
  text <- aoc19::DATASET$day08
  digits <- day08_text2char(text) %>% as.integer()
  layers <- day08_split2chunks(digits, chunk_size = prod(image_size))
  ##
  day08_top_visible_pixel(layers) %>% paste(collapse = "")
}
