#' Central point - starting position
day03_centralpoint <- c(0, 0)
day03_initpath <- function(point) {
  data.frame(
    x = point[1],
    y = point[2]
  )
}

#' Manhettan distance
#'
#' @param point pair numbers representing X and Y coordinate
#' @param ref_point pair numbers representing X and Y coordinate
day03_mhtdist <- function(point, ref_point = c(0,0)) {
  x1 <- point[1]; x2 <- ref_point[1]
  y1 <- point[2]; y2 <- ref_point[2]
  dist <- abs(x1 - x2) + abs(y1 - y2)
  dist
}

#' current position
#'
#' @param path full path
day03_position <- function(path) {
  last_step <- nrow(path)
  x <- path[last_step,]$x
  y <- path[last_step,]$y
  c(x, y)
}

#' Translate instruction into directoin vector (x, y)
#'
#' R : (1, 0); U : (0, 1); L : (-1, 0); D : (0, -1)
#' @param instruction string that defines direction as a first character and
#'   number of steps as rest
day03_path_direction <- function(instruction) {
  dir_encoder = list(
    R = c(1, 0),
    U = c(0, 1),
    L = c(-1, 0),
    D = c(0, -1)
  )
  dir_code <- substr(instruction, 1, 1)
  if (! dir_code %in% names(dir_encoder)) {
    message <- paste("unknown direction ", dir_code, " in instruction ", instruction, "")
    stop(message)
  } else {
    dir_encoder[[dir_code]]
  }
}

#' Translate instruction into number of steps
#'
#' @param instruction string that defines direction as a first character and
#'   number of steps as rest
day03_path_length <- function(instruction) {
  valid <- grepl(pattern = "^(R|U|L|D)\\d+$", x = instruction)
  if (!valid) {
    stop(paste("instruction ", instruction, " has insorrect format"))
  }
  steps <- substr(instruction, 2, 1000000L)
  as.integer(steps)
}

#' build delta to the path according
#'
#' @param current_pos current position (to start from)
#' @param direction direction to move in
#' @param steps number of steps to make
day03_deltapath <- function(current_pos, direction, steps) {
  grid <- expand.grid(
    dx = direction[1],
    dy = direction[2],
    n  = 1:steps,
    x0 = current_pos[1],
    y0 = current_pos[2]
  )
  grid$x <- with(grid, x0 + n * dx)
  grid$y <- with(grid, y0 + n * dy)

  grid[, c("x", "y")]
}

#' Build path from starting point
#'
#' Returns data frame with coordinates of path
#' @param instructions vector of step insructions like "R75","D30","R83"
#' @param start_point vector of 2 values representing starting point for path
day03_fullpath <- function(instructions, start_point = day03_centralpoint) {
  walk <- function(path, instruction) {
    pos <- day03_position(path)
    dir <- day03_path_direction(instruction)
    stp <- day03_path_length(instruction)

    delta_path <- day03_deltapath(pos, dir, stp)
    rbind(path, delta_path)
  }
  Reduce(f = walk, x = instructions, init = day03_initpath(start_point))
}

#' Find all intersections of 2 pathes
#'
#' Returns pairs of x and y coordinates in data frame
#'
#' @param path1 path
#' @param path2 path
day03_intersection <- function(path1, path2) {
  p1 <- path1 %>% unique()
  p2 <- path2 %>% unique()
  merge(p1, p2, by = c("x", "y"))
}

#' Fund find closest (manhettan distance) intersection of 2 wires defined by instruction sets
#'
#' @param instructions1 instructions to build path 1
#' @param instructions2 instructions to build path 2
day03_part1 <- function(instructions1, instructions2) {
  path1 <- day03_fullpath(instructions = instructions1)
  path2 <- day03_fullpath(instructions = instructions2)

  cross <- day03_intersection(path1[-1,], path2[-1,])

  zip2 <- mapply(FUN = c, cross$x, cross$y, SIMPLIFY = F)

  cross$dist <- sapply(
    FUN = day03_mhtdist, zip2,
    simplify = T, USE.NAMES = F)

  min(cross$dist)
}

#' solution of Day 3 part 1
#'
#' @export
day03_part1_solution <- function() {
  i1 <- aoc19::DATASET$day03$w1
  i2 <- aoc19::DATASET$day03$w2
  day03_part1(i1, i2)
}

#' Find shortest (wire length) intersection of 2 wires defined by instruction sequences
#'
#' @param instructions1 instructions to build path 1
#' @param instructions2 instructions to build path 2
day03_part2 <- function(instructions1, instructions2) {
  path1 <- day03_fullpath(instructions = instructions1)
  path1$n1 <- 1:nrow(path1) - 1
  path2 <- day03_fullpath(instructions = instructions2)
  path2$n2 <- 1:nrow(path2) - 1

  cross <- day03_intersection(path1[-1,], path2[-1,])

  cross$dist <- with(cross, n1 + n2)

  min(cross$dist)
}

#' solution of Day 3 part 2
#'
#' @export
day03_part2_solution <- function() {
  i1 <- aoc19::DATASET$day03$w1
  i2 <- aoc19::DATASET$day03$w2
  day03_part2(i1, i2)
}
