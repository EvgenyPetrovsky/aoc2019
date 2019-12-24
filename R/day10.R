#' Identify all asteroids on a matrix map
#'
#' Map is represented by vectod of strings wirh values # and .
#'
#' @param asteroid_map_text text representation of map
day10_identify_all <- function(asteroid_map_text) {
  text <- asteroid_map_text
  rows <- length(text)
  cols <- nchar(text) %>% min()
  res <- list()

  for (row in 1:rows) {
    for (col in 1:cols) {
      if (substr(text[row], col, col) == "#") {
        res <- c(res, list(c(row, col)))
      }
    }
  }

  res
}

#' Identify boundaries to scan with station radar
#'
#' This is square around station
#' @param station station coordinates
#' @param radius distance (shortest) from station to boundary
day10_contour_around_station <- function(station, radius) {
  x <- station[1]
  y <- station[2]
  r <- radius
  list(x_min = x-r, y_min = y-r, x_max = x+r, y_max = y+r)
}

#' Identify all objects that are located on contour line
#'
#' @param objects complete set of asterods
#' @param contour contour boundaries
day10_objects_on_contour <- function(objects, contour) {
  obj_on_contour <- function(obj) {
    x <- obj[1]; y <- obj[2]
    with(contour, {
      if (x == x_min & y >= y_min & y <= y_max) TRUE
      else if (y == y_min & x >= x_min & x <= x_max) TRUE
      else if (x == x_max & y >= y_min & y <= y_max) TRUE
      else if (y == y_max & x >= x_min & x <= x_max) TRUE
      else FALSE
    })
  }
  objects %>%
    Filter(f = obj_on_contour)
}

#' Identify whether asteroid is visible to station
#'
#' Asteroid where station is installed is visible
#'
#' @param new_object asteriod coordinates that needs to be checked
#' @param for_station station coordinates
#' @param among_others previousy identified asteroids
day10_visible <- function(new_object, for_station, among_others) {

  # if coordinates of new objec and station are equal then visible
  if (all(new_object == for_station)) {
    return(TRUE)
  }

  is_between_station_and_new_object <- function(test_object) {
    x_t <- test_object[1]
    y_t <- test_object[2]
    x_s <- for_station[1]
    y_s <- for_station[2]
    x_n <- new_object[1]
    y_n <- new_object[2]

    # if all both asteroids located on the same x coordinate
    # and on one side from station then object is in between
    if (x_s == x_t & x_s == x_n & sign(y_t - y_s) == sign(y_n - y_s)) {
      return(TRUE)
    }
    if (x_s == x_t & x_s == x_n & sign(y_t - y_s) != sign(y_n - y_s)) {
      return(FALSE)
    }
    # if only 2 asteroids are locared on the same x coordinate
    # then none can be in between others
    if (all(x_s == x_t, x_s != x_n)) {
      return(FALSE)
    }
    if (all(x_s != x_t, x_s == x_n)) {
      return(FALSE)
    }

    # for the rest we will calculate b - slope of the line between
    # station and asteroid and compare to slope
    # between station and new asteriod
    b_t <- (y_t - y_s) / (x_t - x_s)
    b_n <- (y_n - y_s) / (x_n - x_s)

    # if slopes are the same and both objects are located
    # on one side from station then object is in between
    if (all(b_t == b_n, sign(x_t - x_s) == sign(x_n - x_s))) TRUE
    else FALSE
  }

  # find objects between station and asteroid
  objects_between <-
    among_others %>%
    Filter(f = is_between_station_and_new_object)

  # asteroid is visible only when no objects between it and station
  length(objects_between) == 0

}
#' Locate all asteroids visible to station
#'
#' @param station station coordinates
#' @param objects asteroids coordinates
day10_locate_objects <- function(station, objects) {
  #determine max redius to scan around station
  max_radius <- objects %>%
    Map(f = function(o) {
      max(abs(o[1]-station[1]), abs(o[2]-station[2]))
    }) %>%
    Reduce(f = max)

  contour <- function(radius)
    day10_contour_around_station(station, radius)
  objects_on_contour <- function(contour)
    day10_objects_on_contour(objects, contour)
  is_visible <- function(object, among_others)
    day10_visible(object, station, among_others)

  visible_objects <- 1:max_radius %>%
    Reduce(f = function(closer_objects, radius) {
      radius %>%
        contour() %>%
        objects_on_contour() %>%
        Filter(f = function(object) is_visible(object, closer_objects)) %>%
        # add new visible objects to previous
        c(closer_objects, .)
    },
    init = list())
  #print(paste(
  #  "station at coordinate:", station[1], station[2],
  #  "visible asteroids:", length(visible_objects)))
  visible_objects
}

#' Day 10 part 1 solution
#'
#' @export
day10_part1_solution <- function() {
  text_map <- aoc19::DATASET$day10
  asteroids <- day10_identify_all(text_map)

  count_max <- asteroids %>%
    Map(f = function(station) day10_locate_objects(station, asteroids)) %>%
    Map(f = length) %>%
    Reduce(f = max)

  count_max
}

#' Transform coordinates from decart to polar system
day10_dec_to_polar <- function(station, asteroid) {
  x_0 <- station[1]
  y_0 <- station[2]
  x_1 <- asteroid[1]
  y_1 <- asteroid[2]

  distance <-
    if (all(station == asteroid)) 0
    else {
      sqrt((x_0 - x_1)^2 + (y_0 - y_1)^2)
    }
  # andge expressed in radians,
  # 0 angle points straight up from 0 point
  # pi/2 angle points to the right
  # pi angle points down
  angle <-
    if (distance == 0) 0
    else {
      # y coordinate flipped to make it feeling more natural
      # (positive values above 0 point)
      unsigned_angle <- asin((y_1 - y_0)/distance)
      # identify 3rd and 4th quarters
      signed_angle <- ifelse(
        x_1 - x_0 < 0, 
        pi - unsigned_angle, 
        unsigned_angle)
      # rotate phase by 90 degrees (pi/2)
      shift_angle <- signed_angle + pi/2
      # represent value of angle between 0 and 2*pi
      angle <- (shift_angle + 2*pi) %% (2*pi)
      angle
    }
  # return combination of angle and distance
  c(angle, distance)
}
