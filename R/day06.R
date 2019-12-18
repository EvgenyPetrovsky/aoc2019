#' Rotates around analyzes object relationships and return list of objects
#' that point to their direct centers of orbits
#'
#' @export
#' @param orbital_rln of orbital relation codes "A)B" where
#'  A is direct center of rotation and
#'  B rotating object
day06_rotates_around <- function(orbital_rln) {
  strsplit(orbital_rln, ")") %>%
    day06_check_splits() %>%
    Map(f = function(a_b) {l = list(); l[a_b[2]] <- a_b[1]; l}) %>%
    Reduce(f = c)
}

#' make list of all objects
#'
#' @param orbit_map object relation map
day06_gather_objects <- function(orbit_map) {
  obj <-
    c(names(orbit_map), sapply(FUN = c, orbit_map)) %>%
    unique() %>%
    sort()
  obj
}


#' check that all splits have 2 elements
#'
#' @param splits - list of splits
day06_check_splits <- function(splits) {
  ll <- splits %>% sapply(FUN = length)
  if (min(ll) != 2) {
    stop("splits have less than 2 elements. min value:", min(ll))
  } else if (max(ll) != 2) {
    stop("splits have more than 2 elements. max value:", max(ll))
  } else {
    splits
  }
}

#' build chains of orbits towards center of everything
#'
#' Function returns result as a list of object names and
#' vectors of objects between them and center of everything
#'
#' @export
#' @param orbit_map object relation map
#' @param obj vector of object for which distance needs to be calculated
day06_build_chains <- function(orbit_map, obj) {
  # iteration function
  iter <- function(cache, object) {
    if (object %in% names(cache)) {
      cache
    } else if (!object %in% names(orbit_map)) {
      cache[[object]] <- character()
      cache
    } else {
      center_object <- orbit_map[[object]]
      c_cache <- iter(cache, center_object)
      c_cache[[object]] <- c(c_cache[[center_object]], center_object)
      c_cache
    }
  }
  # initial call
  cache <- list()
  chains <- Reduce(f = iter, x = obj, init = cache)
  # return sorted list
  chains[sort(obj)]
}

#' calculate number of orbits for given map and object code
#'
#' Function returns result as a list of object names and
#' number of their direct + indirect orbits
#'
#' @export
#' @param orbit_map object relation map
#' @param obj vector of object for which distance needs to be calculated
day06_count_orbits <- function(orbit_map, obj) {
  chains <- day06_build_chains(orbit_map, obj)
  counts <- chains %>%
    Map(f = length)
  # return sorted list
  counts[sort(obj)]
}

#' day 6 part 1 solution
#'
#' @export
day06_part1_solution <- function() {
  #values
  schema  <- aoc19::DATASET$day06
  all_obj <- schema %>% day06_rotates_around() %>% day06_gather_objects()
  # functions
  f <- function(obj) {
    orbit_map <- day06_rotates_around(schema)
    day06_count_orbits(orbit_map, obj)
  }
  sum_total <- function(count_map) {
    Reduce(f = sum, count_map)
  }
  # result
  f(all_obj) %>% sum_total()
}

day06_count_transfers <- function(orbit_map, from_obj, to_obj) {
  chains <- day06_build_chains(orbit_map, c(from_obj, to_obj))
  chain1 <- chains[[1]]
  chain2 <- chains[[2]]

  count  <- c(setdiff(chain1, chain2), setdiff(chain2,chain1)) %>% length()
  count
}

#' day 6 part 1 solution
#'
#' @export
day06_part2_solution <- function() {
  #values
  schema  <- aoc19::DATASET$day06
  # functions
  f <- function(from, to) {
    orbit_map <- day06_rotates_around(schema)
    counts <- day06_count_transfers(orbit_map, from, to)
    counts
  }
  # result
  f(from = "YOU", to = "SAN")
}