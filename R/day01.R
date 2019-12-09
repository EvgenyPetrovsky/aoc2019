#' calculate fuel for mass
#'
#' @export
#' @param mass mass to be launched
day01_fuel <- function(mass) {
  res <- mass %/% 3 - 2
  mapply(FUN = max, res, 0)
}

#' solution for part 1 of day01 puzzle
#'
#' @export
day01_part1_solution <- function() {
  aoc19::DATASET$day01 %>% day01_fuel() %>% sum()
}

#' fuel adjustment on fuel itself
#'
#' @export
#' @param fuel_mass mass of fuel that needs additional fuel
day01_fuelrec <- function(fuel_mass) {
  if (fuel_mass <= 0) 0
  else fuel_mass + day01_fuelrec(day01_fuel(fuel_mass))
}

#' solution for part 1 of day01 puzzle
#'
#' @export
day01_part2_solution <- function() {
  aoc19::DATASET$day01 %>% day01_fuel() %>% sapply(FUN = day01_fuelrec, USE.NAMES = F) %>% sum()
}
