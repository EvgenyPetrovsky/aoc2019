test_that("rotates around A)B returns list where B -> A", {
  expect_equal(day06_rotates_around("A)B"), list(B = "A"))
  expect_error(
    day06_rotates_around("A.B"), 
    message = 'less than 2 elements')
  expect_error(
    day06_rotates_around("A)B)C"), 
    message = 'more than 2 elements')
})

test_that("rotates around with multiple entries list where B -> A", {
  expect_equal(
    day06_rotates_around(c("A)B", "B)C")), 
    list(B = "A", "C" = "B"))
  expect_equal(
    day06_rotates_around(c("A)B", "B)C", "A)D", "E)B")), 
    list(B = "A", C = "B", D = "A", B = "E"))
  expect_error(
    day06_rotates_around(c("A.B", "B)C", "A)D", "E)B")), 
    message = "less than 2 elements")
  expect_error(
    day06_rotates_around(c("A)B)1", "B)C", "A)D", "E)B")), 
    message = "more than 2 elements")
})

test_that("number of orbits for given map and object", {
  f = function(orbit_code, obj) {
    o <- day06_rotates_around
    day06_count_orbits(o(orbit_code), obj)
  }
  expect_equal(
    f(orbit_code = c("A)B"), obj = "B"),
    list(B = 1))
  expect_equal(
    f(orbit_code = c("A)B"), obj = "A"),
    list(A = 0))
  expect_equal(
    f(orbit_code = c("A)B"), obj = c("A", "B")),
    list(A = 0, B = 1))
  expect_equal(
    f(orbit_code = c("A)B", "B)C"), obj = c("A", "B", "C")),
    list(A = 0, B = 1, C = 2))
  expect_equal(
    f(orbit_code = c("A)B", "A)C"), obj = c("A", "B", "C")),
    list(A = 0, B = 1, C = 1))
})

test_that("number of orbits for object non-existing on the map is 0", {
  f = function(orbit_code, obj) {
    o <- day06_rotates_around
    day06_count_orbits(o(orbit_code), obj)
  }
  expect_equal(
    f(orbit_code = c("A)B"), obj = "D"),
    list(D = 0))
  expect_equal(
    f(orbit_code = c("A)B"), obj = c("B", "C")),
    list(B = 1, C = 0))
  expect_equal(
    f(orbit_code = c("A)B", "B)C"), obj = c("A", "B", "C", "D")),
    list(A = 0, B = 1, C = 2, D = 0))
})

test_that("gather objects finds all objects in map", {
  f <- function(x) x %>% day06_rotates_around() %>% day06_gather_objects()
  expect_equal(f(c("A)B")), c("A", "B"))
  expect_equal(f(c("A)B", "A)C")), c("A", "B", "C"))
  expect_equal(f(c("A)B", "C)D")), c("A", "B", "C", "D"))
  expect_equal(f(c("A)B", "B)C", "C)D", "B)E")), c("A", "B", "C", "D", "E"))
})

test_that("aoc part 1 example works", {
  schema <- c(
    "COM)B","B)C","C)D","D)E","E)F","B)G","G)H","D)I","E)J","J)K","K)L"
  )
  all_obj <- schema %>% day06_rotates_around() %>% day06_gather_objects()

  f <- function(obj) {
    orbit_map <- day06_rotates_around(schema)
    counts <- day06_count_orbits(orbit_map, obj)
    Reduce(f = sum, counts)
  }

  expect_equal(f("D"), 3)
  expect_equal(f("L"), 7)
  expect_equal(f(all_obj), 42)

})

test_that("day 6 part 1 solution is correct", {
  expect_equal(day06_part1_solution(), 223251)
})

test_that("orbit chains for a given map and object", {
  f = function(orbit_code, obj) {
    o <- day06_rotates_around
    day06_build_chains(o(orbit_code), obj)
  }
  expect_equal(
    f(orbit_code = c("A)B"), obj = "B"),
    list(B = "A"))
  expect_equal(
    f(orbit_code = c("A)B"), obj = "A"),
    list(A = character()))
  expect_equal(
    f(orbit_code = c("A)B"), obj = c("A", "B")),
    list(A = character(), B = "A"))
  expect_equal(
    f(orbit_code = c("A)B", "B)C"), obj = c("A", "B", "C")),
    list(A = character(), B = "A", C = c("A","B")))
  expect_equal(
    f(orbit_code = c("A)B", "A)C"), obj = c("A", "B", "C")),
    list(A = character(), B = "A", C = c("A")))
})

test_that("count transfers works", {
  schema <- c("A)B","B)C","A)D","D)E", "C)F", "C)G", "G)I")
  #   D - E
  #  /
  # A - B - C - F
  #          \
  #           G - I
  f <- function(from, to) {
    orbit_map <- day06_rotates_around(schema)
    counts <- day06_count_transfers(orbit_map, from, to)
    counts
  }
  expect_equal(f("B","D"), 0)
  expect_equal(f("B","C"), 1)
  expect_equal(f("E","C"), 2)
  expect_equal(f("E","F"), 3)
  expect_equal(f("E","I"), 4)
})

test_that("aoc part 2 example works", {
  schema <- c(
    "COM)B","B)C","C)D","D)E","E)F","B)G","G)H","D)I","E)J","J)K","K)L","K)YOU",
    "I)SAN"
  )
  #                           YOU
  #                          /
  #         G - H       J - K - L
  #        /           /
  # COM - B - C - D - E - F
  #                \
  #                 I - SAN
  f <- function(from, to) {
    orbit_map <- day06_rotates_around(schema)
    counts <- day06_count_transfers(orbit_map, from, to)
    counts
  }

  expect_equal(f(from = "YOU", to = "SAN"), 4)
})

test_that("day 6 part 2 solution is correct", {
  expect_equal(day06_part2_solution(), 430)
})
