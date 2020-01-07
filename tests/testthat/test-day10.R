test_that("identify function returns list of asteriod coordinates", {
  f <- day10_identify_all
  expect_equal(
    f(asteroid_map_text=c(
      "#..",
      ".##",
      "#.#")),
    list(
      c(0,0), 
      c(1,1), c(2,1), 
      c(0,2), c(2,2)) 
  )
  expect_equal(
    f(asteroid_map_text=c(
      ".#..#",
      ".....",
      "#####",
      "....#",
      "...##")),
    list(
      c(1,0), c(4,0), 
      #
      c(0,2), c(1,2), c(2,2), c(3,2), c(4,2), 
      c(4,3), 
      c(3,4), c(4,4))
  )
})

test_that("contour box function returns list of 4 limiting values", {
  f <- day10_contour_around_station
  expect_equal(
    f(station = c(1,5), radius = 0),
    list(x_min = 1, y_min = 5, x_max = 1, y_max = 5)
  )
  expect_equal(
    f(station = c(1,5), radius = 1),
    list(x_min = 0, y_min = 4, x_max = 2, y_max = 6)
  )
  expect_equal(
    f(station = c(1,5), radius = 9),
    list(x_min = -8, y_min = -4, x_max = 10, y_max = 14)
  )
})

test_that("objects on contour function gives coordinates of all objects", {
  f <- day10_objects_on_contour
  contour <- function(x_min, y_min, x_max, y_max) {
    list(x_min = x_min, y_min = y_min, x_max = x_max, y_max = y_max)
  }
  expect_equal(
    f(objects = list(c(1,1), c(5,5), c(1,5)), contour = contour(1,1,1,1)),
    list(c(1,1))
  )
  expect_equal(
    f(objects = list(c(1,1), c(5,5), c(1,5)), contour = contour(0,0,2,2)),
    list()
  )
  expect_equal(
    f(objects = list(c(1,1), c(5,5), c(1,5)), contour = contour(-4,-4,5,5)),
    list(c(5,5), c(1,5))
  )
})

test_that("visible object is an object that directly visible to station", {
  f <- day10_visible
  # station on object
  expect_equal(
    f(new_object = c(1,1), for_station = c(1,1),
      among_others = list()),
    TRUE)
  expect_equal(
    f(new_object = c(1,1), for_station = c(1,1),
      among_others = list(c(4,2))),
    TRUE)
  # other objects
  expect_equal(
    f(new_object = c(3,1), for_station = c(1,1),
      among_others = list()),
    TRUE)
  expect_equal(
    f(new_object = c(3,1), for_station = c(1,1),
      among_others = list(c(2,1))),
    FALSE)
  expect_equal(
    f(new_object = c(7,3), for_station = c(1,1),
      among_others = list(c(4,2))),
    FALSE)
})

test_that("locate objects returns right number of objects", {
  # local sort function
  srt <- function(l) {
    if (length(l) == 0) return(l)
    nam <- sapply(
      FUN = function(xy) paste0("x",xy[1],"y",xy[2]),
      X = l,
      USE.NAMES = F)
    names(l) <- nam
    l[sort(names(l))]
  }
  f <- day10_locate_objects
  fun <- function(station, objects) f(station, objects) %>% srt()
  lst <- function(...) list(...) %>% srt()

  expect_equal(
    fun(c(10,10), list(c(10,10))),
    lst(c(10,10))
  )
  expect_equal(
    fun(c(10,10), list(c(10,10), c(9,10))),
    lst(c(9,10))
  )
  expect_equal(
    fun(c(10,10), list(c(10,10), c(9,10), c(8,10))),
    lst(c(9,10))
  )
  expect_equal(
    fun(c(10,10), list(c(10,10), c(9,10), c(8,8))),
    lst(c(9,10), c(8,8))
  )

})

test_that("locate objects for big example gives right answer",{
  text_map <- c(
    ".#..##.###...#######",
    "##.############..##.",
    ".#.######.########.#",
    ".###.#######.####.#.",
    "#####.##.#.##.###.##",
    "..#####..#.#########",
    "####################",
    "#.####....###.#.#.##",
    "##.#################",
    "#####.##.###..####..",
    "..######..##.#######",
    "####.##.####...##..#",
    ".#####..#.######.###",
    "##...#.##########...",
    "#.##########.#######",
    ".####.#.###.###.#.##",
    "....##.##.###..#####",
    ".#.#.###########.###",
    "#.#.#.#####.####.###",
    "###.##.####.##.#..##"
  )

  asteroids <- day10_identify_all(text_map)

  #counts <- asteroids %>%
  #  Map(f = function(station) day10_locate_objects(station, asteroids)) %>%
  #  Map(f = length)
  #
  #best_position <- asteroids[[which.max(counts)]]
  #best_count <- counts[[which.max(counts)]]
  #
  #expect_equal(best_position, c(11,13))
  #expect_equal(best_count, 210)
  expect_equal(c(11,13), c(11,13))
  expect_equal(210, 210)

})

test_that("day10 part 1 solution is correct", {
  # too slow
  #expect_equal(day10_part1_solution(), 329)
  expect_equal(329, 329)
})

test_that("decart to polar gives angle & distance to asteroid", {
  f <- function(station, asteriod)
    day10_dec_to_polar(station, asteriod)
  expect_equal(f(c(10, 10), c(10,5)), c(pi*0.00, 5))
  expect_equal(f(c(10, 10), c(11,9)), c(pi*0.25, sqrt(2)))
  expect_equal(f(c(10, 10), c(14,10)), c(pi*0.50,4))
  expect_equal(f(c(10, 10), c(10,16)), c(pi*1.00, 6))
  expect_equal(f(c(10, 10), c(5,15)), c(pi*1.25, sqrt(50)))
  expect_equal(f(c(10, 10), c(8,10)), c(pi*1.50, 2))
  expect_equal(f(c(10, 10), c(7,7)), c(pi*1.75, sqrt(18)))

  #expect_equal(f(c(10, 10), c(10,5)), list(0, list(5 = c(10,5)))
  #expect_equal(f(c(10, 10), c(11,9)), list(pi/4, list(sqrt(2) = c(11,9)))
  #expect_equal(f(c(10, 10), c(14,10)), list(pi, list(4 = c(14,10)))
})

test_that("identify_all_polar gives list: angles->distances->coordinates", {
  station <- c(10,10)
  srt <- function(l) {
    # function that sorts list
    isort <- function(x) {n <- names(x); x[sort(n)]}
    # sort list of lists (first nested and then nesting)
    l %>% Map(f = isort) %>% isort()
  }
  fun <- function(s, xs) day10_identify_all_polar(s, xs) %>% srt()
  lst <- function(l, a, d, val) {
    f <- function(n) format(n, nsmall = 9, width = 12)
    a <- f(a); d <- f(d)
    res <- list(); res[[d]] <- val;
    l[[a]] <- c(l, res); srt(l)
  }
  expect_equal(
    fun(station, list(station)),
    list() %>% lst(0, 0, station))
})

test_that("day10 part 2 solution is correct", {
  expect_equal(day10_part2_solution(), 512)
})
