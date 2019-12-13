## code to prepare `INTCODE` dataset goes here

INTCODE <- list()
INTCODE$opcode <- list(
  "01" = 4, "02" = 4, "03" = 2, "04" = 2, "99" = 1
)
INTCODE$valid_mode <- list(
  "01" = list(c(0,1), c(0,1), c(0)),
  "02" = list(c(0,1), c(0,1), c(0)),
  "03" = list(c(0)),
  "04" = list(c(0,1)),
  "99" = NULL
)
##
usethis::use_data(INTCODE, overwrite = T)
