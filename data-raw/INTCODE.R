## code to prepare `INTCODE` dataset goes here

INTCODE <- list()
INTCODE$opcode <- list(
  "01" = 4, "02" = 4, "03" = 2, "04" = 2, 
  "05" = 3, "06" = 3, "07" = 4, "08" = 4,
  "09" = 2, "99" = 1
)
INTCODE$valid_mode <- list(
  "01" = list(c(0,1,2), c(0,1,2), c(0,2)),
  "02" = list(c(0,1,2), c(0,1,2), c(0,2)),
  "03" = list(c(0,2)),
  "04" = list(c(0,1,2)),
  "05" = list(c(0,1,2), c(0,1,2)),
  "06" = list(c(0,1,2), c(0,1,2)),
  "07" = list(c(0,1,2), c(0,1,2), c(0,2)),
  "08" = list(c(0,1,2), c(0,1,2), c(0,2)),
  "08" = list(c(0,1,2), c(0,1,2), c(0,2)),
  "09" = list(c(0,1,2)),
  "99" = NULL
)
##
usethis::use_data(INTCODE, overwrite = T)
