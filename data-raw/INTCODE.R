## code to prepare `INTCODE` dataset goes here

INTCODE <- list()
INTCODE$opcode <- list(
  "01" = 4, "02" = 4, "03" = 2, "04" = 2, "99" = 1
)

##
usethis::use_data(INTCODE, overwrite = T)
