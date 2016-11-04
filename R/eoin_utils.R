
oddcount <- function (xs) {
  k <- 0
  for (x in xs) {
    # %% is the remainder operator
    if (x %% 2 == 1) {
      k <- k + 1
    }
  }
  return(k)
}
