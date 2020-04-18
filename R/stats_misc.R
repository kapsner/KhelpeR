# calculate standard error
# standard error (https://rcompanion.org/rcompanion/c_03.html)
se <- function(vector) {
  return(stats::sd(vector, na.rm = T) / sqrt(length(vector[!is.na(vector)])))
}

distribution <- function(vector, digits = 2) {
  vec <- round(vector, digits)
  ret <- paste0(
    min(vec, na.rm = T), "/ ",
    round(mean(vec, na.rm = T), digits), "/ ",
    stats::median(vec, na.rm = T), "/ ",
    max(vec, na.rm = T), " (",
    paste0("\u00B1", round(stats::sd(vec, na.rm = T), digits)),
    ")"
  )
  return(ret)
}

# mode
mode_fn <- function(vector) {
  # https://stackoverflow.com/questions/2547402/is-there-a-built-in-function-
  # for-finding-the-mode
  ux <- unique(vector)
  return(ux[which.max(tabulate(match(vector, ux)))])
}
