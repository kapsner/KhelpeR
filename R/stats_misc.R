# calculate standard error
# standard error (https://rcompanion.org/rcompanion/c_03.html)
se <- function(vector) {
  return(stats::sd(vector, na.rm = T) / sqrt(length(vector[!is.na(vector)])))
}

distribution <- function(dataset, var, digits = 2) {
  vec <- round(dataset[!is.na(get(var)), get(var)], digits)
  ret <- paste(round(base::min(vec), digits),
               round(base::mean(vec), digits),
               round(stats::median(vec), digits),
               round(base::max(vec), digits),
               paste0("\u00B1", round(stats::sd(vec), digits)),
               sep = "/ ")
  return(ret)
}

distribution_2 <- function(vector, digits = 2) {
  vec <- round(vector, 2)
  ret <- paste0(
    min(vec, na.rm = T), "/ ",
    round(mean(vec, na.rm = T), digits), "/ ",
    median(vec, na.rm = T), "/ ",
    max(vec, na.rm = T), " (",
    paste0("±", round(sd(vec, na.rm = T), digits)),
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
