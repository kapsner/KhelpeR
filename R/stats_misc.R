# calculate standard error
# standard error (https://rcompanion.org/rcompanion/c_03.html)
se <- function(vector){
  return(stats::sd(vector, na.rm = T) / sqrt(length(vector[!is.na(vector)])))
}

distribution <- function(dataset, variable, digits = 2){
  vec <- round(dataset[!is.na(get(variable)),get(variable)], digits)
  ret <- paste(round(base::min(vec), digits),
               round(base::mean(vec), digits),
               round(stats::median(vec), digits),
               round(base::max(vec), digits),
               paste0("\u00B1", round(stats::sd(vec), digits)),
               sep = "/ ")
  return(ret)
}

# mode
modeFn <- function(vector) {
  # https://stackoverflow.com/questions/2547402/is-there-a-built-in-function-for-finding-the-mode
  ux <- unique(vector)
  return(ux[which.max(tabulate(match(vector, ux)))])
}
