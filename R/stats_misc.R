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
               paste0("±", round(stats::sd(vec), digits)),
               sep = "/ ")
  return(ret)
}
