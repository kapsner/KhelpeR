# calculate standard error
# standard error (https://rcompanion.org/rcompanion/c_03.html)
se <- function(vector){
  return(sd(vector, na.rm = T) / sqrt(length(vector[!is.na(vector)])))
}

distribution <- function(dataset, variable){
  vec <- round(dataset[!is.na(get(variable)),get(variable)], 2)
  ret <- paste(base::min(vec),
               round(base::mean(vec), 2),
               stats::median(vec),
               base::max(vec),
               paste0("±", round(stats::sd(vec), 2)),
               sep = "/ ")
  return(ret)
}
