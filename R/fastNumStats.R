#' @title Calculate Overview Statistics For Numerical Variables
#'
#' @param dataset The dataset to analyze. It must be of the class 'data.table'.
#' @param digits An integer. Number of digits to round numeric variables (default: 2).
#'
#' @export

fastNumStats <- function(dataset, digits = 2){

  stopifnot(
    data.table::is.data.table(dataset)
  )

  # subset numeric values
  vec <- colnames(dataset)[dataset[,sapply(.SD, is.numeric), .SDcol=colnames(dataset)]]

  stopifnot(
    length(vec) > 0
  )

  # store dataset temporarily
  dat <- dataset[,vec,with=F]

  retdt <- data.table::data.table(rbind(
    # MAD: mean absolute deviation
    round(dat[,lapply(.SD, stats::mad, na.rm=T)], digits),
    # IQR: Interquartile Range
    round(dat[,lapply(.SD, stats::IQR, na.rm=T)], digits),
    # Skeweness
    round(dat[,lapply(.SD, e1071::skewness, na.rm=T)], digits),
    # Kurtosis
    round(dat[,lapply(.SD, e1071::kurtosis, na.rm=T)], digits),
    # Max-Min (Range)
    round(dat[,lapply(.SD, base::max, na.rm=T)] - dat[,lapply(.SD, base::min, na.rm=T)], digits)
  ))

  retdt <- data.table::data.table(t(retdt), keep.rownames = T)
  colnames(retdt) <- c("Name", "MAD", "IQR", "Skeweness", "Kurtosis", "Max-Min")

  return(retdt)
}
