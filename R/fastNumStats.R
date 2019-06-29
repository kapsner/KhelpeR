fastNumStats <- function(dataset){

  # test, if dataset ist data.table
  if (!data.table::is.data.table(dataset)){
    return("data provided must be a data.table object")

  } else {
    # subset numeric values
    vec <- colnames(dataset)[dataset[,sapply(.SD, is.numeric), .SDcol=colnames(dataset)]]

    if (length(vec) <= 0){
      return("your dataset does not contain numeric variables")
    }

    # store dataset temporarily
    dat <- dataset[,vec,with=F]

    retdt <- data.table::data.table(rbind(
      # MAD: mean absolute deviation
      dat[,lapply(.SD, stats::mad, na.rm=T)],
      # IQR: Interquartile Range
      dat[,lapply(.SD, stats::IQR, na.rm=T)],
      # Skeweness
      dat[,lapply(.SD, e1071::skewness, na.rm=T)],
      # Kurtosis
      dat[,lapply(.SD, e1071::kurtosis, na.rm=T)],
      # Max-Min (Range)
      dat[,lapply(.SD, base::max, na.rm=T)] - dat[,lapply(.SD, base::min, na.rm=T)]
    ))

    retdt <- data.table::data.table(t(retdt), keep.rownames = T)
    colnames(retdt) <- c("Variable", "MAD", "IQR", "Skeweness", "Kurtosis", "Max-Min")

    return(retdt)
  }
}
