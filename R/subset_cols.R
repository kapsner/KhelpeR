subset_num_cols <- function(dataset) {
  ret <- colnames(dataset)[dataset[
    , sapply(.SD, is.numeric), .SDcols = colnames(dataset)]
  ]
  return(ret)
}

subset_cat_cols <- function(dataset) {
  ret <- colnames(dataset)[dataset[
    , sapply(.SD, is.factor), .SDcols = colnames(dataset)]
  ]
  return(ret)
}
