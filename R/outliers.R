#' @title Flag outliers
#'
#' @description This function flags outliers of a continuous variable
#'
#' @param vector A numeric vector.
#'
#' @return A logical vector, indicating if a value is an outlier (TRUE) or not
#'    (FALSE).
#'
#' @export
#'
flag_outliers <- function(vector) {
  # OutLo: Number of outliers (records below Q25-1.5*IQR)
  # OutHi: Number of outliers (records above Q75+1.5*IQR)

  limits <- get_outlier_limits(vector)

  return(vector < limits$low | vector > limits$high)
}

#' @title Replace outliers
#'
#' @description This function replaces outliers of a continuous variable with
#'    the respective lower or upper bound.
#'
#' @param vector A numeric vector.
#'
#' @export
#'
# replace outliers
cap_outliers <- function(vector) {
  # OutLo: Number of outliers (records below Q25-1.5*IQR)
  # OutHi: Number of outliers (records above Q75+1.5*IQR)

  limits <- get_outlier_limits(vector)

  # cap low outliers
  vector[which(vector < limits$low)] <- limits$low

  # cap high outliers
  vector[which(vector > limits$high)] <- limits$high

  # return vector
  return(vector)
}

# get list of cutoff values for outliers
get_outlier_limits <- function(vector) {

  # OutLo: Number of outliers (records below Q25-1.5*IQR)
  # OutHi: Number of outliers (records above Q75+1.5*IQR)

  # get quantiles of vector
  q <- stats::quantile(
    x = vector,
    probs = c(.25, .75),
    na.rm = T,
    names = F
  )

  # IQR
  iqr <- stats::IQR(
    x = vector,
    na.rm = T
  )

  # OutLo: < Q25-1.5*IQR
  out_low <- q[1] - 1.5 * iqr

  # OutHi: > Q75+1.5*IQR
  out_high <- q[2] + 1.5 * iqr

  return(list(
    low = out_low,
    high = out_high
  ))
}
