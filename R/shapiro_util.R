shapiro_util <- function(vector, digits = 3, type = "text") {
  stopifnot(
    is.numeric(vector),
    type %in% c("text", "table")
  )
  shap <- stats::shapiro.test(vector)
  w <- unname(round(shap$statistic, digits))
  p <- round(shap$p.value, digits)
  if (type == "text") {
    ret <- paste0(
      "W: ", w, "\n",
      "p: ", p, p_marker(p)
    )
  } else if (type == "table") {
    ret <- data.table::data.table(
      cbind(
        "W_statistic" = w,
        "p_value" = p,
        "significance" = p_marker(p)
      )
    )
  }
  return(ret)
}
