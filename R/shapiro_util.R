shapiro_util <- function(vector, digits = 3, type = "text") {
  stopifnot(
    is.numeric(vector),
    type %in% c("text", "table")
  )
  shap <- tryCatch({
    ret <- stats::shapiro.test(vector)
    ret
  }, warning = function(w) {
    print(w)
    ret <- NULL
    ret
  }, error = function(e) {
    print(e)
    ret <- NULL
    ret
  }, finally = function(f) {
    return(ret)
  })

  if (!is.null(shap)) {
    w <- unname(round(shap$statistic, digits))
    p <- round(shap$p.value, digits)
    if (type == "text") {
      ret <- paste0(
        p, p_marker(p),
        " (W=", w, ")"
      )
    } else if (type == "table") {
      ret <- data.table::data.table(
        "W_statistic" = w,
        "p_value" = p,
        "significance" = p_marker(p)
      )
    }
  } else {
    if (type == "text") {
      ret <- "Error"
    } else if (type == "table") {
      ret <- data.table::data.table(
        "W_statistic" = "Error",
        "p_value" = "Error",
        "significance" = "Error"
      )
    }
  }
  return(ret)
}
