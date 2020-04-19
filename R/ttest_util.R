ttest_util <- function(dataset,
                       variable,
                       group_var,
                       digits = 3,
                       type = "text") {
  stopifnot(
    type %in% c("text", "table"),
    data.table::is.data.table(dataset),
    is.numeric(dataset[, get(variable)]),
    is.factor(dataset[, get(group_var)])
  )

  ttest <- tryCatch({
    ret <- stats::t.test(
      stats::as.formula(
        eval(parse(text = paste0(
          variable, "~", group_var
        )))
      ),
      data = dataset,
      alternative = "two.sided",
      mu = 0
    )
    ret
  }, warning = function(w) {
    message(w)
    ret <- NULL
    ret
  }, error = function(e) {
    message(e)
    ret <- NULL
    ret
  }, finally = function(f) {
    return(ret)
  })


  if (!is.null(ttest)) {
    tstat <- unname(round(ttest$statistic, digits))
    df <- unname(round(ttest$parameter, digits))
    p <- round(ttest$p.value, digits)
    ci <- paste(round(ttest$conf.int, digits), collapse = ";")
    m <- round(ttest$estimate, digits)
    if (type == "text") {
      ret <- paste0(
        p, p_marker(p),
        " (t=", tstat, ")"
      )
    } else if (type == "table") {
      ret <- data.table::data.table(
        "T_statistic" = tstat,
        "95_CI" = ci,
        "df" = df,
        "m1" = m[1],
        "m2" = m[2],
        "p_value" = p,
        "significance" = p_marker(p)
      )
      colnames(ret)[4:5] <- c(
        gsub(" ", "_", names(m[1])),
        gsub(" ", "_", names(m[2]))
      )
    }
  } else {

    if (type == "text") {
      ret <- "Error"
    } else if (type == "table") {
      ret <- data.table::data.table(
        "T_statistic" = "Error",
        "95_CI" = "Error",
        "df" = "Error",
        "m1" = "Error",
        "m2" = "Error",
        "p_value" = "Error",
        "significance" = "Error"
      )
    }
  }
  return(ret)
}
