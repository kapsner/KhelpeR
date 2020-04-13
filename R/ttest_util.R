ttest_util <- function(dataset, variable, group_var, digits = 3, type = "text") {
  stopifnot(
    type %in% c("text", "table"),
    data.table::is.data.table(dataset),
    is.numeric(dataset[, get(variable)]),
    is.factor(dataset[, get(group_var)])
  )
  ttest <- t.test(
    as.formula(
      eval(parse(text = paste0(
        variable, "~", group_var
      )))
    ),
    data = dataset,
    alternative = "two.sided",
    mu = 0
  )
  tstat <- unname(round(ttest$statistic, digits))
  df <- unname(round(ttest$parameter, digits))
  p <- round(ttest$p.value, digits)
  ci <- paste(round(ttest$conf.int, digits), collapse = ";")
  m <- round(ttest$estimate, digits)
  if (type == "text") {
    ret <- paste0(
      "t: ", tstat, "\n",
      "p: ", p, p_marker(p)
    )
  } else if (type == "table") {
    ret <- data.table::data.table(
      cbind(
        "T_statistic" = tstat,
        "95_CI" = ci,
        "df" = df,
        "m1" = m[1],
        "m2" = m[2],
        "p_value" = p,
        "significance" = p_marker(p)
      )
    )
    colnames(ret)[4:5] <- c(names(m[1]), names(m[2]))
  }
  return(ret)
}
