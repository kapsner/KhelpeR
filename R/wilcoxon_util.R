wilcoxon_util <- function(dataset,
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
  wilcox <- stats::wilcox.test(
    stats::as.formula(
      eval(parse(text = paste0(
        variable, "~", group_var
      )))
    ),
    data = dataset,
    alternative = "two.sided",
    conf.int = T
  )
  w <- unname(round(wilcox$statistic, digits))
  p <- round(wilcox$p.value, digits)
  ci <- paste(round(wilcox$conf.int, digits), collapse = ";")
  d <- round(wilcox$estimate, digits)
  if (type == "text") {
    ret <- paste0(
      p, p_marker(p),
      " (W=", w, ")"
    )
  } else if (type == "table") {
    ret <- data.table::data.table(
        "W_statistic" = w,
        "95_CI" = ci,
        "difference in location" = d,
        "p_value" = p,
        "significance" = p_marker(p)
    )
  }
  return(ret)
}
