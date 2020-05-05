kruskal_util <- function(dataset,
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
  kruskal <- stats::kruskal.test(
    x = dataset[, get(variable)],
    g = dataset[, get(group_var)]
  )
  chi <- round(unname(kruskal$statistic), digits)
  p <- round(kruskal$p.value, digits)
  if (type == "text") {
    ret <- paste0(
      p, p_marker(p),
      " (X\u00B2=", chi, ")"
    )
  } else if (type == "table") {
    ret <- data.table::data.table(
      "Chi_squared_value" = chi,
      "df" = unname(kruskal$parameter),
      "p_value" = p,
      "significance" = p_marker(p)
    )
  }
  return(ret)
}
