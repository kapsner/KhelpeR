levene_util <- function(dataset,
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
  levene <- car::leveneTest(
    y = dataset[, get(variable)],
    group = dataset[, get(group_var)],
    center = stats::median
  )
  f <- round(levene$`F value`[1], digits)
  p <- round(levene$`Pr(>F)`[1], digits)
  if (type == "text") {
    ret <- paste0(
      p, p_marker(p),
      " (F=", f, ")"
    )
  } else if (type == "table") {
    ret <- data.table::data.table(
      "F_value" = f,
      "p_value" = p,
      "significance" = p_marker(p)
    )
  }
  return(ret)
}
