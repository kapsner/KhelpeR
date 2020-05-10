anova_util <- function(dataset,
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

  ano_res <- summary(
    anova_test(
      variable = variable,
      group_var = group_var,
      dataset = dataset
    )
  )

  fstat <- round(ano_res[[1]]$`F value`[1], digits)
  p <- round(ano_res[[1]]$`Pr(>F)`[1], digits)

  if (type == "text") {
    ret <- paste0(
      p, p_marker(p),
      " (F=", fstat, ")"
    )
  } else if (type == "table") {
    ret <- data.table::data.table(
      "F_value" = fstat,
      "df" = ano_res[[1]]$Df[1],
      "p_value" = p,
      "significance" = p_marker(p)
    )
  }
  return(ret)
}

anova_test <- function(
  variable,
  group_var,
  dataset
) {
  model <- stats::lm(
    eval(parse(text = variable)) ~ eval(parse(text = group_var)),
    data = dataset
  )
  ano_res <- stats::aov(model)

  return(ano_res)
}
