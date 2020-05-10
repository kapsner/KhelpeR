tukey_util <- function(
  variable,
  group_var,
  dataset,
  digits) {

  anova_result <- anova_test(
    variable = variable,
    group_var = group_var,
    dataset = dataset
  )

  tuk <- stats::TukeyHSD(
    x = anova_result
  )

  comparisons <- rownames(tuk$`eval(parse(text = group_var))`)
  p_adj <- round(
    unname(tuk$`eval(parse(text = group_var))`[, "p adj"]),
    digits
  )
  ci_lwr <- round(
    unname(tuk$`eval(parse(text = group_var))`[, "lwr"]),
    digits
  )

  ci_upr <- round(
    unname(tuk$`eval(parse(text = group_var))`[, "upr"]),
    digits
  )

  ano_results <- data.table::data.table(
    comparisons = comparisons,
    "P adj." = paste0(p_adj, sapply(p_adj, p_marker)),
    "CI" = paste0("[", ci_lwr, "; ", ci_upr, "]")
  )

  ano_results[, ("text_results") := paste0(
    get("P adj."),
    " ",
    get("CI")
  )]

  ret <- data.table::as.data.table(
    t(ano_results[, get("text_results")])
  )
  colnames(ret) <- ano_results[, get("comparisons")]

  return(ret)
}
