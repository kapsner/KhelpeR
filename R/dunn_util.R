dunn_util <- function(dataset,
                      variable,
                      group_var,
                      digits = 3,
                      type = "text") {

  sink("/dev/null")
  dunn <- dunn.test::dunn.test(
    x = dataset[, get(variable)],
    g = dataset[, get(group_var)],
    method = "bonferroni",
    table = F,
    kw = F,
    list = T
  )
  sink()

  z <- round(dunn$Z, digits)
  p <- round(dunn$P, digits)
  p_adj <- round(dunn$P.adjusted, digits)
  dunn_results <- data.table::data.table(
    comparisons = dunn$comparisons,
    Z = z,
    P = p,
    "P adj." = paste0(p_adj, sapply(p_adj, p_marker))
  )

  dunn_results[, ("text_results") := paste0(
    get("P adj."),
    " (Z=",
    get("Z"),
    ")"
  )]

  ret <- data.table::as.data.table(
    t(dunn_results[, get("text_results")])
  )
  colnames(ret) <- dunn_results[, get("comparisons")]

  return(ret)
}
