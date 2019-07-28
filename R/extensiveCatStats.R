extensiveCatStats <- function(dataset, response_var){

  stopifnot(
    data.table::is.data.table(dataset),
    is.character(response_var)
  )

  # subset numeric values
  vec <- colnames(dataset)[dataset[,sapply(.SD, is.factor), .SDcol=colnames(dataset)]]

  vec <- setdiff(vec, response_var)

  stopifnot(
    length(vec) > 0
  )

  outlist <- list()

  for (variable in vec){
    ta <- table(dataset[,get(variable)], dataset[,get(response_var)], dnn = c(variable, response_var))
    print(ta)
    print(getContingencyTabStats(ta))
  }
}


# get table stats
getContingencyTabStats <- function(table, digits = 2){
  s <- sjstats::xtab_statistics(table)

  outdat <- data.table::as.data.table(rbind(
    c("Statistic test", s$stat.name),
    c("Statistic value", round(s$statistic, digits)),
    c("df", s$df),
    c("Method name", s$method),
    c("Method value", round(s$estimate, digits)),
    c("Fisher's Exact Test", as.character(s$fisher)),
    c("p-value", paste0(round(s$p.value, digits), pMarker(s$p.value)))
  ))
  colnames(outdat) <- c("", "value")
  return(outdat)
}
