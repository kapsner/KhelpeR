#' @title extensiveCatStats
#'
#' @param dataset A data.table object.
#' @param response_var A character string. The name of the column of interest
#' @param digits A integer.
#'
#' @export
#'

extensiveCatStats <- function(dataset, response_var, digits = 4){

  stopifnot(
    data.table::is.data.table(dataset),
    is.character(response_var),
    is.factor(dataset[,get(response_var)]),
    dataset[,nlevels(get(response_var))] >= 2,
    response_var %in% colnames(dataset)
  )

  # subset numeric values
  vec <- colnames(dataset)[dataset[,sapply(.SD, is.factor), .SDcols=colnames(dataset)]]

  # remove response_var
  vec <- setdiff(vec, response_var)

  # get levels with more than 2 categories
  vec <- vec[dataset[,sapply(.SD, nlevels), .SDcols=vec] >= 2]

  stopifnot(
    length(vec) > 0
  )

  outlist <- list()

  for (variable in vec){
    ta <- data.table::data.table(
      table(dataset[,get(variable)], dataset[,get(response_var)], dnn = c(variable, response_var))
    )
    if (nrow(ta) > 4){
      r <- nrow(ta) - 4
    } else {
      r <- 0
    }
    ta[,("statistics"):=c(getContingencyTabStats(ta, digits, output = "vector"), rep("", r))]
    outlist[[vec]] <- ta
  }
  return(outlist)
}


# get table stats
getContingencyTabStats <- function(table, digits = 2, output = "table"){
  stopifnot(
    is.character(output),
    output %in% c("table", "string", "vector")
  )
  s <- sjstats::xtab_statistics(table)

  if (output == "table"){
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
  } else if (output == "string"){
    outdat <- paste0(s$stat.name, ": ", round(s$statistic, digits), "\n",
                     "df: ", s$df, "\n",
                     s$method, ": ", round(s$estimate, digits), "\n",
                     "p-value: ", paste0(round(s$p.value, digits), pMarker(s$p.value)), ifelse(s$fisher,
                                                                                               " (Fisher's Exact Test)",
                                                                                               ""))
  } else if (output == "vector"){
    outdat <- c(paste0(s$stat.name, ": ", round(s$statistic, digits)),
                paste0("df: ", s$df),
                paste0(s$method, ": ", round(s$estimate, digits)),
                paste0("p-value: ", paste0(round(s$p.value, digits), pMarker(s$p.value)), ifelse(s$fisher,
                                                                                               " (Fisher's Exact Test)",
                                                                                               ""))
    )
  }
  return(outdat)
}
