#' @title discrete_stats
#'
#' @param response_var A character. The column name of the discrete
#'   variable of interest.
#'
#' @inheritParams continuous_stats
#'
#' @export
#'
discrete_stats <- function(dataset, response_var, digits = 4) {

  stopifnot(
    data.table::is.data.table(dataset),
    is.character(response_var),
    is.factor(dataset[, get(response_var)]),
    dataset[, nlevels(get(response_var))] >= 2,
    response_var %in% colnames(dataset)
  )

  # subset factor values
  vec <- subset_cat_cols(dataset)

  # remove response_var
  vec <- setdiff(vec, response_var)

  # get levels with more than 2 categories
  vec <- vec[dataset[, sapply(.SD, nlevels), .SDcols = vec] >= 2]
  stopifnot(length(vec) > 0)

  table1 <- data.table::data.table()

  # iterate over numeric vars
  for (var in vec) {
    # get N
    n <- as.numeric(dataset[!is.na(get(var)), .N])

    table1 <- data.table::rbindlist(
      l = list(
        table1,
        data.table::data.table(
          Name = var,
          Group = "overall",
          N = n,
          "% Valid" = 100,
          "NA" = as.numeric(dataset[is.na(get(var)), .N]),
          Levels = as.numeric(dataset[, nlevels(get(var))])
        )
      ),
      fill = TRUE
    )

    for (lv in dataset[, levels(get(response_var))]) {
      table1 <- data.table::rbindlist(
        l = list(
          table1,
          data.table::data.table(
            Name = var,
            Group = lv,
            N = dataset[get(response_var) == lv, .N],
            "NA" = "",
            "% Valid" = round(
              dataset[get(response_var) == lv, .N] / n * 100,
              digits
            ),
            Levels = dataset[get(response_var) == lv, length(
              unique(get(var))
            )]
          )
        ),
        fill = TRUE
      )
    }
  }
  return(table1)
}


# get table stats
get_contingency_tab_stats <- function(table, digits = 2, output = "table") {
  stopifnot(
    is.character(output),
    output %in% c("table", "string", "vector")
  )
  s <- sjstats::xtab_statistics(table)

  if (output == "table") {
    outdat <- data.table::as.data.table(rbind(
      c("Statistic test", s$stat.name),
      c("Statistic value", round(s$statistic, digits)),
      c("df", s$df),
      c("Method name", s$method),
      c("Method value", round(s$estimate, digits)),
      c("Fisher's Exact Test", as.character(s$fisher)),
      c("p-value", paste0(round(s$p.value, digits), p_marker(s$p.value)))
    ))
    colnames(outdat) <- c("", "value")
  } else if (output == "string") {
    outdat <- paste0(s$stat.name, ": ", round(s$statistic, digits), "\n",
                     "df: ", s$df, "\n",
                     s$method, ": ", round(s$estimate, digits), "\n",
                     "p-value: ", paste0(
                       round(s$p.value, digits),
                       p_marker(s$p.value)),
                     ifelse(s$fisher,
                            " (Fisher's Exact Test)",
                            ""))
  } else if (output == "vector") {
    outdat <- c(paste0(s$stat.name, ": ", round(s$statistic, digits)),
                paste0("df: ", s$df),
                paste0(s$method, ": ", round(s$estimate, digits)),
                paste0("p-value: ", paste0(
                  round(s$p.value, digits),
                  p_marker(s$p.value)),
                  ifelse(s$fisher,
                         " (Fisher's Exact Test)",
                         ""))
    )
  }
  return(outdat)
}
