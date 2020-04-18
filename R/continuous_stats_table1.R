#' @title Create statistics overview table for continuous variables
#'
#' @description This function creates a vast amount of statistics
#'
#' @inheritParams continuous_stats
#'
#' @export
#'
continuous_stats_table1 <- function(dataset, group_var = NULL, digits = 2) {

  stopifnot(
    data.table::is.data.table(dataset),
    ifelse(is.null(group_var), TRUE, is.character(group_var)),
    is.numeric(digits)
  )

  # subset numeric variables
  vec <- subset_num_cols(dataset)
  stopifnot(length(vec) > 0)

  # init outtab
  grouped <- ifelse(is.null(group_var), FALSE, TRUE)
  outtab <- continuous_table1(grouped = grouped)

  for (variable in vec) {
    outtab <- data.table::rbindlist(
      list(
        outtab,
        continuous_table1(
          dataset = dataset,
          variable = variable,
          group_var = group_var,
          digits = digits,
          grouped = grouped
        )
      )
    )
  }
  return(outtab)
}


continuous_table1 <- function(dataset = NULL,
                              variable = NULL,
                              group_var = NULL,
                              digits = NULL,
                              grouped) {

  stopifnot(
    ifelse(is.null(variable), TRUE, is.character(variable)),
    ifelse(is.null(group_var), TRUE, is.character(group_var) &&
             is.factor(dataset[, get(group_var)])),
    ifelse(is.null(dataset), TRUE, data.table::is.data.table(dataset) &&
             is.numeric(dataset[, get(variable)])),
    is.logical(grouped),
    ifelse(is.null(digits), TRUE, is.numeric(digits))
  )

  if (!is.null(dataset) && !is.null(variable)) {
      table1 <- data.table::data.table(
        Name = variable,
        "N" = dataset[!is.na(get(variable)), .N],
        "min/mean/med/max (sd)" = distribution_2(
          vector = dataset[!is.na(get(variable)), get(variable)],
          digits = digits
        ),
        Normality = shapiro_util(
          vector = dataset[!is.na(get(variable)), get(variable)],
          digits = digits
        )
      )

      if (!is.null(group_var)) {
      lvls <- dataset[, levels(get(group_var))]

      table1[, `:=` (
        Group = "all",
        Homoscedasticity = ""
      )]

      table1 <- data.table::rbindlist(
        l = list(
          table1,
          data.table::data.table(
            "Name" = variable,
            "Group" = lvls,
            "N" = sapply(
              X = lvls,
              FUN = function(x) {
                dataset[!is.na(get(variable)), ][get(group_var) == x, .N]
              },
              USE.NAMES = F
            ),
            "min/mean/med/max (sd)" = sapply(
              X = lvls,
              FUN = function(x) {
                distribution_2(
                  vector = dataset[!is.na(get(variable)),
                  ][
                    get(group_var) == x, get(variable)
                  ],
                  digits = digits
                )
              },
              USE.NAMES = F
            ),
            "Normality" = sapply(
              X = lvls,
              FUN = function(x) {
                shapiro_util(
                  vector = dataset[!is.na(get(variable)),
                  ][
                    get(group_var) == x, get(variable)
                  ],
                  digits = digits
                )
              },
              USE.NAMES = F
            ),
            "Homoscedasticity" = levene_util(
              dataset = dataset,
              variable = variable,
              group_var = group_var,
              digits = digits
            )
          )
        ),
        fill = T
      )

      data.table::setcolorder(
        x = table1,
        neworder = colnames(continuous_table1(grouped = T))
      )
    }

  } else {
    if (isFALSE(grouped)) {
      table1 <- data.table::data.table(
        "Name" = character(),
        "N" = integer(),
        "min/mean/med/max (sd)" = character(),
        "Normality" = character()
      )
    } else if (isTRUE(grouped)) {
      table1 <- data.table::data.table(
        "Name" = character(),
        "Group" = character(),
        "N" = integer(),
        "min/mean/med/max (sd)" = character(),
        "Normality" = character(),
        "Homoscedasticity" = character()
      )
    }
  }
  return(table1)
}
