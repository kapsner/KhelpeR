#' @title Create group comparisons of continuous variables for a dichotomous
#'   grouping variable.
#'
#' @description This function creates a table that contains the output of
#'   Shapiro Wilk's normality test, Levene's Homoscedasticity test, T-test and
#'   Wilcoxon test for continuous variables.
#'
#' @inheritParams continuous_stats
#'
#' @export
#'
binary_results <- function(dataset, group_var) {
  stopifnot(
    data.table::is.data.table(dataset),
    is.character(group_var),
    is.factor(dataset[, get(group_var)])
  )

  # subset numeric variables
  vec <- subset_num_cols(dataset)
  stopifnot(length(vec) > 0)

  # init table
  outtab <- results_table_bin()

  for (variable in vec) {
    outtab <- data.table::rbindlist(
      list(
        outtab,
        results_table_bin(
          dataset = dataset,
          variable = variable,
          group_var = group_var
        )
      )
    )
  }
  return(outtab)
}

results_table_bin <- function(dataset = NULL,
                              variable = NULL,
                              group_var = NULL) {

  stopifnot(
    ifelse(is.null(variable), TRUE, is.character(variable)),
    ifelse(is.null(group_var), TRUE, is.character(group_var)),
    ifelse(is.null(dataset), TRUE, data.table::is.data.table(dataset) &&
             is.numeric(dataset[, get(variable)]) &&
             is.factor(dataset[, get(group_var)]))
  )

  if (!is.null(dataset) && !is.null(variable) && !is.null(group_var)) {
    lvls <- dataset[, levels(get(group_var))]
    stopifnot(length(lvls) == 2)

    outlist <- list(
      "Name" = variable,
      "Group" = lvls,
      "N" = c(
        dataset[!is.na(get(group_var)) &
                  get(group_var) == lvls[1], .N],
        dataset[!is.na(get(group_var)) &
                  get(group_var) == lvls[2], .N]
      ),
      "Normality" = c(
        shapiro_util(dataset[!is.na(get(group_var)) &
                               get(group_var) == lvls[1], get(variable)]),
        shapiro_util(dataset[!is.na(get(group_var)) &
                               get(group_var) == lvls[2], get(variable)])
      ),
      "Homoscedasticity" = levene_util(
        dataset = dataset,
        variable = variable,
        group_var = group_var
      ),
      "T-Test" = ttest_util(
        dataset = dataset,
        variable = variable,
        group_var = group_var
      ),
      "Wilcoxon-Test" = wilcoxon_util(
        dataset = dataset,
        variable = variable,
        group_var = group_var
      )
    )
  } else {
    outlist <- data.table::data.table(
      cbind(
        "Name" = character(),
        "Group" = character(),
        "N" = numeric(),
        "Normality" = character(),
        "Homoscedasticity" = character(),
        "T-Test" = character(),
        "Wilcoxon-Test" = character()
      )
    )
  }
  return(outlist)
}
