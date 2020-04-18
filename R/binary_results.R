#' @title Create group comparisons of continuous variables for a dichotomous
#'   grouping variable.
#'
#' @description This function creates a table that contains the output of
#'   Shapiro Wilk's normality test, Levene's Homoscedasticity test, T-test and
#'   Wilcoxon test for continuous variables.
#'
#' @param text_results A logical. If TRUE (default), results are returned text
#'    based. Set this argument to FALSE to get each test statistic in an extra
#'    column.
#'
#' @inheritParams continuous_stats
#'
#' @export
#'
binary_results <- function(dataset,
                           group_var,
                           digits = 3,
                           text_results = TRUE) {
  stopifnot(
    data.table::is.data.table(dataset),
    is.character(group_var),
    is.factor(dataset[, get(group_var)]),
    is.logical(text_results),
    is.numeric(digits)
  )

  # subset numeric variables
  vec <- subset_num_cols(dataset)
  stopifnot(length(vec) > 0)

  # init table
  outtab <- results_table_bin(text_results = text_results)

  for (variable in vec) {
    outtab <- data.table::rbindlist(
      list(
        outtab,
        results_table_bin(
          dataset = dataset,
          variable = variable,
          group_var = group_var,
          digits = digits,
          text_results = text_results
        )
      )
    )
  }
  return(outtab)
}

results_table_bin <- function(dataset = NULL,
                              variable = NULL,
                              group_var = NULL,
                              digits = NULL,
                              text_results = TRUE) {

  stopifnot(
    ifelse(is.null(variable), TRUE, is.character(variable)),
    ifelse(is.null(group_var), TRUE, is.character(group_var)),
    ifelse(is.null(dataset), TRUE, data.table::is.data.table(dataset) &&
             is.numeric(dataset[, get(variable)]) &&
             is.factor(dataset[, get(group_var)])),
    is.logical(text_results),
    ifelse(is.null(digits), TRUE, is.numeric(digits))
  )

  if (!is.null(dataset) && !is.null(variable) && !is.null(group_var)) {
    lvls <- dataset[, levels(get(group_var))]
    stopifnot(length(lvls) == 2)

    if (isTRUE(text_results)) {
      outlist <- list(
        "Name" = variable,
        "Group" = lvls,
        "N" = sapply(
          X = lvls,
          FUN = function(x) {
            dataset[!is.na(get(variable)), ][get(group_var) == x, .N]
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
        ),
        "T-Test" = ttest_util(
          dataset = dataset,
          variable = variable,
          group_var = group_var,
          digits = digits
        ),
        "Wilcoxon-Test" = wilcoxon_util(
          dataset = dataset,
          variable = variable,
          group_var = group_var,
          digits = digits
        )
      )
    } else if (isFALSE(text_results)) {

      shap_0 <- shapiro_util(
        vector = dataset[!is.na(get(variable)),
        ][
          get(group_var) == lvls[1], get(variable)
        ],
        digits = digits,
        type = "table"
      )
      shap_1 <- shapiro_util(
        vector = dataset[!is.na(get(variable)),
        ][
          get(group_var) == lvls[2], get(variable)
        ],
        digits = digits,
        type = "table"
      )

      levene <- levene_util(
        dataset = dataset,
        variable = variable,
        group_var = group_var,
        digits = digits,
        type = "table"
      )

      ttest <- ttest_util(
        dataset = dataset,
        variable = variable,
        group_var = group_var,
        digits = digits,
        type = "table"
      )

      wilcoxon <- wilcoxon_util(
        dataset = dataset,
        variable = variable,
        group_var = group_var,
        digits = digits,
        type = "table"
      )

      outlist <- list(
        "Name" = variable,
        "Group" = lvls,
        "N" = c(
          dataset[!is.na(get(variable)), ][get(group_var) == lvls[1], .N],
          dataset[!is.na(get(variable)), ][get(group_var) == lvls[2], .N]
        ),
        "Shapiro_W" = c(shap_0$W_statistic, shap_1$W_statistic),
        "Shapiro_p" = c(shap_0$p_value, shap_1$p_value),
        "Shapiro_sign." = c(shap_0$significance, shap_1$significance),
        "Levene_F" = levene$F_value,
        "Levene_p" = levene$p_value,
        "Levene_sign." = levene$significance,
        "T_T" = ttest$T_statistic,
        "T_CI" = ttest$`95_CI`,
        "T_df" = ttest$df,
        "T_m1" = ttest$`mean in group neg`,
        "T_m2" = ttest$`mean in group pos`,
        "T_p" = ttest$p_value,
        "T_sign." = ttest$significance,
        "Wilcoxon_W" = wilcoxon$W_statistic,
        "Wilcoxon_CI" = wilcoxon$`95_CI`,
        "Wilcoxon_difference" = wilcoxon$`difference in location`,
        "Wilcoxon_p" = wilcoxon$p_value,
        "Wilcoxon_sign." = wilcoxon$significance
      )
    }
  } else {
    if (isTRUE(text_results)) {
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
    } else if (isFALSE(text_results)) {
      outlist <- data.table::data.table(
        cbind(
          "Name" = character(),
          "Group" = character(),
          "N" = numeric(),
          "Shapiro_W" = numeric(),
          "Shapiro_p" = numeric(),
          "Shapiro_sign." = character(),
          "Levene_F" = numeric(),
          "Levene_p" = numeric(),
          "Levene_sign." = character(),
          "T_T" = numeric(),
          "T_CI" = character(),
          "T_df" = numeric(),
          "T_m1" = numeric(),
          "T_m2" = numeric(),
          "T_p" = numeric(),
          "T_sign." = character(),
          "Wilcoxon_W" = numeric(),
          "Wilcoxon_CI" = character(),
          "Wilcoxon_difference" = numeric(),
          "Wilcoxon_p" = numeric(),
          "Wilcoxon_sign." = character()
        )
      )
    }
  }
  return(outlist)
}
