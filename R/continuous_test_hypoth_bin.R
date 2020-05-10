#' @title Create group comparisons of continuous variables for a dichotomous
#'   grouping variable.
#'
#' @description This function creates a table that contains the output of
#'   Shapiro Wilk's normality test, Levene's Homoscedasticity test, T-test or
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
continuous_test_hypoth_bin <- function(dataset,
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
  outtab <- data.table::data.table()

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
      ),
      fill = T
    )
  }
  return(outtab)
}

results_table_bin <- function(dataset,
                              variable,
                              group_var,
                              digits,
                              text_results = TRUE) {

  stopifnot(
    is.character(variable),
    is.character(group_var),
    data.table::is.data.table(dataset) &&
      is.numeric(dataset[, get(variable)]) &&
      is.factor(dataset[, get(group_var)]),
    is.logical(text_results),
    is.numeric(digits)
  )

  if (!is.null(dataset) && !is.null(variable) && !is.null(group_var)) {
    lvls <- dataset[, levels(get(group_var))]
    stopifnot(length(lvls) == 2)

    # get test results
    which_test <- continuous_hypoth_which_test_bin(
      dataset = dataset,
      variable = variable,
      group_var = group_var,
      equal_variances = FALSE
    )

    if (which_test == "t.test") {
      p_value <- ttest_util(
        dataset = dataset,
        variable = variable,
        group_var = group_var
      )
    } else if (which_test == "wilcox.test") {
      p_value <- wilcoxon_util(
        dataset = dataset,
        variable = variable,
        group_var = group_var
      )
    }

    if (isTRUE(text_results)) {
      outlist <- list(
        "Name" = variable,
        "Group" = lvls,
        "N" = get_grouped_N(lvls, dataset, variable, group_var),
        "Dispersion min/mean/med/max (sd)" = get_grouped_dispersion(
          lvls, dataset, variable, group_var, digits
        ),
        "Normality" = get_grouped_normality(
          lvls, dataset, variable, group_var, digits
        ),
        "Homoscedasticity" = levene_util(
          dataset = dataset,
          variable = variable,
          group_var = group_var,
          digits = digits
        ),
        "p-Value" = p_value
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
        "T_m1" = ttest[, get(colnames(ttest)[4])],
        "T_m2" = ttest[, get(colnames(ttest)[5])],
        "T_p" = ttest$p_value,
        "T_sign." = ttest$significance,
        "Wilcoxon_W" = wilcoxon$W_statistic,
        "Wilcoxon_CI" = wilcoxon$`95_CI`,
        "Wilcoxon_difference" = wilcoxon$`difference in location`,
        "Wilcoxon_p" = wilcoxon$p_value,
        "Wilcoxon_sign." = wilcoxon$significance
      )
      names(outlist)[names(outlist) == "T_m1"] <- paste0(
        "T_", colnames(ttest)[4]
      )
      names(outlist)[names(outlist) == "T_m2"] <- paste0(
        "T_", colnames(ttest)[5]
      )
    }
  } else {
    outlist <- "Error"
  }
  return(outlist)
}
