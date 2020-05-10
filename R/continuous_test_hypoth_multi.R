#' @title Create group comparisons of continuous variables for a multi group
#'   (>2) grouping variable.
#'
#' @description This function creates a table that contains the output of
#'   Shapiro Wilk's normality test, Levene's Homoscedasticity test, Kruskal
#'   Wallis test and Dunn's post hoc test.
#'
#' @param text_results A logical. If TRUE (default), results are returned text
#'    based. Set this argument to FALSE to get each test statistic in an extra
#'    column.
#' @param type A character. Either "non-par" (performing Kruskal Wallis
#'    test and Dunn's post-hoc test) or "par" (performing ANOVA and Tukey's
#'    post-hoc test).
#'
#' @inheritParams continuous_stats
#'
#' @export
#'
continuous_test_hypoth_multi <- function(dataset,
                                         group_var,
                                         type,
                                         digits = 3) {
  stopifnot(
    data.table::is.data.table(dataset),
    is.character(group_var),
    is.factor(dataset[, get(group_var)]),
    is.numeric(digits),
    is.character(type) && type %in% c("non-par", "par")
  )

  # subset numeric variables
  vec <- subset_num_cols(dataset)
  stopifnot(length(vec) > 0)

  # init table
  outtab <- data.table::data.table()

  for (variable in vec) {

    if (type == "non-par") {
      test_results <- results_table_multi_non_par(
        dataset = dataset,
        variable = variable,
        group_var = group_var,
        digits = digits
      )
    } else if (type == "par") {
      test_results <- results_table_multi_par(
        dataset = dataset,
        variable = variable,
        group_var = group_var,
        digits = digits
      )
    }

    outtab <- data.table::rbindlist(
      list(
        outtab,
        test_results
      ),
      fill = T
    )
  }
  return(outtab)
}

results_table_multi_non_par <- function(dataset,
                                variable,
                                group_var,
                                digits) {

  stopifnot(
    is.character(variable),
    is.character(group_var),
    data.table::is.data.table(dataset) &&
      is.numeric(dataset[, get(variable)]) &&
      is.factor(dataset[, get(group_var)]),
    is.numeric(digits)
  )

  if (!is.null(dataset) && !is.null(variable) && !is.null(group_var)) {
    lvls <- dataset[, levels(get(group_var))]
    stopifnot(length(lvls) > 2)

    outlist <- list(
      "Name" = variable,
      "Homoscedasticity" = levene_util(
        dataset = dataset,
        variable = variable,
        group_var = group_var,
        digits = digits
      ),
      "Kruskal-Test" = kruskal_util(
        dataset = dataset,
        variable = variable,
        group_var = group_var,
        digits = digits
      )
    )
    outlist <- c(
      outlist,
      dunn_util(
        dataset = dataset,
        variable = variable,
        group_var = group_var,
        digits = digits
      )
    )
  } else {
    outlist <- "Error"
  }
  return(outlist)
}


results_table_multi_par <- function(dataset,
                                        variable,
                                        group_var,
                                        digits) {

  stopifnot(
    is.character(variable),
    is.character(group_var),
    data.table::is.data.table(dataset) &&
      is.numeric(dataset[, get(variable)]) &&
      is.factor(dataset[, get(group_var)]),
    is.numeric(digits)
  )

  if (!is.null(dataset) && !is.null(variable) && !is.null(group_var)) {
    lvls <- dataset[, levels(get(group_var))]
    stopifnot(length(lvls) > 2)

    outlist <- list(
      "Name" = variable,
      "Homoscedasticity" = levene_util(
        dataset = dataset,
        variable = variable,
        group_var = group_var,
        digits = digits
      ),
      "ANOVA" = anova_util(
        dataset = dataset,
        variable = variable,
        group_var = group_var,
        digits = digits
      )
    )
    outlist <- c(
      outlist,
      tukey_util(
        dataset = dataset,
        variable = variable,
        group_var = group_var,
        digits = digits
      )
    )
  } else {
    outlist <- "Error"
  }
  return(outlist)
}
