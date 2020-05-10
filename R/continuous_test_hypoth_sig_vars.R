#' @title Get significant variables in binary group comparisons
#'
#' @description This function returns a vector of variable names, which
#'    have significant test results of either the T-Test or the Wilcoxon Test.
#'
#' @param significance_level A numeric. The significance threshold of returned
#'    variables (default: 0.05).
#'
#' @inheritParams continuous_stats
#'
#' @export
#'
continuous_test_hypoth_sig_vars <- function(dataset, # nolint
                                            group_var,
                                            significance_level = 0.05) {
  stopifnot(
    data.table::is.data.table(dataset),
    is.character(group_var),
    is.factor(dataset[, get(group_var)]),
    is.numeric(significance_level),
    significance_level > 0 && significance_level <= 0.2
  )

  bin_results <- continuous_test_hypoth_bin(
    dataset = dataset,
    group_var = group_var,
    text_results = FALSE
  )

  significant_vars <- bin_results[
    get("T_p") < significance_level |
      get("Wilcoxon_p") < significance_level, unique(get("Name"))
  ]

  return(significant_vars)
}
