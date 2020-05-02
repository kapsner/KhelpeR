#' @title Create group comparisons of continuous variables for a dichotomous
#'   grouping variable 2.
#'
#' @description This function creates a table that contains basic statistics
#'    and p-values of the groups to compare
#'
#' @param stat A character. Either "mean" to report the mean and standard
#'    deviation, or "median" to report the median and IQR.
#'
#' @inheritParams continuous_test_hypoth_bin
#'
#' @export
#'
continuous_test_hypoth_bin2 <- function(dataset,
                                        group_var,
                                        stat = "mean",
                                        digits = 2) {

  stopifnot(
    data.table::is.data.table(dataset),
    is.character(group_var),
    is.factor(dataset[, get(group_var)]),
    is.numeric(digits),
    is.character(stat) && stat %in% c("mean", "median")
  )

  # subset numeric variables
  vec <- subset_num_cols(dataset)
  stopifnot(length(vec) > 0)

  cont_stats <- continuous_stats_table1(
    dataset = dataset[, c(group_var, vec), with = FALSE],
    group_var = group_var
  )

  bin_res <- continuous_test_hypoth_bin(
    dataset = dataset[, c(group_var, vec), with = FALSE],
    group_var = group_var,
    text_results = FALSE
  )

  lvls <- dataset[, levels(get(group_var))]
  stopifnot(length(lvls) == 2)

  # init outtab
  outtab <- data.table::data.table()

  for (variable in unique(cont_stats$Name)) {
    gr_0 <- dataset[get(group_var) == lvls[1], get(variable)]
    gr_1 <- dataset[get(group_var) == lvls[2], get(variable)]
    n_0 <- dataset[get(group_var) == lvls[1] & !is.na(get(variable)), .N]
    n_1 <- dataset[get(group_var) == lvls[2] & !is.na(get(variable)), .N]

    if (stat == "mean") {
      stat_0 <- paste0(
        round(mean(gr_0, na.rm = TRUE), 2),
        " (\u00B1", round(stats::sd(gr_0, na.rm = TRUE), 2), ")"
      )
      stat_1 <- paste0(
        round(mean(gr_1, na.rm = TRUE), 2),
        " (\u00B1", round(stats::sd(gr_1, na.rm = TRUE), 2), ")"
      )
    } else if (stat == "median") {
      stat_0 <- paste0(
        round(stats::median(gr_0, na.rm = TRUE), 2),
        " (", round(stats::IQR(gr_0, na.rm = TRUE), 2), ")"
      )
      stat_1 <- paste0(
        round(stats::median(gr_1, na.rm = TRUE), 2),
        " (", round(stats::IQR(gr_1, na.rm = TRUE), 2), ")"
      )
    }

    which_test <- continuous_hypoth_which_test_bin(
      dataset,
      variable,
      group_var
    )

    outstats <- data.table::data.table(
      Name = variable,
      gr_0 = paste0(
        stat_0,
        " [n=", n_0, "]"
      ),
      gr_1 = paste0(
        stat_1,
        " [n=", n_1, "]"
      ),
      "p-Value" = ifelse(
        which_test == "wilcox.test",
        # return wilcoxon
        paste0(bin_res[get("Name") == variable,
                       unique(get("Wilcoxon_p"))],
               bin_res[get("Name") == variable,
                       unique(get("Wilcoxon_sign."))],
               " (Wilcoxon)"),
        # return ttest
        paste0(bin_res[get("Name") == variable,
                       unique(get("T_p"))],
               bin_res[get("Name") == variable,
                       unique(get("T_sign."))],
               " (T-Test)")
      )
    )
    colnames(outstats)[2:3] <- paste0("Group: ", lvls)

    outtab <- data.table::rbindlist(
      l = list(
        outtab,
        outstats
      ),
      fill = TRUE
    )
  }
  return(outtab)
}

continuous_hypoth_which_test_bin <- function(dataset, variable, group_var) {

  bin_res <- continuous_test_hypoth_bin(
    dataset = dataset[, c(group_var, variable), with = FALSE],
    group_var = group_var,
    text_results = FALSE
  )

  normality_assumption_violated <- any(
    bin_res[, grepl("\\*", get("Shapiro_sign."))]
  )

  if (isTRUE(normality_assumption_violated)) {
    ret <- "wilcox.test"
  } else if (isFALSE(normality_assumption_violated)) {
    ret <- "t.test"
  }

  return(ret)
}
