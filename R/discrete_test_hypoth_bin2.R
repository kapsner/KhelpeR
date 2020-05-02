#' @title Create group comparisons of discrete variables for a dichotomous
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
discrete_test_hypoth_bin2 <- function(dataset,
                                        group_var,
                                        digits = 2) {

  stopifnot(
    data.table::is.data.table(dataset),
    is.character(group_var),
    is.factor(dataset[, get(group_var)]),
    is.numeric(digits)
  )

  # subset categorical variables
  vec <- subset_cat_cols(dataset)
  vec <- setdiff(vec, group_var)
  stopifnot(length(vec) > 0)

  lvls <- dataset[, levels(get(group_var))]
  stopifnot(length(lvls) == 2)

  # init outtab
  outtab <- data.table::data.table()

  for (variable in vec) {

    xtab <- table(dataset[, get(variable)], dataset[, get(group_var)])

    xstat <- sjstats::xtab_statistics(xtab, B = 10000)

    p_calc <- ifelse(
      isFALSE(xstat$fisher)
      , "Chi squared"
      , "Fisher"
    )

    stats <- paste0(
      round(xstat$p.value, digits),
      p_marker(xstat$p.value),
      " (", p_calc, ") (",
      xstat$method, ": ",
      round(xstat$estimate, digits),
      "; ",
      xstat$stat.name,
      ": ",
      round(unname(xstat$statistic), digits)
    )



    outstats <- data.table::data.table(
      Name = variable,
      Group = rownames(xtab),
      gr_0 = unname(xtab[, 1]),
      gr_1 = unname(xtab[, 2]),
      "p-Value" = stats
    )
    colnames(outstats)[3:4] <- paste0("Group: ", lvls)

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
