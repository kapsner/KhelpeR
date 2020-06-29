#' @title Helper function to create Q-Q-Plots and Histograms
#'
#' @inheritParams continuous_stats_grouped
#'
#' @export
#'
plot_normality <- function(dataset,
                           variable,
                           group_var = NULL
) {

  stopifnot(
    data.table::is.data.table(dataset),
    is.character(variable),
    is.numeric(dataset[, get(variable)]),
    ifelse(is.null(group_var), TRUE,
           is.character(group_var) && is.factor(dataset[, get(group_var)])
    )
  )

  title = paste0("Q-Q-Plot / Histogram of variable: ", variable)

  if (!is.null(group_var)) {
    lvls <- dataset[, levels(get(group_var))]

    plt_list <- sapply(
      X = lvls,
      FUN = function(i) {
        p <- plot_qq_hist(
          dataset = dataset[get(group_var) == i, ],
          variable = variable,
          title = paste0("Group: ", i)
        )
        return(p)
      },
      simplify = FALSE,
      USE.NAMES = TRUE
    )

    gr <- gridExtra::arrangeGrob(
      grobs = plt_list,
      nrow = length(plt_list),
      ncol = 1,
      top = title
    )
  } else {
    gr <- plot_qq_hist(
      dataset = dataset,
      variable = variable,
      title = title
    )
  }
  return(gr)
}


plot_qq_hist <- function(dataset, variable, title = NULL) {

  # Q-Q-plot
  qq <- ggpubr::ggqqplot(
    data = dataset,
    x = variable,
    ggtheme = ggpubr::theme_pubr()
  )

  # Histogram
  hi <- ggpubr::gghistogram(
    data = dataset,
    x = variable,
    ggtheme = ggpubr::theme_pubr()
  )

  p <- gridExtra::arrangeGrob(
    qq,
    hi,
    top = title,
    nrow = 1,
    ncol = 2
  )

  return(p)
}
