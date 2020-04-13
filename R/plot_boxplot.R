#' @title Helper function to create boxplots
#'
#' @param title A character. The plot title (default: NULL).
#' @param xlab A character. The label of the x axis (default: NULL).
#' @param ylab A character. The label of the y axis (default: NULL).
#'
#' @inheritParams continuous_stats_grouped
#'
#' @export
#'
plot_boxplot <- function(dataset,
                         variable,
                         group_var = NULL,
                         title = NULL,
                         xlab = NULL,
                         ylab = NULL
                         ) {

  stopifnot(
    data.table::is.data.table(dataset),
    is.character(variable),
    is.numeric(dataset[, get(variable)]),
    (is.character(group_var) && is.factor(dataset[, get(group_var)])) ||
      is.null(group_var),
    is.character(title) || is.null(title),
    is.character(ylab) || is.null(ylab),
    is.character(xlab) || is.null(xlab)
  )

  p <- ggplot2::ggplot(
    data = dataset
  ) +
    ggplot2::ggtitle(title) +
    ggplot2::ylab(ylab) +
    ggpubr::theme_pubr()

  # only displaying variable (no groups)
  if (is.null(group_var)) {
    p <- p +
      ggplot2::geom_boxplot(
        mapping = ggplot2::aes_string(
          x = factor(0),
          y = variable
        )
      ) +
      ggplot2::scale_x_discrete(breaks = NULL) +
      ggplot2::xlab(NULL)
  } else {
    p <- p +
      ggplot2::geom_boxplot(
        mapping = ggplot2::aes_string(
          x = group_var,
          y = variable
        )
      ) +
      ggplot2::xlab(xlab)
  }
  return(p)
}
