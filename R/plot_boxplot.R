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
                         method = NULL,
                         group_var = NULL,
                         title = NULL,
                         xlab = NULL,
                         ylab = NULL
                         ) {

  stopifnot(
    data.table::is.data.table(dataset),
    is.character(variable),
    is.numeric(dataset[, get(variable)]),
    ifelse(is.null(group_var), TRUE,
           is.character(group_var) && is.factor(dataset[, get(group_var)]) &&
             is.character(method) && method %in%
             c("t.test", "wilcox.test", "anova", "kruskal.test")),
    is.character(title) || is.null(title),
    is.character(ylab) || is.null(ylab),
    is.character(xlab) || is.null(xlab)
  )

  # only displaying variable (no groups)
  if (is.null(group_var)) {
    p <- ggplot2::ggplot(
      data = dataset
    ) +
      ggplot2::ggtitle(title) +
      ggplot2::ylab(ylab) +
      ggpubr::theme_pubr() +
      ggplot2::geom_boxplot(
        mapping = ggplot2::aes_string(
          x = factor(0),
          y = variable
        )
      ) +
      ggplot2::scale_x_discrete(breaks = NULL) +
      ggplot2::xlab(NULL)

  } else {

    combinations <- gtools::combinations(
      n = length(unique(dataset[, get(group_var)])),
      r = 2,
      v = unique(dataset[, get(group_var)])
    )

    comparisons <- lapply(
      X = seq_len(nrow(combinations)),
      FUN = function(x) {
        combinations[1, ]
      })

    symnum_args <- list(
      cutpoints = c(0, 0.001, 0.01, 0.05, 1),
      symbols = c("***", "**", "*", "ns")
    )


    p <- ggpubr::ggboxplot(
      data = dataset,
      x = group_var,
      y = variable,
      xlab = xlab,
      ylab = ylab,
      title = title,
      ggtheme = ggpubr::theme_pubr()
    ) +
      ggpubr::stat_compare_means(
        method = method,
        comparisons = comparisons,
        symnum.args = symnum_args
      )

  }
  return(p)
}
