#' @title Create grouped statistics of one continuous variable
#'
#' @description This function creates grouped statistics of one variable.
#'
#' @param variable A character. Name of the variable to analyze.
#'
#' @inheritParams continuous_stats
#'
#' @export
#'
continuous_stats_grouped <- function(dataset, variable, group_var, digits = 2){

  stopifnot(
    data.table::is.data.table(dataset),
    is.character(variable),
    is.character(group_var),
    is.numeric(dataset[, get(variable)]),
    is.factor(dataset[, get(group_var)])
  )

  retdt <- extensive_stats(NULL)
  outdat <- data.table::data.table(" " = colnames(retdt))

  cols <- ""

  for (cat in dataset[, unique(get(group_var))]) {
    # append cat to colnames
    cols <- c(cols, cat)

    outdat <- cbind(outdat,
                    data.table::data.table(
                      t(dataset[get(group_var) == cat,
                                extensive_stats(
                                  vector = get(variable),
                                  digits = digits
                                )]
                      )
                    ))
  }
  colnames(outdat) <- c(" ", paste("Category:", cols[2:length(cols)]))

  return(outdat)
}
