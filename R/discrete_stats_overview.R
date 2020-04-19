#' @title Calculate overview statistics of discrete variables
#'
#' @param show_levels A logical. Display the levels of discrete variables
#'   (default: FALSE).
#'
#' @inheritParams continuous_stats
#'
#' @export
discrete_stats_overview <- function(dataset, show_levels = FALSE, digits = 2) {

  stopifnot(
    data.table::is.data.table(dataset),
    is.logical(show_levels)
  )

  # subset numeric values
  vec <- subset_cat_cols(dataset)
  stopifnot(length(vec) > 0)

  # init table1
  if (isFALSE(show_levels)) {
    table1 <- initialize_table1_cat()
  } else if (isTRUE(show_levels)) {
    table1 <- initialize_table1_cat(grouped = TRUE)
  }

  # iterate over numeric vars
  for (var in vec) {
    # get N
    n <- as.numeric(dataset[!is.na(get(var)), .N])

    new_data <- data.table::data.table(
      Name = var,
      N = n,
      "NA" = as.numeric(dataset[is.na(get(var)), .N]),
      Levels = as.numeric(dataset[, nlevels(get(var))]),
      Mode = as.character(dataset[, mode_fn(get(var))])
    )
    if (isTRUE(show_levels)) {
      new_data[["Category"]] <- "all"
      new_data[["% Valid"]] <- ""
    }

    table1 <- data.table::rbindlist(
      l = list(
        table1,
        new_data
      ),
      fill = T
    )

    if (isTRUE(show_levels)) {
      for (lv in dataset[, levels(get(var))]) {
        table1 <- rbind(
          table1,
          data.table::data.table(
            Name = var,
            Category = paste0("Group: ", lv),
            N = dataset[get(var) == lv, .N],
            "NA" = "",
            "% Valid" = round(
              dataset[get(var) == lv, .N] / n * 100,
              digits
            ),
            Levels = "",
            Mode = ""
          )
        )
      }
    }
  }
  return(table1)
}

initialize_table1_cat <- function(grouped = FALSE) {
  if (isFALSE(grouped)) {
    table1 <- data.table::data.table("Name" = character(),
                                     "N" = integer(),
                                     "NA" = integer(),
                                     "Levels" = integer(),
                                     "Mode" = character())
  } else {
    table1 <- data.table::data.table("Name" = character(),
                                     "Category" = character(),
                                     "N" = integer(),
                                     "NA" = integer(),
                                     "% Valid" = integer(),
                                     "Levels" = integer(),
                                     "Mode" = character())
  }
  return(table1)
}
