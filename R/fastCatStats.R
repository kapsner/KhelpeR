initializeTable1Cat <- function(grouped = FALSE){
  if (isFALSE(grouped)){
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

#' @title Calculate Overview Statistics For Categorical vars
#'
#' @param dataset The dataset to analyze. It must be of the class 'data.table'.
#' @param show_levels A logical.
#'
#' @export

fastCatStats <- function(dataset, show_levels = FALSE){

  stopifnot(
    data.table::is.data.table(dataset),
    is.logical(show_levels)
  )

  # subset numeric values
  vec <- colnames(dataset)[dataset[,sapply(.SD, is.factor), .SDcol=colnames(dataset)]]

  stopifnot(
    length(vec) > 0
  )

  # init table1
  if (isFALSE(show_levels)){
    table1 <- initializeTable1Cat()
  } else if (isTRUE(show_levels)){
    table1 <- initializeTable1Cat(grouped = TRUE)
  }

  # iterate over numeric vars
  for (var in vec){
    # get N
    N <- as.numeric(dataset[!is.na(get(var)),.N])

    new_data <- data.table::data.table(
      Name = var,
      N = N,
      "NA" = as.numeric(dataset[is.na(get(var)),.N]),
      Levels = as.numeric(dataset[,nlevels(get(var))]),
      Mode = as.character(dataset[,modeFn(get(var))])
    )
    if (isTRUE(show_levels)){
      new_data[["Category"]] <- ""
      new_data[["% Valid"]] <- ""
    }

    table1 <- rbind(table1, new_data)

    if (isTRUE(show_levels)){
      for (lv in dataset[,levels(get(var))]){
        table1 <- rbind(table1,
                        data.table::data.table(Name = "",
                                               Category = paste0("Group: ", lv),
                                               N = dataset[get(var)==lv,.N],
                                               "NA" = "",
                                               "% Valid" = dataset[get(var)==lv,.N] / N * 100,
                                               Levels = "",
                                               Mode = "")
        )
      }
    }
  }
  return(table1)
}
