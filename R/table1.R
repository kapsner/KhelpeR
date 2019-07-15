initializeTable1 <- function(grouped = FALSE){
  if (isFALSE(grouped)){
    table1 <- data.table::data.table("Name" = character(),
                                     "N" = integer(),
                                     "Distribution (min/mean/med/max/sd)" = character(),
                                     "Normality" = character())
  } else {
    table1 <- data.table::data.table("Name" = character(),
                                     "Group" = character(),
                                     "N" = integer(),
                                     "Distribution (min/mean/med/max/sd)" = character(),
                                     "Normality" = character(),
                                     "Homoscedasticity" = character())
  }
  return(table1)
}


#' @title Genereate Overview Table for Numeric Variables
#'
#' @param dataset The dataset to analyze. It must be of the class 'data.table'.
#' @param group_var A character. Name of the grouping variable. The grouping variable
#'   should be of the type 'factor'.
#' @param digits An integer. Number of digits to round numeric variables (default: 2).
#'
#' @importFrom data.table .SD .N
#'
#' @export

fillTable1Num <- function(dataset, group_var = NULL, digits = 2){
  # test, if dataset ist data.table
  if (!data.table::is.data.table(dataset)){
    return("data provided must be a data.table object")
  } else {
    # subset numeric values
    vec <- colnames(dataset)[dataset[,sapply(.SD, is.numeric), .SDcol=colnames(dataset)]]

    if (length(vec) <= 0){
      return("your dataset does not contain numeric variables")
    }

    # init table1
    if (is.null(group_var)){
      table1 <- initializeTable1()
    } else {
      table1 <- initializeTable1(grouped = TRUE)
    }

    # iterate over numeric variables
    for (variable in vec){
      shap <- shaprioUtil(dataset, variable, digits = digits)
      new_data <- data.table::data.table(
        Name = variable,
        N = as.numeric(dataset[!is.na(get(variable)),.N]),
        "Distribution (min/mean/med/max/sd)" = distribution(dataset, variable, digits),
        Normality = paste0("p=", shap[3,"value",with=F], pMarker(shap[3,"value",with=F]), paste0(" (W=", shap[2,"value",with=F], ")"))
      )
      if (!is.null(group_var)){
        levene <- leveneUtil(dataset, variable, group_var, digits = digits)
        new_data[["Group"]] <- ""
        new_data[["Homoscedasticity"]] <- paste0("p=", levene[3,"value",with=F], pMarker(levene[3,"value",with=F]), paste0(" (F=", levene[2,"value",with=F], ")"))
      }

      # append new data to table 1
      table1 <- rbind(table1, new_data)

      if (!is.null(group_var)){
        for (gr in dataset[,unique(get(group_var))]){
          subset <- dataset[get(group_var)==gr,]
          shap_sub <- shaprioUtil(subset, variable, digits = digits)
          table1 <- rbind(table1,
                          data.table::data.table(Name = "",
                                                 Group = paste0("Group: ", gr),
                                                 N = subset[!is.na(get(variable)),.N],
                                                 "Distribution (min/mean/med/max/sd)" = distribution(subset, variable, digits),
                                                 Normality = paste0("p=", shap_sub[3,"value",with=F], pMarker(shap_sub[3,"value",with=F]), paste0(" (W=", shap_sub[2,"value",with=F], ")")),
                                                 Homoscedasticity = "")
          )
        }
      }
    }
    return(table1)
  }
}
