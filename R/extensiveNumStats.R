extensive_stats <- function(vector,
                            rows = F,
                            digits = 2) {

  if (!is.null(vector)) {
    if (isTRUE(rows)) {
      vector <- unname(t(vector))
    }

    fac_vec <- factor(vector)
    Q <- stats::quantile(
      vector,
      probs = c(.25, .75),
      na.rm = T,
      names = F
    )
    I_out <- stats::IQR(
      vector,
      na.rm = T
    ) * 1.5

    retdt <- list(
      N = as.numeric(base::sum(!is.na(vector))),
      "NA" = as.numeric(base::sum(is.na(vector))),
      Unique = as.numeric(base::nlevels(fac_vec))
    )

    if (is.numeric(vector)) {
      retdt <- c(retdt,
                 list(
                   Min = round(
                     as.numeric(base::min(vector, na.rm = T)),
                     digits
                   ),
                   Q25 = round(
                     as.numeric(Q[1]),
                     digits
                   ),
                   Q50 = round(
                     as.numeric(stats::median(vector, na.rm = T)),
                     digits
                   ),
                   Mean = round(
                     as.numeric(base::mean(vector, na.rm = T)),
                     digits
                   ),
                   Q75 = round(
                     as.numeric(Q[2]), digits
                   ),
                   Max = round(
                     as.numeric(base::max(vector, na.rm = T)),
                     digits
                   ),
                   SD = round(
                     as.numeric(stats::sd(vector, na.rm = T)),
                     digits
                   ),
                   Neg = as.numeric(
                     base::sum(vector < 0, na.rm = T)
                   ),
                   Zero = as.numeric(
                     base::sum(vector == 0, na.rm = T)
                   ),
                   Pos = as.numeric(
                     base::sum(vector > 0, na.rm = T)
                   ),
                   OutLo = as.numeric(
                     base::sum(vector < (Q[1] - I_out), na.rm = T)
                   ),
                   OutHi = as.numeric(
                     base::sum(vector > (Q[2] + I_out), na.rm = T)
                   ),
                   MAD_mean = round(
                     as.numeric(stats::mad(
                       vector,
                       center = base::mean(vector),
                       na.rm = T)
                     ),
                     digits
                   ),
                   MAD_med = round(
                     as.numeric(stats::mad(
                       vector,
                       center = stats::median(vector),
                       na.rm = T)
                     ),
                     digits
                   ),
                   Skeweness = round(
                     as.numeric(e1071::skewness(vector, na.rm = T)),
                     digits
                   ),
                   Kurtosis = round(
                     as.numeric(e1071::kurtosis(vector, na.rm = T)),
                     digits
                   ),
                   Variance = round(
                     as.numeric(stats::var(vector, na.rm=T)),
                     digits
                   ),
                   Range = round(
                     as.numeric(base::max(vector, na.rm = T) -
                                  base::min(vector, na.rm = T)),
                     digits
                   ),
                   IQR = round(
                     as.numeric(stats::IQR(vector, na.rm = T)),
                     digits
                   ),
                   SE = round(
                     as.numeric(se(vector)),
                     digits
                   )
                 )
      )
    }
  } else {
    # return empty table for initialization
    retdt <- data.table::data.table(
      cbind(N = numeric(),
            "NA" = numeric(),
            Unique = numeric(),
            Min = numeric(),
            Q25 = numeric(),
            Q50 = numeric(),
            Mean = numeric(),
            Q75 = numeric(),
            Max = numeric(),
            SD = numeric(),
            Neg = numeric(),
            Zero = numeric(),
            Pos = numeric(),
            OutLo = numeric(),
            OutHi = numeric(),
            MAD_mean = numeric(),
            MAD_med = numeric(),
            Skeweness = numeric(),
            Kurtosis = numeric(),
            Variance = numeric(),
            Range = numeric(),
            IQR = numeric(),
            SE = numeric()
      )
    )
  }
  return(retdt)
}


#' @title Create Numeric Statistics
#'
#' @description This function creates a vast amount of numeric statistics
#'
#' @param dataset The dataset to analyze. It must be of the class 'data.table'.
#' @param group_var A character. Name of the grouping variable. The grouping variable
#'   should be of the type 'factor'.
#' @param method A character. The format of the resulting table. One of: "all" (default)
#'   to show extensive numeric statistis, "base" to show a table with basic numeric
#'   statistics and "others" to show the complexer other part of the numeric statistics.
#'
#' @importFrom data.table ":="
#'
#' @export
#'
stats_table <- function(dataset, group_var = NULL, method = "all"){

  stopifnot(
    data.table::is.data.table(dataset),
    is.character(method),
    ifelse(is.null(group_var), TRUE, is.character(group_var))
  )

  # subset numeric variables
  #% vec <- colnames(dataset)[dataset[,sapply(.SD, is.numeric), .SDcols = colnames(dataset)]]

  # subset all variables
  vec <- colnames(dataset)

  stopifnot(
    length(vec) > 0
  )

  # init table
  table <- extensive_stats(NULL)

  if (is.null(group_var)){
    table <- cbind(Name = character(), table)
    for (var in vec){
      table <- data.table::rbindlist(
        table,
        cbind(Name = var, dataset[,extensive_stats(get(var))]),
        fill = TRUE
      )
    }
  } else {
    # TODO test for factor or character here
    table <- cbind(Name = character(), Group = character(), table)
    for (variable in vec){
      table <- data.table::rbindlist(
        table,
        cbind(Name = var, Group = "", dataset[,extensive_stats(get(var))]),
        fill = TRUE
      )
      for (group in dataset[,unique(get(group_var))]){
        table <- data.table::rbindlist(
          table,
          cbind(Name = "",
                Group = paste0("Group: ", group),
                dataset[get(group_var)==group,extensive_stats(get(variable))]
          )
          fill = TRUE
        )
      }
    }
  }
  if (method == "base"){
    vec <- c("Neg", "Zero", "Pos", "OutLo", "OutHi",
             "MAD_mean", "MAD_med", "Skeweness", "Kurtosis",
             "Variance", "Range", "IQR", "SE")
    table[,(vec):=NULL]
  } else if (method == "others"){
    vec <- c("Unique", "Min", "Q25", "Q50",
             "Mean", "Q75", "Max", "SD")
    table[,(vec):=NULL]
  }
  return(data.table::data.table(table))
}


#' @title Create Numeric Statistics of one Variable per Category
#'
#' @description This function creates a vast amount of numeric statistics per category
#'
#' @param dataset The dataset to analyze. It must be of the class 'data.table'.
#' @param group_var A character. Name of the grouping variable. The grouping variable
#'   should be of the type 'factor'.
#' @param var A character. Name of the variable to analyze.
#'
#' @importFrom magrittr %>%
#' @importFrom data.table ":=" ".N" ".SD"
#'
#' @export
#'
stats_tablePerCat <- function(dataset, var, group_var){

  stopifnot(
    data.table::is.data.table(dataset),
    is.character(variable),
    is.character(group_var),
    is.numeric(dataset[,get(var)]),
    is.factor(dataset[,get(group_var)])
  )

  retdt <- extensive_stats(NULL)
  outdat <- data.table::data.table(" " = colnames(retdt))

  cols <- ""

  for (cat in dataset[,unique(get(group_var))]){
    # append cat to colnames
    cols <- c(cols, cat)

    outdat <- cbind(outdat,
                    data.table::data.table(
                      t(dataset[get(group_var)==cat,extensive_stats(get(var))])
                    ))
  }
  colnames(outdat) <- c(" ", paste("Category:", cols[2:length(cols)]))

  return(outdat)
}
