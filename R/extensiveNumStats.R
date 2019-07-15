extensiveNumStats <- function(vector, rows=F, digits = 2){

  if (!is.null(vector)){
    if (isTRUE(rows)){
      vector <- unname(t(vector))
    }

    fac_vec <- factor(vector)
    Q <- stats::quantile(vector, probs=c(.25, .75), na.rm=T, names=F)
    I_out <- stats::IQR(vector, na.rm=T)*1.5

    retdt <- list(n = as.numeric(base::sum(!is.na(vector))),
                  is_na = as.numeric(base::sum(is.na(vector))),
                  unique = as.numeric(base::nlevels(fac_vec)),
                  min = round(as.numeric(base::min(vector, na.rm = T)), digits),
                  q25 = round(as.numeric(Q[1]), digits),
                  q50 = round(as.numeric(stats::median(vector, na.rm = T)), digits),
                  mean = round(as.numeric(base::mean(vector, na.rm = T)), digits),
                  q75 = round(as.numeric(Q[2]), digits),
                  max = round(as.numeric(base::max(vector, na.rm = T)), digits),
                  sd = round(as.numeric(stats::sd(vector, na.rm = T)), digits),
                  neg = as.numeric(base::sum(vector < 0, na.rm = T)),
                  zero = as.numeric(base::sum(vector == 0, na.rm = T)),
                  pos = as.numeric(base::sum(vector > 0, na.rm = T)),
                  outLo = as.numeric(base::sum(vector < (Q[1]-I_out), na.rm = T)),
                  outHi = as.numeric(base::sum(vector > (Q[2]+I_out), na.rm = T)),
                  mad_mean = round(as.numeric(stats::mad(vector, center = base::mean(vector), na.rm=T)), digits),
                  mad_med = round(as.numeric(stats::mad(vector, center = stats::median(vector), na.rm=T)), digits),
                  skeweness = round(as.numeric(e1071::skewness(vector, na.rm=T)), digits),
                  kurtosis = round(as.numeric(e1071::kurtosis(vector, na.rm=T)), digits),
                  variance = round(as.numeric(stats::var(vector, na.rm=T)), digits),
                  range = round(as.numeric(base::max(vector, na.rm=T) - base::min(vector, na.rm=T)), digits),
                  iqr = round(as.numeric(stats::IQR(vector, na.rm=T)), digits),
                  se = round(as.numeric(se(vector)),  digits)
    )
  } else {
    # return empty table for initialization
    retdt <- data.table::data.table(
      cbind(n = numeric(),
            is_na = numeric(),
            unique = numeric(),
            min = numeric(),
            q25 = numeric(),
            q50 = numeric(),
            mean = numeric(),
            q75 = numeric(),
            max = numeric(),
            sd = numeric(),
            neg = numeric(),
            zero = numeric(),
            pos = numeric(),
            outLo = numeric(),
            outHi = numeric(),
            mad_mean = numeric(),
            mad_med = numeric(),
            skeweness = numeric(),
            kurtosis = numeric(),
            variance = numeric(),
            range = numeric(),
            iqr = numeric(),
            se = numeric()
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
numStatsTable <- function(dataset, group_var = NULL, method = "all"){
  # test, if dataset ist data.table
  if (!data.table::is.data.table(dataset)){
    return("data provided must be a data.table object")
  } else {
    # subset numeric values
    vec <- colnames(dataset)[dataset[,sapply(.SD, is.numeric), .SDcol=colnames(dataset)]]

    if (length(vec) <= 0){
      return("your dataset does not contain numeric variables")
    }

    # init table
    table <- extensiveNumStats(NULL)

    if (is.null(group_var)){
      table <- cbind(Name = character(), table)
      for (variable in vec){
        table <- rbind(table, cbind(Name = variable, dataset[,extensiveNumStats(get(variable))]))
      }
    } else {
      # TODO test for factor or character here
      table <- cbind(Name = character(), Group = character(), table)
      for (variable in vec){
        table <- rbind(table, cbind(Name = variable, Group = "", dataset[,extensiveNumStats(get(variable))]))
        for (group in dataset[,unique(get(group_var))]){
          table <- rbind(table, cbind(Name = "", Group = group, dataset[get(group_var)==group,extensiveNumStats(get(variable))]))
        }
      }
    }
    if (method == "base"){
      vec <- c("neg", "zero", "pos", "outLo", "outHi",
               "mad_mean", "mad_med", "skeweness", "kurtosis",
               "variance", "range", "iqr", "se")
      table[,(vec):=NULL]
    } else if (method == "others"){
      vec <- c("unique", "min", "q25", "q50",
               "mean", "q75", "max", "sd")
      table[,(vec):=NULL]
    }
    return(data.table::data.table(table))
  }
}
