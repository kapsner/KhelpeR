#' @title Create statistics table for continuous variables
#'
#' @description This function creates a vast amount of statistics
#'
#' @param dataset The dataset to analyze. It must be of the class 'data.table'.
#' @param group_var A character. Name of the grouping variable. The grouping
#'   variable should be of the type 'factor'.
#' @param method A character. The format of the resulting table. One of: "all"
#'   (default) to show extensive numeric statistis, "base" to show a table
#'   with basic numeric statistics and "others" to show the complexer other
#'   part of the numeric statistics.
#' @param digits An integer. The number of decimal digits in the table
#'   (default: 2).
#'
#' @importFrom magrittr %>%
#' @importFrom data.table ":=" ".N" ".SD"
#'
#' @export
#'
continuous_stats <- function(dataset,
                             group_var = NULL,
                             method = "all",
                             digits = 2){

  stopifnot(
    data.table::is.data.table(dataset),
    is.character(method),
    method %in% c("all", "base", "others"),
    ifelse(is.null(group_var), TRUE, is.character(group_var)),
    is.numeric(digits)
  )

  # subset numeric variables
  #% vec <- colnames(dataset)[dataset[,sapply(.SD, is.numeric),
  #% .SDcols = colnames(dataset)]]

  # subset numeric variables
  vec <- subset_num_cols(dataset)
  stopifnot(length(vec) > 0)

  # init outtab
  outtab <- extensive_stats(NULL)

  if (is.null(group_var)) {
    outtab <- cbind(Name = character(),
                   Type = character(),
                   outtab)
    for (variable in vec) {
      outtab <- data.table::rbindlist(
        list(
          outtab,
          cbind(Name = variable,
                Type = dataset[, class(get(variable))],
                dataset[, extensive_stats(
                  vector = get(variable),
                  digits = digits
                )]
          )
        ),
        fill = TRUE
      )
    }
  } else {
    # TODO test for factor or character here
    vec <- setdiff(vec, group_var)
    outtab <- cbind(Name = character(),
                   Group = character(),
                   Type = character(),
                   outtab)
    for (variable in vec) {
      outtab <- data.table::rbindlist(
        list(
          outtab,
          cbind(Name = variable,
                Group = "",
                Type = dataset[, class(get(variable))],
                dataset[, extensive_stats(
                  vector = get(variable),
                  digits = digits
                )]
          )
        ),
        fill = TRUE
      )
      for (group in dataset[, unique(get(group_var))]) {
        outtab <- data.table::rbindlist(
          list(
            outtab,
            cbind(Name = "",
                  Group = paste0("Group: ", group),
                  Type = "",
                  dataset[get(group_var) == group,
                          extensive_stats(
                            vector = get(variable),
                            digits = digits
                          )]
            )
          ),
          fill = TRUE
        )
      }
    }
  }
  if (method == "base") {
    vec <- c("Neg", "Zero", "Pos", "OutLo", "OutHi",
             "MAD_mean", "MAD_med", "Skeweness", "Kurtosis",
             "Variance", "Range", "IQR", "SE")
    outtab[, (vec) := NULL]
  } else if (method == "others") {
    vec <- c("Unique", "Min", "Q25", "Q50",
             "Mean", "Q75", "Max", "SD")
    outtab[, (vec) := NULL]
  }
  return(data.table::data.table(outtab))
}


# create extensive statistics
extensive_stats <- function(vector,
                            rows = F,
                            digits = 2) {

  if (!is.null(vector)) {
    if (isTRUE(rows)) {
      vector <- unname(t(vector))
    }

    # for all variable types
    fac_vec <- factor(vector)

    retdt <- list(
      N = as.numeric(base::sum(!is.na(vector))),
      "NA" = as.numeric(base::sum(is.na(vector))),
      Unique = as.numeric(base::nlevels(fac_vec))
    )

    # only numeric variable types
    if (is.numeric(vector)) {

      q <- stats::quantile(
        vector,
        probs = c(.25, .75),
        na.rm = T,
        names = F
      )
      i_out <- stats::IQR(
        vector,
        na.rm = T
      ) * 1.5

      retdt <- c(
        retdt,
        list(
          Min = round(
            as.numeric(base::min(vector, na.rm = T)),
            digits
          ),
          Q25 = round(
            as.numeric(q[1]),
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
            as.numeric(q[2]), digits
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
            base::sum(vector < (q[1] - i_out), na.rm = T)
          ),
          OutHi = as.numeric(
            base::sum(vector > (q[2] + i_out), na.rm = T)
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
            as.numeric(stats::var(vector, na.rm = T)),
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
