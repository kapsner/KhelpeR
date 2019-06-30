extensiveNumStats <- function(vector, rows=F){

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
                  min = as.numeric(base::min(vector, na.rm = T)),
                  q25 = as.numeric(Q[1]),
                  q50 = as.numeric(stats::median(vector, na.rm = T)),
                  mean = as.numeric(base::mean(vector, na.rm = T)),
                  q75 = as.numeric(Q[2]),
                  max = as.numeric(base::max(vector, na.rm = T)),
                  sd = as.numeric(stats::sd(vector, na.rm = T)),
                  neg = as.numeric(base::sum(vector < 0, na.rm = T)),
                  zero = as.numeric(base::sum(vector == 0, na.rm = T)),
                  pos = as.numeric(base::sum(vector > 0, na.rm = T)),
                  outLo = as.numeric(base::sum(vector < (Q[1]-I_out), na.rm = T)),
                  outHi = as.numeric(base::sum(vector > (Q[2]+I_out), na.rm = T)),
                  mad_mean = as.numeric(stats::mad(vector, center = mean(vector), na.rm=T)),
                  mad_med = as.numeric(stats::mad(vector, center = median(vector), na.rm=T)),
                  skeweness = as.numeric(e1071::skewness(vector, na.rm=T)),
                  kurtosis = as.numeric(e1071::kurtosis(vector, na.rm=T)),
                  variance = as.numeric(stats::var(vector, na.rm=T)),
                  range = as.numeric(base::max(vector, na.rm=T) - base::min(vector, na.rm=T)),
                  iqr = as.numeric(stats::IQR(vector, na.rm=T)),
                  se = as.numeric(se(vector))
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


numStatsTable <- function(dataset, group_var = NULL){
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
      table <- cbind(Name = character(), Group = character(), table)
      for (variable in vec){
        table <- rbind(table, cbind(Name = variable, Group = "", dataset[,extensiveNumStats(get(variable))]))
        for (group in dataset[,unique(get(group_var))]){
          table <- rbind(table, cbind(Name = "", Group = group, dataset[get(group_var)==group,extensiveNumStats(get(variable))]))
        }
      }
    }
    return(table)
  }
}
