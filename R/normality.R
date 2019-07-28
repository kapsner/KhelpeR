# shapiro utility
shaprioUtil <- function(dataset, variable, digits=3){
  outdat <- tryCatch({
    shap <- stats::shapiro.test(dataset[,get(variable)])
    outdat <- data.table::data.table(
      rbind(c("Method:", shap$method),
            c("W-statistic:", unname(round(shap$statistic, digits))),
            c("p-value:", round(shap$p.value, digits)))
    )
  }, error = function(e){
    outdat <- data.table::data.table(
      rbind(c("Method:", ""),
            c("W-statistic:", "Error"),
            c("p-value:", "Error"))
    )
  }, finally = function(f){
    return(outdat)
  })
  colnames(outdat) <- c("", "value")
  return(outdat)
}
