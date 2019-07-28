# levene utility
leveneUtil <- function(dataset, variable, group, digits = 3){
  outdat <- tryCatch({
    levene <- car::leveneTest(y = dataset[,get(variable)], group = dataset[,get(group)], center = stats::median)
    outdat <- data.table::data.table(
      rbind(c("Method:", attributes(levene)$heading),
            c("F-value:", round(levene$`F value`[1], digits)),
            c("p-value:", round(levene$`Pr(>F)`[1], digits)))
    )
  }, error = function(e){
    outdat <- data.table::data.table(
      rbind(c("Method:", ""),
            c("F-value:", "Error"),
            c("p-value:", "Error"))
    )
  }, finally = function(f){
    return(outdat)
  })
  colnames(outdat) <- c("", "value")
  return(outdat)
}
