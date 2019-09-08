#' @title evalBinaryMetrics
#'
#' @param prediction A numeric vector.
#' @param true_vals A numeric vector.
#' @param positive A character string (defaul: "1"). Indicating the positive class of the binary prediction.
#' @param cutoff A numeric value between 0.1 and 0.9. Indicating the cutoff, if probablities for the positive class are provided with \emph{prediction}.
#'
#' @export
#'

# binary evaluation metrics
evalBinaryMetrics <- function(prediction, true_vals, positive = "1", cutoff=0.5){

  stopifnot(
    is.numeric(prediction),
    is.numeric(true_vals),
    cutoff >= 0.1 && cutoff <= 0.9
  )

  y_pred <- as.numeric(as.character(ifelse(prediction > cutoff, 1, 0)))
  y_true <- as.numeric(as.character(true_vals))

  roc_pred <- ROCR::prediction(prediction, true_vals)

  return(
    list(
      auc = MLmetrics::AUC(y_pred = prediction, y_true = y_true),
      prauc = MLmetrics::PRAUC(y_pred = prediction, y_true = y_true),
      pre = MLmetrics::Precision(y_pred = y_pred, y_true = y_true, positive = positive),
      sen = MLmetrics::Sensitivity(y_pred = y_pred, y_true = y_true, positive = positive),
      spe = MLmetrics::Specificity(y_pred = y_pred, y_true = y_true, positive = positive),
      acc = MLmetrics::Accuracy(y_pred = y_pred, y_true = y_true),
      f1s = MLmetrics::F1_Score(y_pred = y_pred, y_true = y_true, positive = positive),
      conmat = MLmetrics::ConfusionMatrix(y_pred = y_pred, y_true = y_true),
      roc.perf = ROCR::performance(roc_pred, measure = "tpr", x.measure = "fpr")
    )
  )
}
