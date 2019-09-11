#' @title evalBinaryMetrics
#'
#' @param y_pred A numeric vector.
#' @param y_true A numeric vector.
#' @param positive A character string (defaul: "1"). Indicating the positive class of the binary prediction.
#' @param cutoff A numeric value between 0.1 and 0.9. Indicating the cutoff, if probablities for the positive class are provided with \emph{prediction}.
#'
#' @export
#'

# binary evaluation metrics
evalBinaryMetrics <- function(y_pred, y_true, positive = "1", cutoff=0.5){

  stopifnot(
    is.numeric(y_pred),
    is.numeric(y_true),
    is.numeric(cutoff),
    cutoff >= 0.1 && cutoff <= 0.9
  )

  roc <- PRROC::roc.curve(scores.class0 = y_pred[y_true == positive],
                          scores.class1 = y_pred[y_true != positive],
                          curve = T)
  pr <- PRROC::pr.curve(scores.class0 = y_pred[y_true == positive],
                           scores.class1 = y_pred[y_true != positive],
                        curve = T)
  auc <- MLmetrics::AUC(y_pred = y_pred, y_true = y_true)
  prauc <- MLmetrics::PRAUC(y_pred = y_pred, y_true = y_true)


  y_pred <- as.numeric(as.character(ifelse(y_pred > cutoff, 1, 0)))
  y_true <- as.numeric(as.character(y_true))

  return(
    list(
      auc = auc,
      prauc = prauc,
      precision = MLmetrics::Precision(y_pred = y_pred, y_true = y_true, positive = positive),
      sensitivity = MLmetrics::Sensitivity(y_pred = y_pred, y_true = y_true, positive = positive),
      specificity = MLmetrics::Specificity(y_pred = y_pred, y_true = y_true, positive = positive),
      accuracy = MLmetrics::Accuracy(y_pred = y_pred, y_true = y_true),
      f1score = MLmetrics::F1_Score(y_pred = y_pred, y_true = y_true, positive = positive),
      confusionmatrix = MLmetrics::ConfusionMatrix(y_pred = y_pred, y_true = y_true),
      auc_plot = roc,
      prauc_plot = pr
    )
  )
}


#' @title printEvals
#'
#' @param evalobj A list object. Created e.g. by the function \code{evalBinaryMetrics}.
#'
#' @export
#'
printEvals <- function(evalobj){

  for (i in names(evalobj)){
    if (i == "confusionmatrix"){
      cat(paste0("\n", i, "\n"))
      print(evalobj[[i]])
    } else if (!grepl("_plot", i)){
      cat(paste0("\n", i, ": ", evalobj[[i]]), "\n")
    }
  }
}


#' @title findBestPredictionCutoff
#'
#' @inheritParams evalBinaryMetrics
#'
#' @importFrom magrittr "%>%"
#'
#' @export
#'
findBestPredictionCutoff <- function(y_pred, y_true, cutoff = 0.5){
  # TODO for debugging
  # y_pred <- sample(c(1,0), size = 100, replace = T)
  # y_true <- sample(c(1,0), size = 100, replace = T)
  # cutoff = 0.5

  stopifnot(
    is.numeric(y_pred),
    is.numeric(y_true),
    is.numeric(cutoff),
    cutoff >= 0.1 && cutoff <= 0.9
  )

  # code modified from https://shiring.github.io/machine_learning/2017/05/01/fraud
  pred <- data.table::data.table(
    cbind(predict = y_pred,
          actual = y_true)
  )

  thresholds <- seq(from = 0, to = 1, by = 0.1)

  pred_thresholds <- data.table::data.table(
    cbind(
      actual = pred$actual,
      predict = pred$predict
    )
  )

  pred_thresholds[,(as.character(thresholds)):=lapply(thresholds, function(x){
    prediction <- ifelse(get("predict") > as.numeric(x), 1, 0)
    return(ifelse(get("actual") == 1,
                  ifelse(get("actual") == prediction, "TP", "FN"),
                  ifelse(get("actual") == prediction, "TN", "FP")))
  })]

  return(
    pred_thresholds %>%
      tidyr::gather(x, y, 3:ncol(pred_thresholds)) %>%
      dplyr::group_by(actual, x, y) %>%
      dplyr::summarise(n = dplyr::n()) %>%
      ggplot2::ggplot(
        ggplot2::aes(x = as.numeric(x), y = as.integer(as.character(n)), color = factor(actual))) +
      ggplot2::geom_vline(xintercept = cutoff, alpha = 0.5) +
      ggplot2::geom_line() +
      ggplot2::geom_point(alpha = 0.5) +
      ggplot2::theme_bw() +
      ggplot2::facet_wrap(~ y, scales = "free", ncol = 2) +
      ggplot2::labs(x = "prediction threshold",
                    y = "number of observations",
                    color = "Ground truth",
                    title = paste0("Prediction at cutoff = ", cutoff))
  )
}
