#' @title lgbCV
#'
#' @param data A lgb.Dataset.
#' @param params A list containing lgb.params.
#' @param eval Either a character string, defining a valid evaluation metric, or a custom evaluation metric.
#' @param cat_features A vector containing columnnames of categorical features (defaul: NULL).
#' @param validationset A lgb.Dataset containing validation data.
#' @param earlystop A integer value. Number of early stopping iterations (default: 1000).
#' @param seed A integer value. Seed for reproducibility (default: 17).
#' @param eval_freq A integer value (default: 20).
#' @param nfold A integer value (default: 10).
#' @param nfold A integer value (default: 5e4)
#'
#' @export
#'
#'
lgbCV <- function(data, params, eval, cat_features = NULL, validationset, earlystop = 1000, seed = 17,
                  eval_freq = 20, nfold = 10, nrounds = 5e4, verbose = 1){

  # measure time
  start.time <- Sys.time()
  cat("\nStart Time cross-validation:", getTimestamp("human"), "\n\n")

  # cv
  set.seed(seed)
  lgb_cv <- lgb.cv(params = params,
                   data = data,
                   eval = eval,
                   eval_freq = eval_freq,
                   nrounds = nrounds,
                   nfold = nfold,
                   early_stopping_rounds = earlystop,
                   stratified = T,
                   showsd = T,
                   categorical_feature = cat_features,
                   verbose = verbose)

  cat("Best iter:", lgb_cv$best_iter)

  # measure time
  end.time <- Sys.time()
  time.taken_cv <- end.time - start.time
  cat("\nTime Taken:", time.taken_cv, "\n\n")

  valids <- list(validation = validationset)


  # measure time
  start.time <- Sys.time()
  cat("\nStart Time validation training:", getTimestamp("human"), "\n\n")

  set.seed(seed)
  lgb.model <- lgb.train(params = params,
                         data = data,
                         eval = eval,
                         nrounds = lgb_cv$best_iter,
                         valids = valids,
                         eval_freq = eval_freq,
                         categorical_feature = cat_features)

  # measure time
  end.time <- Sys.time()
  time.taken <- end.time - start.time
  cat("\nTime Taken:", time.taken, "\n\n")

  return(list(model = lgb.model,
              best.iter = lgb_cv$best_iter,
              time = time.taken_cv))
}


lgb_final_regression <- function(data, params, eval, best.iter, cat_features=NULL){

  # measure time
  start.time <- Sys.time()
  cat("\nStart Time submission training:", getTimestamp("human"), "\n\n")

  set.seed(17)
  lgb.model <- lgb.train(params = params,
                         data = data,
                         eval = eval,
                         nrounds = best.iter,
                         eval_freq = 20,
                         categorical_feature = cat_features)

  # measure time
  end.time <- Sys.time()
  time.taken <- end.time - start.time
  cat("\nTime Taken:", time.taken, "\n\n")

  return(lgb.model)
}


# Hyperparameters binary:
# (default) parameters:
# https://lightgbm.readthedocs.io/en/latest/Parameters.html
# lgb.params <- list(objective = "binary",
#                    metric = "auc",
#                    boosting = "gbdt",               # default: gdbt
#                    boost_from_average="false",      # default: false
#                    is_unbalance="false",             # default: false
#scale_pos_weight = sum(label_train == 0) / sum(label_train == 1),      # default = 1.0
#                    learning_rate = 0.1,             # default: 0.1
#                    num_leaves = 31,                 # default: 31
#                    tree_learner = "voting",         # default: serial
#                    num_threads = 4,                 # default: 0
#                    seed = 17,                       # defaul: None
#                    max_depth = -1,                  # default: -1
#                    min_data_in_leaf  = 20,          # default: 20 (= min_child_samples)
#                    min_sum_hessian_in_leaf = 1e-3,  # default: 1e-3 (= min_child_weight)
#                    bagging_fraction = 0.7,          # default: 1.0 (= subsample)
#                    bagging_freq = 5,                # default: 0
#                    feature_fraction = 0.7,          # default: 1.0 (= colsample_bytree)
#                    lambda_l1 = 0.0,                 # default: 0.0
#                    lambda_l2 = 0.0,                 # default: 0.0
#                    min_gain_to_split = 0.0,         # default: 0.0
#                    min_data_per_group = 100,        # default: 100
#                    max_cat_threshold = 32,          # default: 32
#                    cat_l2 = 10.0,                   # default: 10.0
#                    cat_smooth = 10.0,               # default: 10.0
#                    max_cat_to_onehot = 4,           # default: 4
#                    max_bin = 255,                   # default: 255
#                    min_data_in_bin=3)               # default: 3


# custom metrics:
#' @title lgb_prauc
#'
#' @param preds Predictions.
#' @param dtrain A lgb.Dataset.
#'
#' @export

# https://stats.stackexchange.com/questions/10501/calculating-aupr-in-r
# area under the precision-recall-curve is better for unbalanced targets
lgb_prauc <- function(preds, dtrain){
  # to not disturb parallelization of lightgbm, use as few functions as possible

  probs <- data.table::data.table(cbind(labels = getinfo(dtrain, "label"),
                                        preds = preds))

  return(
    list(
      name = "prauc",
      higher_better = TRUE,
      value = PRROC::pr.curve(
        scores.class0 = probs[labels == "1",preds],
        scores.class1 = probs[labels == "0",preds],
        curve = F)[["auc.integral"]]
    )
  )
}
