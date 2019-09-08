# create empty tables for model evaluation

# lightgbm
eval.lgb.binary <- data.table(cbind(
  "Number" = integer(),
  "N_features" = integer(),
  "CV_time" = numeric(),
  "best_iter" = integer(),
  "boost_from_avg" = character(),
  "is_unbalance" = character(),
  "learning_rate" = numeric(),
  "num_leaves" = integer(),
  "min_data_in_leaf" = integer(),
  "min_sum_hessian_in_leaf" = numeric(),
  "bagging_fraction" = numeric(),
  "bagging_freq" = integer(),
  "feature_fraction" = numeric(),
  "lambda_l1" = numeric(),
  "lambda_l2" = numeric(),
  "min_gain_to_split" = numeric(),
  "min_data_per_group" = integer(),
  "AUC" = numeric(),
  "PRAUC" = numeric(),
  "Precision" = numeric(),
  "Sensitivity" = numeric(),
  "Specificity" = numeric(),
  "Accuracy" = numeric(),
  "F1_Score" = numeric(),
  "TN" = numeric(), # conmat[1]
  "FN" = numeric(), # conmat[2]
  "FP" = numeric(), # conmat[3]
  "TP" = numeric(), # conmat[4]
  "AUC_PUBLIC" = numeric(),
  "filename" = character()
))

# export table
write.table(eval.lgb.binary, "./eval_binary_lgb.csv", 
            row.names = F, 
            sep = ",", 
            dec = ".", 
            fileEncoding = "UTF-8")
