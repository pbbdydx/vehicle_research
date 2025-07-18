# imports


# import models







#function to evaluate models

evaluate_model <- function(y_true, predicted_probs) {
  thresholds <- seq(0.01, 0.99, by = 0.01)

  metrics <- data.frame(
    Threshold = thresholds,
    Precision = NA,
    Recall = NA,
    F1 = NA,
    Accuracy = NA
  )

  for (i in seq_along(thresholds)) {
    t <- thresholds[i]
    preds <- ifelse(predicted_probs > t, 1, 0)
    metrics$Precision[i] <- Precision(preds, y_true)
    metrics$Recall[i] <- Recall(preds, y_true)
    metrics$F1[i] <- F1_Score(preds, y_true)
    metrics$Accuracy[i] <- Accuracy(preds, y_true)
  }

  best <- metrics[which.max(metrics$F1), ]
  return(best)
}


preds <- list(
  Logistic = log_pred,
  XGBoost = xbg_pred,
  RandomForest = rf_pred,
  SVM = svm_pred,
  Logistic_rose = log_pred_rose,
  XGBoost_rose = xgb_pred_rose,
  RandomForest_rose = rf_pred_rose,
  SVM_rose = svm_pred_rose,
  GLMNet = glmnet_pred,
  GLMNet_rose = glmnet_pred_rose
)



# evaluate model at optimal F1 score
results <- lapply(preds, function(p) evaluate_model(y_bin_test, p))
results_df <- do.call(rbind, results)
results_df <- cbind(Model = names(preds), results_df)

# Print table
print(results_df[order(-results_df$F1), ], row.names = FALSE)

# ROC curves
roc_list <- lapply(preds, function(p) roc(response = y_bin_test, predictor = p))
plot(roc_list[[1]], col = 1, main = "ROC Curves", legacy.axes = TRUE)
for (i in 2:length(roc_list)) lines(roc_list[[i]], col = i)
legend("bottomright", legend = names(preds), col = 1:length(preds), lty = 1, cex = 0.7)








# ------- Analysis -------