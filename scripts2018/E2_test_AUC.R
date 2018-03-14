#### E2 ROC/AUC Curves and maximizing True Positive Rate
library(ROCR)
## need to stack the pred_results from wide to long so that Y column has at least two classes (win/loss)
pred <- prediction(pred_results$pred_binary.win, rep(1, nrow(pred_results)))
perf <- performance(pred,"tpr","fpr")