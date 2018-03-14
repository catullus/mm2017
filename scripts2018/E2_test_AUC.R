#### E2 ROC/AUC Curves and maximizing True Positive Rate
library(ROCR)
## need to stack the pred_results from wide to long so that Y column has at least two classes (win/loss)
pred <- prediction(pred_results$pred_binary.win, rep(1, nrow(pred_results)))
perf <- performance(pred,"tpr","fpr")


maximize_thresh <- function(actual, prob, cutoff){
    #browser()
    ## add binary win_loss based on cutoff
    data = data.frame(actual=actual, prob=prob)
    data$pred_win = ifelse(data$prob > cutoff, "win", "loss")
    ## how many were correct?
    data$correct = ifelse(data$actual == data$pred_win, "correct", "wrong")
    ## summarize number correct
    accuracy = table(data$correct)
    ## return cutoff val and % correct
    accuracy_stats = list(data.frame(percent_correct = accuracy["correct"]/nrow(data), cutoff_val = cutoff))
    return(accuracy_stats)
}

#### data from M1.R training_results object (ranger random forest)
#maximize_thresh(actual = training_results$win_loss.x, prob = training_results$win, cutoff = 0.55)
lapply(seq(30, 75, by = 5)/100, FUN = maximize_thresh, actual = training_results$win_loss.x, prob = training_results$win)
