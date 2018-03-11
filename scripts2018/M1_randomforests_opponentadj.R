## Models 1 - random Forests with opponent adjusted stats
rf5wOA <- randomForest(as.factor(win_loss.x) ~ ., data = na.exclude(dplyr::select(reg_opp_5w, -gameID, -team.x, -team.y,-Season.x, -Daynum.x, -score_diff.x, -score_diff.y, -score.x, -score.y)))

rf5wOA
varImpPlot(rf5wOA)

