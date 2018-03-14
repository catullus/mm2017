## Models 1 - random Forests with opponent adjusted stats

notgitpath <- "C:/Users/cflagg/Documents/R_Projects/mm2017_local/"

rf5wOA <- readRDS(paste0(notgitpath, "rf5wOA_mod.rds"))

if (!("ranger5wOA" %in% ls())){
    rf5wOA <- randomForest(as.factor(win_loss.x) ~ ., data = na.exclude(dplyr::select(reg_opp_5w, -gameID, -team.x, -team.y,-Season.x, -Daynum.x, -score_diff.x, -score_diff.y, -score.x, -score.y)))
    
    
    rf5wOA_small <- randomForest(as.factor(win_loss.x) ~ team_rank.x + team_rank.y + ftm.pct.x + ftm.pct.y + or.pct.x + or.pct.y + fgm3.pct.x + fgm3.pct.y + fgm2.pct.x + fgm2.pct.y + poss.eff.wt.x + poss.eff.wt.y, 
                                 data = na.exclude(dplyr::select(reg_opp_5w, -gameID, -team.x, -team.y,-Season.x, -Daynum.x, -score_diff.x, -score_diff.y, -score.x, -score.y)), ncores = 8, parallel = TRUE)

    
    ## try ranger() function
    ranger5wOA <- ranger(as.factor(win_loss.x) ~ ., data = na.exclude(dplyr::select(reg_opp_5w, -gameID, -team.x, -team.y,-Season.x, -Daynum.x, -score_diff.x, -score_diff.y, -score.x, -score.y)), importance = 'impurity', probability = TRUE)
}

## for ranger(probability=TRUE) models
if (class(ranger5wOA) == "ranger"){
    training_results <- predict(ranger5wOA, data = na.exclude(reg_opp_5w))
    training_results <- data.frame(na.exclude(reg_opp_5w), training_results$predictions)
    training_results$training_binary <- ifelse(training_results$win > 0.5, "win", "loss")
    training_results$correct <- ifelse(training_results$training_binary == training_results$win_loss.x, "correct", "wrong")
}

data.frame(training_results$win_loss.x, training_results$win)

### analyze variable importance of ranger() output
ranger5wOA
rg_imp <- importance(ranger5wOA)

rg_imp <- data.frame(var=names(rg_imp), importance=rg_imp) %>% arrange(desc(importance))

saveRDS(rf5wOA, paste0(notgitpath, "rf5wOA_mod.rds"))
saveRDS(ranger5wOA, paste0(notgitpath, "ranger5wOA_mod.rds"))


