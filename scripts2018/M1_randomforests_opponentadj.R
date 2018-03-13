## Models 1 - random Forests with opponent adjusted stats

notgitpath <- "C:/Users/cflagg/Documents/R_Projects/mm2017_local/"

rf5wOA <- readRDS(paste0(notgitpath, "rf5wOA_mod.rds"))

if (!("ranger5wOA" %in% ls())){
    #rf5wOA <- randomForest(as.factor(win_loss.x) ~ ., data = na.exclude(dplyr::select(reg_opp_5w, -gameID, -team.x, -team.y,-Season.x, -Daynum.x, -score_diff.x, -score_diff.y, -score.x, -score.y)))
    
    #rf5wOA
    #varImpPlot(rf5wOA)
    
    ## try ranger() function
    ranger5wOA <- ranger(as.factor(win_loss.x) ~ ., data = na.exclude(dplyr::select(reg_opp_5w, -gameID, -team.x, -team.y,-Season.x, -Daynum.x, -score_diff.x, -score_diff.y, -score.x, -score.y)), importance = 'impurity')
}

### analyze variable importance of ranger() output
ranger5wOA
rg_imp <- importance(ranger5wOA)

rg_imp <- data.frame(var=names(rg_imp), importance=rg_imp)

saveRDS(rf5wOA, paste0(notgitpath, "rf5wOA_mod.rds"))
saveRDS(ranger5wOA, paste0(notgitpath, "ranger5wOA_mod.rds"))
