## Models 1 - random Forests with opponent adjusted stats

notgitpath <- "C:/Users/cflagg/Documents/R_Projects/mm2017_local/"

rf5wOA <- readRDS(paste0(notgitpath, "rf5wOA_mod.rds"))

## load simple tourny outcomes
tourny <- read.csv(paste0(inpath, "data/TourneyCompactResults.csv"))
## filter down based on info from W1.R
# season_target is the seasons we are filtering to to reduce processing time
tourny <- dplyr::filter(tourny, Season %in% season_target)

tourny$w_uid <- paste0(tourny$Season, "_", tourny$Wteam) # winning team uid
tourny$l_uid <- paste0(tourny$Season, "_", tourny$Lteam) # losing team uid

teams_in_tourny <- c(tourny$Wteam, tourny$Lteam)
teams_in_tourny <- unique(teams_in_tourny)


### reduce the data set
smaller <- na.exclude(dplyr::filter(reg_opp_5w, team.x %in% teams_in_tourny | team.y %in% teams_in_tourny) %>% dplyr::select(-gameid, -team.x, -team.y,-season.x, -daynum.x, -score_diff.x, -score_diff.y, -score.x, -score.y))

#if (!("ranger5wOA" %in% ls())){
    # rf5wOA <- randomForest(as.factor(win_loss.x) ~ ., data = smaller, ncores = 8, parallel = TRUE)

### caret usage
trControl = trainControl(method = "cv", number = 10, allowParallel = TRUE, verboseIter = FALSE)
st <- Sys.time()
rfCaret5wOA <- train(as.factor(win_loss.x) ~ ., data = smaller, method = "rf", prox = FALSE, trControl = trControl)
ed <- Sys.time()

#### mini randomforest with top 12 predictors
rf5wOA_small <- randomForest(as.factor(win_loss.x) ~ team_rank.x + team_rank.y + ftm.pct.x + ftm.pct.y + or.pct.x + or.pct.y + fgm3.pct.x + fgm3.pct.y + fgm2.pct.x + fgm2.pct.y + poss.eff.wt.x + poss.eff.wt.y, 
                                 data = smaller, ncores = 8, parallel = TRUE)

## try ranger() function
ranger5wOA <- ranger(as.factor(win_loss.x) ~ ., data = smaller, importance = 'impurity', probability = TRUE)
#}


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


