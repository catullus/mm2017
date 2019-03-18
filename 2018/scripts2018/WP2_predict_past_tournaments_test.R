## WP2: wrangle data
## Use data from W1 as inputs to M1 models...
## need to munge data first

# input data = reg_long_stats
# take the LAST GAME for a team %IN% tournament X in same season
# That is their last 5 game average before getting into the tournament
# output data = reg_long_laststat
#Open the datasets:
if (length(list.files("C:/Users/jroberti/Git/mm2017/data/")) > 0){
    inpath <- "C:/Users/jroberti/Git/mm2017/"
} else if (length(list.files("C:/Users/cflagg/Documents/GitHub/mm2017/data/")) > 0 ) {
    inpath <- "C:/Users/cflagg/Documents/GitHub/mm2017/"   
} else if (length(list.files("C:/Users/Amy/Documents/GitHub/mm2017/data/")) > 0) {
    inpath <- "C:/Users/Amy/Documents/GitHub/mm2017/"
}

## load simple tourny outcomes

## grab LAST GAME of each game in tourney_test by season
## for each season, for each team, slice the last record and return
pred_inputs <- reg_long_stats %>% group_by(season, team) %>% slice(n()) %>% select(-gameid) %>% ungroup
pred_inputs$uid <- paste0(pred_inputs$season,"_", pred_inputs$team) # input uid

winning_inputs <- dplyr::filter(pred_inputs, team %in% unique(tourny$Wteam)) %>% dplyr::select(-season, -daynum) ## need to add suffix ".x"
names(winning_inputs) <- paste0(names(winning_inputs),".x")
losing_inputs <- dplyr::filter(pred_inputs, team %in% unique(tourny$Lteam)) %>% dplyr::select(-season, -daynum) ## need to add suffix ".y"
names(losing_inputs) <- paste0(names(losing_inputs),".y")

# fill tourney data frame with stats from reg_long_laststat
# two runs
# add team X data to tourney data
rf_test_data <- merge(x = tourny, y = winning_inputs, by.x = "w_uid", by.y="uid.x", all.x=TRUE)
# add team Y data to tourney data
rf_test_data <- merge(x = rf_test_data, y = losing_inputs, by.x = "l_uid", by.y = "uid.y")

rf_test_data <- rf_test_data %>% select(-win_loss.y) # drop the win loss column, which is from the regular season NOT the tournament
rf_test_data$loc.x <- as.factor(rf_test_data$loc.x)

## factors need same number of levels in test data
# levels(rf_test_data$loc.x) <-levels(reg_opp_5w$loc.x)

## randomForest will puke if your factors do not have the same levels between test and training data

# compare output of predictions
pred_results <- data.frame(tourny, 
                            pred_binary = predict(object = rf5wOA, newdata = rf_test_data), 
                            pred_prob = predict(object = rf5wOA, newdata = rf_test_data, type = 'prob'))
table(pred_results$pred_binary)[2]/nrow(pred_results)
#pred_results <- data.frame(tourny, 
 #                          pred_binary = predict(ranger5wOA, data = rf_test_data)$predictions)
#pred_results$pred_binary <- ifelse(pred_results$pred_binary.win >0.5, "win", "loss")
pred_results_rfsmall <- data.frame(tourny, 
                           pred_binary = predict(object = rf5wOA_small, newdata = rf_test_data), 
                           pred_prob = predict(object = rf5wOA_small, newdata = rf_test_data, type = 'prob'))

table(pred_results_rfsmall$pred_binary)[2]/nrow(pred_results_rfsmall)
# how many game outcomes did the model get right?
# every row in the tourny df is a win...and our model predicts the outcome of the ".x" team...
# thus the total number of predicted "wins" should equal the number of rows
pred_results <- filter(pred_results, WSeed_num != 16)
pred_total <- table(pred_results$pred_binary)

paste0(round(pred_total[2]/sum(pred_total),2), " percent correct wins")

write.csv(pred_results, paste0(inpath,'test_results/random_forest_test_results_rf5wOA.csv'), row.names=FALSE)
write.csv(pred_results, paste0(inpath,'test_results/random_forest_test_results_ranger5wOA.csv'), row.names=FALSE)
