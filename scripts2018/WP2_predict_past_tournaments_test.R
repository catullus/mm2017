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
tourny <- read.csv(paste0(inpath, "data/TourneyCompactResults.csv"))
## filter down based on info from W1.R
# season_target is the seasons we are filtering to to reduce processing time
tourny <- dplyr::filter(tourny, Season %in% season_target)

tourny$w_uid <- paste0(tourny$Season, "_", tourny$Wteam) # winning team uid
tourny$l_uid <- paste0(tourny$Season, "_", tourny$Lteam) # losing team uid

## grab LAST GAME of each game in tourney_test by season
## for each season, for each team, slice the last record and return
pred_inputs <- reg_long_stats %>% group_by(Season, team) %>% slice(n()) %>% select(-gameID) %>% ungroup
pred_inputs$uid <- paste0(pred_inputs$Season,"_", pred_inputs$team) # input uid

winning_inputs <- dplyr::filter(pred_inputs, team %in% unique(tourny$Wteam)) %>% dplyr::select(-Season, -Daynum) ## need to add suffix ".x"
names(winning_inputs) <- paste0(names(winning_inputs),".x")
losing_inputs <- dplyr::filter(pred_inputs, team %in% unique(tourny$Lteam)) %>% dplyr::select(-Season, -Daynum) ## need to add suffix ".y"
names(losing_inputs) <- paste0(names(losing_inputs),".y")

# fill tourney data frame with stats from reg_long_laststat
# two runs
# add team X data to tourney data
rf_test_data <- merge(x = tourny, y = winning_inputs, by.x = "w_uid", by.y="uid.x", all.x=TRUE)
# add team Y data to tourney data
rf_test_data <- merge(x = rf_test_data, y = losing_inputs, by.x = "l_uid", by.y = "uid.y")

rf_test_data <- rf_test_data %>% select(-win_loss.y) # drop the win loss column, which is from the regular season NOT the tournament

#### check that test data names matches names used in model #### 
into_rf <- na.exclude(dplyr::select(reg_opp_5w, -gameID, -team.x, -team.y,-Season.x, -Daynum.x, -score_diff.x, -score_diff.y, -score.x, -score.y))
#pred_data <- dplyr::select(rf_test_data, names(into_rf))

data.frame(names(into_rf) %in% names(rf_test_data), names(into_rf))
data.frame(names(rf_test_data) %in% names(rf_test_data), names(rf_test_data))

#### check that test data COLUMN TYPES match types in use model, including factor levels ####
## character vectors must be converted to factors ##
sapply(reg_opp_5w, class)
sapply(rf_test_data, class)

#%>% select(-gameID, -Season.x, -Daynum.x, -loc.x, -win_loss.x)

## force integers into numeric
#pred_data <- rf_test_data %>% mutate_if(is.integer, as.numeric)
#sapply(pred_data, class)
# levels(pred_data$loc.x) <-levels(reg_opp_5w$loc.x)

## randomForest will puke if your factors do not have the same levels between test and training data
rf_test_data$loc.x <- as.factor(rf_test_data$loc.x)

# levels(reg_opp_5w$loc.x) ## input training data
# levels(pred_data$loc.x) ## input test data

levels(rf_test_data$loc.x) <-levels(reg_opp_5w$loc.x)

# compare output of predictions
predict(object = rf5wOA, newdata = rf_test_data)
pred_results <- data.frame(tourny, 
                           pred_binary = predict(object = rf5wOA, newdata = rf_test_data), 
                           pred_prob = predict(object = rf5wOA, newdata = rf_test_data, type = 'prob'))

# how many game outcomes did the model get right?
# every row in the tourny df is a win...and our model predicts the outcome of the ".x" team...
# thus the total number of predicted "wins" should equal the number of rows
pred_total <- table(pred_results$pred_binary)

paste0(round(pred_total[2]/sum(pred_total),2), " percent correct wins")
