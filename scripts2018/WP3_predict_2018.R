#### WP3_predict_2018

# That is their last 5 game average before getting into the tournament
# output data = reg_long_laststat
#Open the datasets:
if (length(list.files("C:/Users/jroberti/Git/mm2017/data2018/")) > 0){
    inpath <- "C:/Users/jroberti/Git/mm2017/"
} else if (length(list.files("C:/Users/cflagg/Documents/GitHub/mm2017/data2018/")) > 0 ) {
    inpath <- "C:/Users/cflagg/Documents/GitHub/mm2017/"   
} else if (length(list.files("C:/Users/Amy/Documents/GitHub/mm2017/data2018/")) > 0) {
    inpath <- "C:/Users/Amy/Documents/GitHub/mm2017/"
}

seeds2018 <- read.csv(paste0(inpath, "data2018/NCAATourneySeeds.csv"), stringsAsFactors=FALSE)
seeds2018 <- dplyr::filter(seeds2018, Season == 2018) %>% arrange(TeamID)

teamNames <- read.csv(paste0(inpath, "data2018/Teams.csv"), stringsAsFactors=FALSE)

teamNames <- dplyr::select(teamNames, TeamID, TeamName)

#### put together all unique matchups
# stolen from: https://stackoverflow.com/questions/17171148/non-redundant-version-of-expand-grid
expand.grid.unique <- function(x, y, include.equals=FALSE)
{
    x <- unique(x)
    
    y <- unique(y)
    
    g <- function(i)
    {
        z <- setdiff(y, x[seq_len(i-include.equals)])
        
        if(length(z)) cbind(x[i], z, deparse.level=0)
    }
    
    do.call(rbind, lapply(seq_along(x), g))
}

all_matchups <- expand.grid.unique(seeds2018$TeamID, seeds2018$TeamID)
all_matchups <- as.data.frame(all_matchups)
names(all_matchups) <- c("team_x", "team_y")

### create unique ID
all_matchups$ID <- paste0("2018_", all_matchups$team_x, "_", all_matchups$team_y)

### make a vector to filter Regular season data
tourny_teams <- unique(c(unique(all_matchups$team_x), unique(all_matchups$team_y)))

### filter out teams needed
submission_data <- reg2018_pp_5w %>% filter(team %in% tourny_teams) %>% arrange(desc(daynum)) %>% group_by(team) %>%  slice(n()) 
submission_data$team_rank.1 <- NULL

## "1124" is missing from team_x?

#### NOT FINDING THIS ID #####
#"2018_1104_1158" %in% all_matchups$ID

#### MERGE 2018 SEASON data with 2018 matchups ####
test <- merge(x = all_matchups, y = submission_data, by.x = "team_x", by.y = "team")
#write.csv(test, "test_preds_missing.csv", row.names=FALSE)
test2 <- merge(x = test, y = submission_data, by.x = "team_y", by.y = "team")

#### predict on new unseen matchups ####
preds_2018 <- predict(ranger5wOA, data = test2)

head(preds_2018)

test2$prediction.loss <- preds_2018$predictions[,1] 
test2$prediction.win <- preds_2018$predictions[,2]

write.csv(dplyr::select(test2, ID, prediction.win) %>% rename(Pred=prediction.win), paste0(inpath, "submissions/teddyt_ranger5wOA_toKag.csv"), row.names=FALSE)

write.csv(all_matchups, "all_tournament_matchups_2018.csv", row.names=FALSE)

#### for my brackets #### 
bracket_data <- test2

bracket_data2 <- merge(bracket_data, teamNames, by.x = "team_y", by.y = "TeamID")
bracket_data2 <- merge(bracket_data2, teamNames, by.x = "team_x", by.y = "TeamID")

write.csv(bracket_data2, paste0(inpath, "submissions/ranger5wOA_bracket_results.csv"), row.names=FALSE)
