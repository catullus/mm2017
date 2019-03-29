## basic_possession analysis
## S1 2018 

##### RANK TEAMS ####
library(plyr)
library(dplyr)
library(stringr)
library(PlayerRatings)
library(car)
library(randomForest)
library(tidyr)
# library(gbm)

source("~/Documents/GitHub/mm2017/2018/scripts2018/functions_2018.R")
source("~/Documents/GitHub/mm2017/2018/scripts2018/function_previous_performance_NCAA.R")

##### READ RAW DATA #####
inpath <- "C:/Users/Amy/Documents/GitHub/mm2017/2018/data2018/"

reg_box <- data.table::fread(paste0(inpath, "RegularSeasonDetailedResults.csv"), stringsAsFactors = FALSE)

# assign a week value
reg_box$week <- sapply(reg_box$DayNum, assign_week, week_idx)

## basic stats
reg_box$W_score_diff <- reg_box$WScore - reg_box$LScore
reg_box$L_score_diff <- reg_box$LScore - reg_box$WScore

## Calculate basic possession
# basic_poss = (fga2 + fga3 + turnovers + 0.44*free_throws) - offensive_rebounds
# source = https://www.nbastuffer.com/analytics101/possession/
reg_box$W_off_poss <- (reg_box$WFGA + reg_box$WFGA3 + (reg_box$WFTA*0.44)) - reg_box$WOR
reg_box$L_off_poss <- (reg_box$LFGA + reg_box$LFGA3 + (reg_box$LFTA*0.44)) - reg_box$LOR
reg_box$W_def_poss <- reg_box$L_off_poss
reg_box$L_def_poss <- reg_box$W_off_poss

## calculate off efficiency as points per possession
reg_box$W_eff <- reg_box$WScore / reg_box$W_off_poss 
reg_box$L_eff <- reg_box$LScore / reg_box$L_off_poss

## defensive efficiency - points allowed per opponent possession
reg_box$W_deff <- reg_box$LScore / reg_box$L_off_poss
reg_box$L_deff <- reg_box$WScore / reg_box$W_off_poss

##### CALCULATE DIFFERENCES (before splitting data) ##### 

## transform from wide to long data structure
head(reg_box)


##### test the split and join ##### 
test <- filter(reg_box, Season == 2003)
Wtest <- dplyr::select(test, starts_with("W"), -WLoc, Season)
Ltest <- dplyr::select(reg_box, starts_with("L"), week, Season)
names(Wtest) <- gsub(x = names(Wtest), pattern = "^W_|^W", replacement = "")
names(Ltest) <- gsub(x = names(Ltest), pattern = "^L_|^L", replacement = "")

head(Wtest)







##### SPLIT DATA -- win and loss data frames #####
## select data for winning and losing teams 
w_regbox <- dplyr::select(reg_box, starts_with("W"), -WLoc, Season)
w_regbox$outcome <- "win"
# teamid_week_season
w_regbox$season_key <- paste0(w_regbox$WTeamID, "_", w_regbox$week, "_", w_regbox$Season)
l_regbox <- dplyr::select(reg_box, starts_with("L"), week, Season)
l_regbox$outcome <- "loss"
l_regbox$season_key <- paste0(l_regbox$WTeamID, "_", l_regbox$week, "_", l_regbox$Season)

## rename columns so they align
names(w_regbox) <- gsub(x = names(w_regbox), pattern = "^W_|^W", replacement = "")
names(l_regbox) <- gsub(x = names(l_regbox), pattern = "^L_|^L", replacement = "")

## rowbind data
regbox_int <- rbind(w_regbox, l_regbox)

# match season_keys
table(regbox_int$season_key %in% regrank_int$season_key)

## combine data
comb_df <- merge(x = regbox_int, y = regrank_int, by.x='season_key', by.y='season_key')
comb_df$outcome_num <- ifelse(comb_df$outcome == "win", 1, 0)

regbox_int$outcome_num <- ifelse(regbox_int$outcome == 'win', 1, 0)
lr1 <- glm(outcome_num ~ eff + deff + off_poss + def_poss, data = regbox_int, family = binomial,  control = list(maxit = 50))
