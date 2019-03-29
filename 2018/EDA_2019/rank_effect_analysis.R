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

reg <- read.csv(paste0(inpath, "RegularSeasonCompactResults.csv"), stringsAsFactors = FALSE)
#reg <- filter(reg, Season %in% c(2002:2013))

team <- read.csv(paste0(inpath, "Teams.csv"), stringsAsFactors = FALSE)
seasons <- read.csv(paste0(inpath, "Seasons.csv"), stringsAsFactors = FALSE)

tourney <- read.csv(paste0(inpath, "NCAATourneyCompactResults.csv"), stringsAsFactors = FALSE)
#tourney <- filter(tourney, Season %in% c(2002:2013))

seeds <- read.csv(paste0(inpath, "NCAATourneySeeds.csv"), stringsAsFactors = FALSE)
seed <- filter(seeds, Season %in% c(2002:2018))
seed$key <- paste0(seed$Season,"_",seed$Team)
seed$seedval <- as.numeric(str_extract(seed$Seed, "[0-9]{1,2}"))

seed_loser <- seed
names(seed_loser) <- paste0("L_", names(seed_loser))
seed_winner <- seed
names(seed_winner) <- paste0("W_", names(seed_winner))

reg$wdiff <- reg$WScore - reg$LScore
reg$ldiff <- reg$LScore - reg$WScore

#### RANK TEAMS BY ranker() FUNCTION ####
#start <- Sys.time()
# debug(ranker)
regrank <-  plyr::ddply(reg, .(Season), function(x) {ranker(x)})
#end <- Sys.time()
#end-start

## Explore: Winning_Team_Rank / Losing_Team_Rank ~ Score Differential per game

## WEIGHT ALG
#head(regrank)
regrank$win_weight <- (regrank$LTeamID_rank/regrank$WTeamID_rank)*1 #1
regrank$loss_weight <- (regrank$LTeamID_rank/regrank$WTeamID_rank)*1 #1

regrank$rank_diff <- regrank$WTeamID_rank - regrank$LTeamID_rank

regrank$Wseason_key <- paste0(regrank$WTeamID, "_", regrank$week, "_", regrank$Season)
regrank$Lseason_key <- paste0(regrank$LTeamID, "_", regrank$week, "_", regrank$Season)
## does the difference in ranks, location, and Season explain the winning point margin?
#m1 <- lm(wdiff ~ rank_diff + WLoc + Season, data = regrank)
#summary(m1)
# 0.196% of variation

## spread of ranks throughout season
#hist(c(regrank$WTeamID_rank, regrank$LTeamID_rank))

w_regrank <- dplyr::select(regrank, Wseason_key, WTeamID_rank)
w_regrank$winloss <- "win"
l_regrank <- dplyr::select(regrank, Lseason_key, LTeamID_rank)
l_regrank$winloss <- "loss"

names(w_regrank) <- gsub(x = names(w_regrank), pattern = "^W", replacement = "")
names(l_regrank) <- gsub(x = names(l_regrank), pattern = "^L", replacement = "")

regrank_int <- rbind(w_regrank, l_regrank)
