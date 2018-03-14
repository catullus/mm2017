## Goal: 
# Mess with one regular season data and the previous_perf() function
# Avoid ranks
# Make sure function works with NCAA data
# Currently working on 2017 data so I avoid renaming 50+ columns (column name format changed in 2018)
library(plyr)
library(dplyr)
library(zoo)
library(stringr)
library(PlayerRatings)
library(car)
library(randomForest)
library(tidyr)
library(ranger)
library(caret)

## seasons being used for this and other data sets
season_target <- c(2010:2017)

#Open the datasets:
if (length(list.files("C:/Users/jroberti/Git/mm2017/data/")) > 0){
    inpath <- "C:/Users/jroberti/Git/mm2017/"
} else if (length(list.files("C:/Users/cflagg/Documents/GitHub/mm2017/data/")) > 0 ) {
    inpath <- "C:/Users/cflagg/Documents/GitHub/mm2017/"   
} else if (length(list.files("C:/Users/Amy/Documents/GitHub/mm2017/data/")) > 0) {
    inpath <- "C:/Users/Amy/Documents/GitHub/mm2017/"
}

source(paste0(inpath, "/scripts2018/function_previous_performance_NCAA.R"))
source(paste0(inpath, "/scripts2018/functions_2018.R"))

#### read detailed regular season results ####
reg <-read.csv(paste0(inpath, "data/RegularseasonDetailedResults.csv"), stringsAsFactors = FALSE, header = TRUE)
names(reg) <- tolower(names(reg))
## arrange by lowest wteam, as that is how kaggle wants the IDs arranged (lowest team first)
reg <- dplyr::arrange(reg, season, daynum, wteam, lteam)
team <- read.csv(paste0(inpath, "data/Teams.csv"), stringsAsFactors = FALSE)
seeds <- read.csv(paste0(inpath, "data/TourneySeeds.csv"), stringsAsFactors = FALSE)
seeds$uid <- paste0(seeds$season,"_",seeds$Team)
seeds$Seed_num <- as.integer(gsub("^w|^X|^Y|^Z|a|b","", seeds$Seed))


# create unique identifier for a game matchup e.g. "season_team1_team2"
reg$gameid <- paste0(reg$season, "_", reg$wteam, "_", reg$lteam)

## assign win/loss and total point diff
reg$wscore_diff <- reg$wscore - reg$lscore
reg$lscore_diff <- reg$lscore - reg$wscore

### give me one season for testing
reg <- dplyr::filter(reg, season %in% c(season_target)) ## comment out or modify to expand data set

### generate ranks for the filtered regular season data set
regrank <-  plyr::ddply(reg, .(season), function(x) {ranker(x)})

#create adjusted shooting stats:  (fg = 2 or 3 pointer...)  let's partition 2 and 3 pointers:
######### wINNING TEAM #############
#2-pointers attempted:
reg$wfga2<-reg$wfga-reg$wfga3
#2-pointers made:
reg$wfgm2<-reg$wfgm-reg$wfgm3
#2-point made %
reg$wfgm2.pct<-reg$wfgm2/reg$wfga2
#2/3 point ratio attempts
reg$wfga23.rat<-reg$wfga2/reg$wfga3

#ft made %
reg$wftm.pct<-reg$wftm/reg$wfta

#3-point made %
reg$wfgm3.pct<-reg$wfgm3/reg$wfga3

#create a score check to make sure the fgm2, fgm3, and ftm add up to respective score:  #check should be 0
w.score.check<-reg$wscore-((reg$wfgm2*2)+(reg$wfgm3*3)+reg$wftm)

######### lOSING TEAM ##############
#2-pointers attempted:
reg$lfga2<-reg$lfga-reg$lfga3
#2-pointers made:
reg$lfgm2<-reg$lfgm-reg$lfgm3
#2-point made %
reg$lfgm2.pct<-reg$lfgm2/reg$lfga2
#2/3 point ratio attempts
reg$lfga23.rat<-reg$lfga2/reg$lfga3

#ft made %
reg$lftm.pct<-reg$lftm/reg$lfta

#3-point made %
reg$lfgm3.pct<-reg$lfgm3/reg$lfga3

#teams' shooting percentages:
reg$wshoot.prct<-reg$wfgm2+reg$wfgm3+reg$wftm/reg$wfga2+reg$wfga3+reg$wfta
reg$lshoot.prct<-reg$lfgm2+reg$lfgm3+reg$lftm/reg$lfga2+reg$lfga3+reg$lfta

#teams' weighted shooting percentages:
reg$wshoot.prct.wt<-(2*reg$wfgm2)+(3*reg$wfgm3)+reg$wftm/reg$wfga2+reg$wfga3+reg$wfta
reg$lshoot.prct.wt<-(2*reg$lfgm2)+(3*reg$lfgm3)+reg$lftm/reg$lfga2+reg$lfga3+reg$lfta

#rebound prct
#offensive rbds attempts w team = defensive rbds attempts l team
reg$wor.a<-reg$wor+reg$ldr
reg$wor.pct<-reg$wor/reg$wor.a
reg$ldr.a<-reg$wor.a
reg$ldr.pct<-reg$ldr/reg$ldr.a

#defensive rbds attempts w team = offensive rbds attempts l team
reg$wdr.a<-reg$wdr+reg$lor
reg$wdr.pct<-reg$wdr/reg$wdr.a
reg$lor.a<-reg$wdr.a
reg$lor.pct<-reg$lor/reg$lor.a

#### numer of possessions:####
reg$wposs<-reg$wfga2+reg$wfga3+reg$wfta+reg$wto   #multiply the turnovers by the shooting % of l team to get a more accurate picture of how detrimental the turnovers are.  Also, on the other end, multiple the defensive rbs by the shooting percentage of w team to see how advantageous the defensive rbs are...
reg$wposs.action<-(reg$wfgm2+reg$wfgm3+reg$wftm-(reg$wto*(reg$lshoot.prct/100))+(reg$wdr*(reg$wshoot.prct/100)))
reg$wposs.eff<-reg$wposs.action/reg$wposs
#using weighted shooting %
reg$wposs.action.wt<-(reg$wfgm2+reg$wfgm3+reg$wftm-(reg$wto*(reg$lshoot.prct.wt/100))+(reg$wdr*(reg$wshoot.prct.wt/100)))
#possession efficiency:
reg$wposs.eff.wt<-reg$wposs.action.wt/reg$wposs

#losing team:
reg$lposs<-reg$lfga2+reg$lfga3+reg$lfta+reg$ldr+reg$lto
#non-weighted shooting stats:
reg$lposs.action<-(reg$lfgm2+reg$lfgm3+reg$lftm-(reg$lto*(reg$wshoot.prct/100))+(reg$ldr*(reg$lshoot.prct/100)))
reg$lposs.eff<-reg$lposs.action/reg$lposs
#weighted shooting stats:
reg$lposs.action.wt<-(reg$lfgm2+reg$lfgm3+reg$lftm-(reg$lto*(reg$wshoot.prct.wt/100))+(reg$ldr*(reg$lshoot.prct.wt/100)))
reg$lposs.eff.wt<-reg$lposs.action.wt/reg$lposs

### generate ranks for the filtered regular season data set
reg <-  plyr::ddply(reg, .(season), function(x) {ranker(x)})

reg <- dplyr::select(reg, -weekkey_loser, -weekkey_winner)

#### rename "w" and "l" columns ####
reg_winning_stats <-reg[,grep("^w.*|week|season|daynum|gameid|team_rank", names(reg))]
reg_losing_stats <-reg[,grep("^l.*|week|season|daynum|gameid|team_rank",names(reg))] ## location doesn't get picked up for this one

names(reg_winning_stats)<-gsub("^w","",names(reg_winning_stats)) #remove w from col name
reg_winning_stats <- dplyr::rename(reg_winning_stats, week=eek) %>% dplyr::select(-loc)
reg_winning_stats$win_loss <- "win"
names(reg_losing_stats)<-gsub("^l","",names(reg_losing_stats)) # remove l from col name
names(reg_losing_stats)<-gsub("^w","",names(reg_losing_stats)) # remove "w" from "wloc"
reg_losing_stats <- rename(reg_losing_stats, week=eek)
reg_losing_stats$win_loss <- "loss"

## check names matching
#data.frame(names(reg_winning_stats) %in% names(reg_losing_stats), names(reg_winning_stats))
#data.frame(names(reg_losing_stats) %in% names(reg_winning_stats), names(reg_losing_stats))

#### "Stack win/loss data into the long format"  ####
reg_long_stats <- rbind(reg_winning_stats, reg_losing_stats)
reg_long_stats <- arrange(reg_long_stats, team, daynum)

#### calculate how previous performance from last 5 games (width = 5) predicts outcome of games ####
## where _pp_ == "previous performance" 
reg_pp_5w <- previous_perf(data = reg_long_stats, grouper = c("season", "team"), arranger = "daynum", width = 5, func = mean, exclude = c("season", "daynum", "loc", "win_loss", "gameid", "score", "win", "week")) # leaving score out...because it''s confusing -- this is averaging their final score over the last 5 games

#### single team previous performance data ####
head(reg_pp_5w)
head(reg_long_stats)

## checking merge output... want the rows where team.x != team.y
##...as 4 rows are returned with data all criss crossed
## i.e. for the 4 rows returned, we want rows 2 and 3 (not 1 and 4)
# test <- filter(reg_pp_5w, gameid == "2017_1101_1149") %>% select(gameid, team, win_loss, fgm, fgm3, or)
# merge(x = test, y=  test, by.x = "gameid", by.y="gameid") # see why

#### opponent adjusted data set ####
### where "_opp_ == "opponent adjusted data"
reg_opp_5w <- merge(x = dplyr::filter(reg_pp_5w), 
                       y = dplyr::filter(reg_pp_5w), 
                       by.x = "gameid", 
                       by.y = "gameid")

## THIS STEP REMOVES DUPlICATE ROwS wITH ERRONEOUS DATA MATCHED UP
## IF YOU DON'T UNDESTAND THIS, lOOK AT THE object "TEST" ABOVE FOR RESUlTS OF THE MERGE
## Also removes rows with NA
reg_opp_5w <- dplyr::filter(reg_opp_5w, !is.na(team.x) & team.x != team.y) %>% select(-win_loss.y, -season.y, -daynum.y, -week.x, -week.y)
#reg_opp_5w$loc.x <- as.factor(reg_opp_5w$loc.x)

head(reg_opp_5w)

#### DO ONE MORE CHECK ON DATA OUTPUT ####
# filter(reg_pp_5w, gameid == "2017_1101_1270") %>% select(team, gameid, fgm, fgm3, ftm) ## this is data before join
# filter(reg_opp_5w, gameid == "2017_1101_1270") %>% select(team.x, gameid, fgm.x, fgm3.x, ftm.x, fgm.y, fgm3.y, ftm.y) ## it should equal the team.x stats in this data frame

