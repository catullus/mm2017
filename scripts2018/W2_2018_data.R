## W2 2018 data
## Goal: 
# Mess with one reg2018ular season data and the previous_perf() function
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

## seasons being used for this and other data sets
season_target18 <- c(2018)

#Open the datasets:
if (length(list.files("C:/Users/jroberti/Git/mm2017/data2018/")) > 0){
    inpath <- "C:/Users/jroberti/Git/mm2017/"
} else if (length(list.files("C:/Users/cflagg/Documents/GitHub/mm2017/data2018/")) > 0 ) {
    inpath <- "C:/Users/cflagg/Documents/GitHub/mm2017/"   
} else if (length(list.files("C:/Users/Amy/Documents/GitHub/mm2017/data2018/")) > 0) {
    inpath <- "C:/Users/Amy/Documents/GitHub/mm2017/"
}

source(paste0(inpath, "/scripts2018/function_previous_performance_NCAA.R"))
source(paste0(inpath, "/scripts2018/functions_2018.R"))

#### read detailed reg2018ular season results ####
reg2018 <-read.csv(paste0(inpath, "data2018/RegularSeasonDetailedResults.csv"), stringsAsFactors = FALSE, header = TRUE)
## arrange by lowest Wteam, as that is how kaggle wants the IDs arranged (lowest team first)
reg2018 <- dplyr::arrange(reg2018, Season, DayNum, WTeamID, LTeamID)
team2018 <- read.csv(paste0(inpath, "data2018/Teams.csv"), stringsAsFactors = FALSE)
seeds2018 <- read.csv(paste0(inpath, "data2018/NCAATourneySeeds.csv"), stringsAsFactors = FALSE)
seeds2018$uid <- paste0(seeds2018$Season,"_",seeds2018$Team)
seeds2018$Seed_num <- as.integer(gsub("^W|^X|^Y|^Z|a|b","", seeds2018$Seed))

# create unique identifier for a game matchup e.g. "season_team1_team2"
reg2018$gameID <- paste0(reg2018$Season, "_", reg2018$WTeamID, "_", reg2018$LTeamID)

## assign win/loss and total point diff
reg2018$Wscore_diff <- reg2018$WScore - reg2018$LScore
reg2018$Lscore_diff <- reg2018$LScore - reg2018$WScore

### give me one season for testing
reg2018 <- dplyr::filter(reg2018, Season %in% c(season_target18)) ## comment out or modify to expand data set

### generate ranks for the filtered reg2018ular season data set
reg2018rank <-  plyr::ddply(reg2018, .(Season), function(x) {ranker(x)})

#create adjusted shooting stats:  (fg = 2 or 3 pointer...)  let's partition 2 and 3 pointers:
######### WINNING TEAM #############
#2-pointers attempted:
reg2018$Wfga2<-reg2018$Wfga-reg2018$Wfga3
#2-pointers made:
reg2018$Wfgm2<-reg2018$Wfgm-reg2018$Wfgm3
#2-point made %
reg2018$Wfgm2.pct<-reg2018$Wfgm2/reg2018$Wfga2
#2/3 point ratio attempts
reg2018$Wfga23.rat<-reg2018$Wfga2/reg2018$Wfga3

#ft made %
reg2018$Wftm.pct<-reg2018$Wftm/reg2018$Wfta

#3-point made %
reg2018$Wfgm3.pct<-reg2018$Wfgm3/reg2018$Wfga3

#create a score check to make sure the fgm2, fgm3, and ftm add up to respective score:  #check should be 0
W.score.check<-reg2018$Wscore-((reg2018$Wfgm2*2)+(reg2018$Wfgm3*3)+reg2018$Wftm)

######### LOSING TEAM ##############
#2-pointers attempted:
reg2018$Lfga2<-reg2018$Lfga-reg2018$Lfga3
#2-pointers made:
reg2018$Lfgm2<-reg2018$Lfgm-reg2018$Lfgm3
#2-point made %
reg2018$Lfgm2.pct<-reg2018$Lfgm2/reg2018$Lfga2
#2/3 point ratio attempts
reg2018$Lfga23.rat<-reg2018$Lfga2/reg2018$Lfga3

#ft made %
reg2018$Lftm.pct<-reg2018$Lftm/reg2018$Lfta

#3-point made %
reg2018$Lfgm3.pct<-reg2018$Lfgm3/reg2018$Lfga3

#teams' shooting percentages:
reg2018$Wshoot.prct<-reg2018$Wfgm2+reg2018$Wfgm3+reg2018$Wftm/reg2018$Wfga2+reg2018$Wfga3+reg2018$Wfta
reg2018$Lshoot.prct<-reg2018$Lfgm2+reg2018$Lfgm3+reg2018$Lftm/reg2018$Lfga2+reg2018$Lfga3+reg2018$Lfta

#teams' weighted shooting percentages:
reg2018$Wshoot.prct.wt<-(2*reg2018$Wfgm2)+(3*reg2018$Wfgm3)+reg2018$Wftm/reg2018$Wfga2+reg2018$Wfga3+reg2018$Wfta
reg2018$Lshoot.prct.wt<-(2*reg2018$Lfgm2)+(3*reg2018$Lfgm3)+reg2018$Lftm/reg2018$Lfga2+reg2018$Lfga3+reg2018$Lfta

#rebound prct
#offensive rbds attempts W team = defensive rbds attempts L team
reg2018$Wor.a<-reg2018$Wor+reg2018$Ldr
reg2018$Wor.pct<-reg2018$Wor/reg2018$Wor.a
reg2018$Ldr.a<-reg2018$Wor.a
reg2018$Ldr.pct<-reg2018$Ldr/reg2018$Ldr.a

#defensive rbds attempts W team = offensive rbds attempts L team
reg2018$Wdr.a<-reg2018$Wdr+reg2018$Lor
reg2018$Wdr.pct<-reg2018$Wdr/reg2018$Wdr.a
reg2018$Lor.a<-reg2018$Wdr.a
reg2018$Lor.pct<-reg2018$Lor/reg2018$Lor.a

#numer of possessions:
reg2018$Wposs<-reg2018$Wfga2+reg2018$Wfga3+reg2018$Wfta+reg2018$Wto   #multiply the turnovers by the shooting % of L team to get a more accurate picture of how detrimental the turnovers are.  Also, on the other end, multiple the defensive rbs by the shooting percentage of W team to see how advantageous the defensive rbs are...
reg2018$Wposs.action<-(reg2018$Wfgm2+reg2018$Wfgm3+reg2018$Wftm-(reg2018$Wto*(reg2018$Lshoot.prct/100))+(reg2018$Wdr*(reg2018$Wshoot.prct/100)))
reg2018$Wposs.eff<-reg2018$Wposs.action/reg2018$Wposs
#using weighted shooting %
reg2018$Wposs.action.wt<-(reg2018$Wfgm2+reg2018$Wfgm3+reg2018$Wftm-(reg2018$Wto*(reg2018$Lshoot.prct.wt/100))+(reg2018$Wdr*(reg2018$Wshoot.prct.wt/100)))
#possession efficiency:
reg2018$Wposs.eff.wt<-reg2018$Wposs.action.wt/reg2018$Wposs
#losing team:
reg2018$Lposs<-reg2018$Lfga2+reg2018$Lfga3+reg2018$Lfta+reg2018$Ldr+reg2018$Lto
#non-weighted shooting stats:
reg2018$Lposs.action<-(reg2018$Lfgm2+reg2018$Lfgm3+reg2018$Lftm-(reg2018$Lto*(reg2018$Wshoot.prct/100))+(reg2018$Ldr*(reg2018$Lshoot.prct/100)))
reg2018$Lposs.eff<-reg2018$Lposs.action/reg2018$Lposs
#weighted shooting stats:
reg2018$Lposs.action.wt<-(reg2018$Lfgm2+reg2018$Lfgm3+reg2018$Lftm-(reg2018$Lto*(reg2018$Wshoot.prct.wt/100))+(reg2018$Ldr*(reg2018$Lshoot.prct.wt/100)))
reg2018$Lposs.eff.wt<-reg2018$Lposs.action.wt/reg2018$Lposs

### generate ranks for the filtered reg2018ular season data set
reg2018 <-  plyr::ddply(reg2018, .(Season), function(x) {ranker(x)})

#### rename "W" and "L" columns ####
reg2018_winning_stats <-reg2018[,grep("W.*|Week|Season|Daynum|gameID", names(reg2018))]
reg2018_winning_stats$win_loss <- "win"
reg2018_losing_stats <-reg2018[,grep("L.*|Week|Season|Daynum|Wloc|gameID",names(reg2018))] ## location doesn't get picked up for this one
reg2018_losing_stats$win_loss <- "loss"

names(reg2018_winning_stats)<-gsub("^W","",names(reg2018_winning_stats)) #remove W from col name
names(reg2018_losing_stats)<-gsub("^L","",names(reg2018_losing_stats)) # remove L from col name
names(reg2018_losing_stats)<-gsub("^W","",names(reg2018_losing_stats)) # remove "W" from "Wloc"

#### "Stack win/loss data into the long format"  ####
reg2018_long_stats <- rbind(reg2018_winning_stats, reg2018_losing_stats)
reg2018_long_stats <- arrange(reg2018_long_stats, team, Daynum)

#### calculate how previous performance from last 5 games (width = 5) predicts outcome of games ####
## where _pp_ == "previous performance" 
reg2018_pp_5w <- previous_perf(data = reg2018_long_stats, grouper = c("Season", "team"), arranger = "Daynum", width = 5, func = mean, exclude = c("Season", "Daynum", "loc", "win_loss", "gameID", "score")) # leaving score out...because it''s confusing -- this is averaging their final score over the last 5 games

#### single team previous performance data ####
head(reg2018_pp_5w)
head(reg2018_long_stats)

## checking merge output... want the rows where team.x != team.y
##...as 4 rows are returned with data all criss crossed
## i.e. for the 4 rows returned, we want rows 2 and 3 (not 1 and 4)
# test <- filter(reg2018_pp_5w, gameID == "2017_1101_1149") %>% select(gameID, team, win_loss, fgm, fgm3, or)
# merge(x = test, y=  test, by.x = "gameID", by.y="gameID") # see why

#### opponent adjusted data set ####
### where "_opp_ == "opponent adjusted data"
reg2018_opp_5w <- merge(x = dplyr::filter(reg2018_pp_5w), 
                    y = dplyr::filter(reg2018_pp_5w), 
                    by.x = "gameID", 
                    by.y = "gameID")

## THIS STEP REMOVES DUPLICATE ROWS WITH ERRONEOUS DATA MATCHED UP
## IF YOU DON'T UNDESTAND THIS, LOOK AT THE object "TEST" ABOVE FOR RESULTS OF THE MERGE
## Also removes rows with NA
reg2018_opp_5w <- dplyr::filter(reg2018_opp_5w, !is.na(team.x) & team.x != team.y) %>% select(-win_loss.y, -Season.y, -Daynum.y, -loc.y, -eek.x, -eek.y)
reg2018_opp_5w$loc.x <- as.factor(reg2018_opp_5w$loc.x)

head(reg2018_opp_5w)

#### DO ONE MORE CHECK ON DATA OUTPUT ####
# filter(reg2018_pp_5w, gameID == "2017_1101_1270") %>% select(team, gameID, fgm, fgm3, ftm) ## this is data before join
# filter(reg2018_opp_5w, gameID == "2017_1101_1270") %>% select(team.x, gameID, fgm.x, fgm3.x, ftm.x, fgm.y, fgm3.y, ftm.y) ## it should equal the team.x stats in this data frame

