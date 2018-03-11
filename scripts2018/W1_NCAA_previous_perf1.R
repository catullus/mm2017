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

#Open the datasets:
if (length(list.files("C:/Users/jroberti/Git/mm2017/data/")) > 0){
    inpath <- "C:/Users/jroberti/Git/mm2017/"
} else if (length(list.files("C:/Users/cflagg/Documents/GitHub/mm2017/data/")) > 0 ) {
    inpath <- "C:/Users/cflagg/Documents/GitHub/mm2017/"   
} else if (length(list.files("C:/Users/Amy/Documents/GitHub/mm2017/data/")) > 0) {
    inpath <- "C:/Users/Amy/Documents/GitHub/mm2017/"
}

source(paste0(inpath, "/scripts2018/function_previous_performance_NCAA.R"))

#### read detailed regular season results ####
reg <-read.csv(paste0(inpath, "data/RegularSeasonDetailedResults.csv"), stringsAsFactors = FALSE, header = TRUE)
## arrange by lowest Wteam, as that is how kaggle wants the IDs arranged (lowest team first)
reg <- dplyr::arrange(reg, Season, Daynum, Wteam, Lteam)
team <- read.csv(paste0(inpath, "data/Teams.csv"), stringsAsFactors = FALSE)

# create unique identifier for a game matchup e.g. "season_team1_team2"
reg$gameID <- paste0(reg$Season, "_", reg$Wteam, "_", reg$Lteam)

## assign win/loss and total point diff
reg$Wscore_diff <- reg$Wscore - reg$Lscore
reg$Lscore_diff <- reg$Lscore - reg$Wscore

### give me one season for testing
reg <- dplyr::filter(reg, Season %in% c(2014, 2015, 2016, 2017)) ## comment out or modify to expand data set

#create adjusted shooting stats:  (fg = 2 or 3 pointer...)  let's partition 2 and 3 pointers:
######### WINNING TEAM #############
#2-pointers attempted:
reg$Wfga2<-reg$Wfga-reg$Wfga3
#2-pointers made:
reg$Wfgm2<-reg$Wfgm-reg$Wfgm3
#2-point made %
reg$Wfgm2.pct<-reg$Wfgm2/reg$Wfga2
#2/3 point ratio attempts
reg$Wfga23.rat<-reg$Wfga2/reg$Wfga3

#ft made %
reg$Wftm.pct<-reg$Wftm/reg$Wfta

#3-point made %
reg$Wfgm3.pct<-reg$Wfgm3/reg$Wfga3

#create a score check to make sure the fgm2, fgm3, and ftm add up to respective score:  #check should be 0
W.score.check<-reg$Wscore-((reg$Wfgm2*2)+(reg$Wfgm3*3)+reg$Wftm)

######### LOSING TEAM ##############
#2-pointers attempted:
reg$Lfga2<-reg$Lfga-reg$Lfga3
#2-pointers made:
reg$Lfgm2<-reg$Lfgm-reg$Lfgm3
#2-point made %
reg$Lfgm2.pct<-reg$Lfgm2/reg$Lfga2
#2/3 point ratio attempts
reg$Lfga23.rat<-reg$Lfga2/reg$Lfga3

#ft made %
reg$Lftm.pct<-reg$Lftm/reg$Lfta

#3-point made %
reg$Lfgm3.pct<-reg$Lfgm3/reg$Lfga3

#teams' shooting percentages:
reg$Wshoot.prct<-reg$Wfgm2+reg$Wfgm3+reg$Wftm/reg$Wfga2+reg$Wfga3+reg$Wfta
reg$Lshoot.prct<-reg$Lfgm2+reg$Lfgm3+reg$Lftm/reg$Lfga2+reg$Lfga3+reg$Lfta

#teams' weighted shooting percentages:
reg$Wshoot.prct.wt<-(2*reg$Wfgm2)+(3*reg$Wfgm3)+reg$Wftm/reg$Wfga2+reg$Wfga3+reg$Wfta
reg$Lshoot.prct.wt<-(2*reg$Lfgm2)+(3*reg$Lfgm3)+reg$Lftm/reg$Lfga2+reg$Lfga3+reg$Lfta

#rebound prct
#offensive rbds attempts W team = defensive rbds attempts L team
reg$Wor.a<-reg$Wor+reg$Ldr
reg$Wor.pct<-reg$Wor/reg$Wor.a
reg$Ldr.a<-reg$Wor.a
reg$Ldr.pct<-reg$Ldr/reg$Ldr.a

#defensive rbds attempts W team = offensive rbds attempts L team
reg$Wdr.a<-reg$Wdr+reg$Lor
reg$Wdr.pct<-reg$Wdr/reg$Wdr.a
reg$Lor.a<-reg$Wdr.a
reg$Lor.pct<-reg$Lor/reg$Lor.a

#numer of possessions:
reg$Wposs<-reg$Wfga2+reg$Wfga3+reg$Wfta+reg$Wto   #multiply the turnovers by the shooting % of L team to get a more accurate picture of how detrimental the turnovers are.  Also, on the other end, multiple the defensive rbs by the shooting percentage of W team to see how advantageous the defensive rbs are...
reg$Wposs.action<-(reg$Wfgm2+reg$Wfgm3+reg$Wftm-(reg$Wto*(reg$Lshoot.prct/100))+(reg$Wdr*(reg$Wshoot.prct/100)))
reg$Wposs.eff<-reg$Wposs.action/reg$Wposs
#using weighted shooting %
reg$Wposs.action.wt<-(reg$Wfgm2+reg$Wfgm3+reg$Wftm-(reg$Wto*(reg$Lshoot.prct.wt/100))+(reg$Wdr*(reg$Wshoot.prct.wt/100)))
#possession efficiency:
reg$Wposs.eff.wt<-reg$Wposs.action.wt/reg$Wposs
#losing team:
reg$Lposs<-reg$Lfga2+reg$Lfga3+reg$Lfta+reg$Ldr+reg$Lto
#non-weighted shooting stats:
reg$Lposs.action<-(reg$Lfgm2+reg$Lfgm3+reg$Lftm-(reg$Lto*(reg$Wshoot.prct/100))+(reg$Ldr*(reg$Lshoot.prct/100)))
reg$Lposs.eff<-reg$Lposs.action/reg$Lposs
#weighted shooting stats:
reg$Lposs.action.wt<-(reg$Lfgm2+reg$Lfgm3+reg$Lftm-(reg$Lto*(reg$Wshoot.prct.wt/100))+(reg$Ldr*(reg$Lshoot.prct.wt/100)))
reg$Lposs.eff.wt<-reg$Lposs.action.wt/reg$Lposs

#### rename "W" and "L" columns ####
reg_winning_stats <-reg[,grep("W.*|Week|Season|Daynum|gameID", names(reg))]
reg_winning_stats$win_loss <- "win"
reg_losing_stats <-reg[,grep("L.*|Week|Season|Daynum|Wloc|gameID",names(reg))] ## location doesn't get picked up for this one
reg_losing_stats$win_loss <- "loss"

names(reg_winning_stats)<-gsub("^W","",names(reg_winning_stats)) #remove W from col name
names(reg_losing_stats)<-gsub("^L","",names(reg_losing_stats)) # remove L from col name
names(reg_losing_stats)<-gsub("^W","",names(reg_losing_stats)) # remove "W" from "Wloc"

#### "Stack win/loss data into the long format"  ####
reg_long_stats <- rbind(reg_winning_stats, reg_losing_stats)
reg_long_stats <- arrange(reg_long_stats, team, Daynum)

#### calculate how previous performance from last 5 games (width = 5) predicts outcome of games ####
## where _pp_ == "previous performance" 
reg_pp_5w <- previous_perf(data = reg_long_stats, grouper = c("Season", "team"), arranger = "Daynum", width = 5, func = mean, exclude = c("Season", "Daynum", "loc", "win_loss", "gameID", "score")) # leaving score out...because it''s confusing -- this is averaging their final score over the last 5 games

#### single team previous performance data ####
head(reg_pp_5w)
head(reg_long_stats)

## checking merge output... want the rows where team.x != team.y
##...as 4 rows are returned with data all criss crossed
## i.e. for the 4 rows returned, we want rows 2 and 3 (not 1 and 4)
# test <- filter(reg_pp_5w, gameID == "2017_1101_1149") %>% select(gameID, team, win_loss, fgm, fgm3, or)
# merge(x = test, y=  test, by.x = "gameID", by.y="gameID") # see why

#### opponent adjusted data set ####
### where "_opp_ == "opponent adjusted data"
reg_opp_5w <- merge(x = dplyr::filter(reg_pp_5w), 
                       y = dplyr::filter(reg_pp_5w), 
                       by.x = "gameID", 
                       by.y = "gameID")

## THIS STEP REMOVES DUPLICATE ROWS WITH ERRONEOUS DATA MATCHED UP
## IF YOU DON'T UNDESTAND THIS, LOOK AT THE object "TEST" ABOVE FOR RESULTS OF THE MERGE
## Also removes rows with NA
reg_opp_5w <- dplyr::filter(reg_opp_5w, !is.na(team.x) & team.x != team.y) %>% select(-win_loss.y, -Season.y, -Daynum.y, -loc.y)
reg_opp_5w$loc.x <- as.factor(reg_opp_5w$loc.x)

head(reg_opp_5w)

#### DO ONE MORE CHECK ON DATA OUTPUT ####
# filter(reg_pp_5w, gameID == "2017_1101_1270") %>% select(team, gameID, fgm, fgm3, ftm) ## this is data before join
# filter(reg_opp_5w, gameID == "2017_1101_1270") %>% select(team.x, gameID, fgm.x, fgm3.x, ftm.x, fgm.y, fgm3.y, ftm.y) ## it should equal the team.x stats in this data frame

