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
reg <- dplyr::filter(reg, Season == 2017) ## comment out or modify to expand data set

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
reg_pp_5w <- previous_perf(data = reg_long_stats, grouper = "team", arranger = "Daynum", width = 5, func = mean, exclude = c("Season", "Daynum", "loc", "win_loss", "gameID"))

#### single team previous performance data ####
head(reg_pp_5w)
head(reg_long_stats)

#### This results in a "single team" data set i.e. we'll only be able to look at whether a single team's stats predict their future performance...while ignoring the strength of their opponent

#### opponent adjusted data set ####
### where "_opp_ == "opponent adjusted data"
reg_opp_5w <- merge(x = dplyr::filter(reg_prevperf_5w), 
                       y = dplyr::filter(reg_prevperf_5w), 
                       by.x = "gameID", 
                       by.y = "gameID")

#### remove NA rows ####
reg_opp_5w <- dplyr::filter(reg_opp_5w, !is.na(team.x))

## check for duplicates and othre weird stuff on the self-join
# test <- filter(reg_opp_5w, gameID == "2017_1187_1214")

reg_opp_5w$uid <- paste0(reg_opp_5w$gameID,"_", reg_opp_5w$win_loss.x) 
reg_opp_5w$dupe <- duplicated(reg_opp_5w$uid)

reg_opp_5w <- filter(reg_opp_5w, dupe == FALSE)

## each game is duplicated twice, (one for winning team, one for losing)
## reduce number of rows to make RF faster

### trying to figure out quick way to convert character columns to factor, without naming each column
# cols <- ldply(lapply(reg_opp_5w, is.character))
# 
# fix_cols <- dplyr::filter(cols, V1 == TRUE)
# 
# reg_opp_5w[cols] <- lapply(reg_opp_5w[,c(fix_cols[,1])], factor)

reg_opp_5w$loc.x <- as.factor(reg_opp_5w$loc.x)

rf5wOA <- randomForest(as.factor(win_loss.x) ~ ., data = na.exclude(dplyr::select(reg_opp_5w, -gameID, -team.x, -team.y, -win_loss.y, -loc.y, -Season.x, -Daynum.x, -Season.y, -Daynum.y, -score_diff.x, -score_diff.y, -score.x, -score.y, -uid, -dupe)))

rf5wOA
varImpPlot(rf5wOA)

# #### calculate additional stats #####
# #create win and loss differential:
# reg$Wdiff <- reg$Wscore - reg$Lscore
# reg$Ldiff <- reg$Lscore - reg$Wscore
# 
# #create adjusted shooting stats:  (fg = 2 or 3 pointer...)  let's partition 2 and 3 pointers:
# ######### WINNING TEAM #############
# #2-pointers attempted:
# reg$Wfga2<-reg$Wfga-reg$Wfga3
# #2-pointers made:
# reg$Wfgm2<-reg$Wfgm-reg$Wfgm3
# #2-point made %
# reg$Wfgm2.pct<-reg$Wfgm2/reg$Wfga2
# #2/3 point ratio attempts
# reg$Wfga23.rat<-reg$Wfga2/reg$Wfga3
# 
# #ft made %
# reg$Wftm.pct<-reg$Wftm/reg$Wfta
# 
# #3-point made %
# reg$Wfgm3.pct<-reg$Wfgm3/reg$Wfga3
# 
# #create a score check to make sure the fgm2, fgm3, and ftm add up to respective score:  #check should be 0
# W.score.check<-reg$Wscore-((reg$Wfgm2*2)+(reg$Wfgm3*3)+reg$Wftm)
# 
# ######### LOSING TEAM ##############
# #2-pointers attempted:
# reg$Lfga2<-reg$Lfga-reg$Lfga3
# #2-pointers made:
# reg$Lfgm2<-reg$Lfgm-reg$Lfgm3
# #2-point made %
# reg$Lfgm2.pct<-reg$Lfgm2/reg$Lfga2
# #2/3 point ratio attempts
# reg$Lfga23.rat<-reg$Lfga2/reg$Lfga3
# 
# #ft made %
# reg$Lftm.pct<-reg$Lftm/reg$Lfta
# 
# #3-point made %
# reg$Lfgm3.pct<-reg$Lfgm3/reg$Lfga3
# 
# #teams' shooting percentages:
# reg$Wshoot.prct<-reg$Wfgm2+reg$Wfgm3+reg$Wftm/reg$Wfga2+reg$Wfga3+reg$Wfta
# reg$Lshoot.prct<-reg$Lfgm2+reg$Lfgm3+reg$Lftm/reg$Lfga2+reg$Lfga3+reg$Lfta
# 
# #teams' weighted shooting percentages:
# reg$Wshoot.prct.wt<-(2*reg$Wfgm2)+(3*reg$Wfgm3)+reg$Wftm/reg$Wfga2+reg$Wfga3+reg$Wfta
# reg$Lshoot.prct.wt<-(2*reg$Lfgm2)+(3*reg$Lfgm3)+reg$Lftm/reg$Lfga2+reg$Lfga3+reg$Lfta
# 
# #rebound prct
# #offensive rbds attempts W team = defensive rbds attempts L team
# reg$Wor.a<-reg$Wor+reg$Ldr
# reg$Wor.pct<-reg$Wor/reg$Wor.a
# reg$Ldr.a<-reg$Wor.a
# reg$Ldr.pct<-reg$Ldr/reg$Ldr.a
# 
# #defensive rbds attempts W team = offensive rbds attempts L team
# reg$Wdr.a<-reg$Wdr+reg$Lor
# reg$Wdr.pct<-reg$Wdr/reg$Wdr.a
# reg$Lor.a<-reg$Wdr.a
# reg$Lor.pct<-reg$Lor/reg$Lor.a
# 
# #numer of possessions:
# reg$Wposs<-reg$Wfga2+reg$Wfga3+reg$Wfta+reg$Wto   #multiply the turnovers by the shooting % of L team to get a more accurate picture of how detrimental the turnovers are.  Also, on the other end, multiple the defensive rbs by the shooting percentage of W team to see how advantageous the defensive rbs are...
# reg$Wposs.action<-(reg$Wfgm2+reg$Wfgm3+reg$Wftm-(reg$Wto*(reg$Lshoot.prct/100))+(reg$Wdr*(reg$Wshoot.prct/100)))
# reg$Wposs.eff<-reg$Wposs.action/reg$Wposs 
# #using weighted shooting %
# reg$Wposs.action.wt<-(reg$Wfgm2+reg$Wfgm3+reg$Wftm-(reg$Wto*(reg$Lshoot.prct.wt/100))+(reg$Wdr*(reg$Wshoot.prct.wt/100)))
# #possession efficiency:
# reg$Wposs.eff.wt<-reg$Wposs.action.wt/reg$Wposs  
# #losing team:
# reg$Lposs<-reg$Lfga2+reg$Lfga3+reg$Lfta+reg$Ldr+reg$Lto
# #non-weighted shooting stats:
# reg$Lposs.action<-(reg$Lfgm2+reg$Lfgm3+reg$Lftm-(reg$Lto*(reg$Wshoot.prct/100))+(reg$Ldr*(reg$Lshoot.prct/100)))
# reg$Lposs.eff<-reg$Lposs.action/reg$Lposs  
# #weighted shooting stats:
# reg$Lposs.action.wt<-(reg$Lfgm2+reg$Lfgm3+reg$Lftm-(reg$Lto*(reg$Wshoot.prct.wt/100))+(reg$Ldr*(reg$Lshoot.prct.wt/100)))
# reg$Lposs.eff.wt<-reg$Lposs.action.wt/reg$Lposs  
