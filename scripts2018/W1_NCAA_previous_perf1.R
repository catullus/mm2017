## Goal: 
# Mess with one regular season data and the previous_perf() function
# Avoid ranks
# Make sure function works with NCAA data
# Currently working on 2017 data so I avoid renaming 50+ columns (column name format changed in 2018)

# The Flagg-Roberti MM Model
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
    inpath <- "C:/Users/jroberti/Git/mm2017/data/"
} else if (length(list.files("C:/Users/cflagg/Documents/GitHub/mm2017/data/")) > 0 ) {
    inpath <- "C:/Users/cflagg/Documents/GitHub/mm2017/"   
} else if (length(list.files("C:/Users/Amy/Documents/GitHub/mm2017/data/")) > 0) {
    inpath <- "C:/Users/Amy/Documents/GitHub/mm2017/"
}

source(paste0(inpath, "/scripts2018/function_previous_performance_NCAA.R"))

#### read detailed regular season results ####
reg<-read.csv(paste0(inpath, "data/RegularSeasonDetailedResults.csv"), stringsAsFactors = FALSE, header = TRUE)
team <- read.csv(paste0(inpath, "data/Teams.csv"), stringsAsFactors = FALSE)

## assign win/loss and total point diff
reg$Wscore_diff <- reg$Wscore - reg$Lscore
reg$Lscore_diff <- reg$Lscore - reg$Wscore

### give me one season for testing
reg <- dplyr::filter(reg, Season == 2017)



#### rename "W" and "L" columns ####
reg_winning_stats <-reg[,grep("W.*|Week|Season|Daynum", names(reg))]
reg_winning_stats$win_loss <- "win"
reg_losing_stats <-reg[,grep("L.*|Week|Season|Daynum|Wloc",names(reg))] ## location doesn't get picked up for this one
reg_losing_stats$win_loss <- "loss"

names(reg_winning_stats)<-gsub("^W","",names(reg_winning_stats)) #remove W from col name
names(reg_losing_stats)<-gsub("^L","",names(reg_losing_stats)) # remove L from col name
names(reg_losing_stats)<-gsub("^W","",names(reg_losing_stats)) # remove "W" from "Wloc"

#### "Stack win/loss data into the long format"  ####
reg_long_stats <- rbind(reg_winning_stats, reg_losing_stats)
reg_long_stats <- arrange(reg_long_stats, team, Daynum)

#### calculate how previous performance from last 5 games (width = 5) predicts outcome of games ####
reg_prevperf_5w <- previous_perf(data = reg_long_stats, grouper = "team", arranger = "Daynum", width = 5, func = mean, exclude = c("Season", "Daynum", "loc", "win_loss"))

## review output 
# review team 1101 from the 2017 season
# their first two games they scored 65 points... (in reg_long_stats)
# note the columns in reg_prevperf now represent "means" rather than raw stats i.e. the mean of the last 5 games (if there are 5 games to average)
head(reg_prevperf_5w)
head(reg_long_stats)

#### MODEL EXAMPLE #### 
## Don't put model results in this file
## We ask the question, can we predict a win/loss FOR ANY GAME given a team's performance up to 5 weeks prior to a game
## Why do this? We are using data from a team's 30+ games...rather than summarizing their 30 games into one end of season statistic
## randomForest(win_loss ~ score + fgm + loc + ftm + score_diff, data = reg_prevperf_5w)



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
