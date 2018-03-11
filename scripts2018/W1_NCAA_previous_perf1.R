## Goal: 
# Mess with one regular season data and the previous_perf() function
# Avoid ranks
# Make sure function works with NCAA data

# The Flagg-Roberti MM Model
library(plyr)
library(dplyr)
library(stringr)
library(PlayerRatings)
library(car)
library(randomForest)
library(tidyr)

#Open the datasets:
if (length(list.files("C:/Users/jroberti/Git/mm2017/data/")) > 0){
    inpath <- "C:/Users/jroberti/Git/mm2017/data/"
} else if (length(list.files("C:/Users/cflagg/Documents/GitHub/mm2017/data/")) > 0 ) {
    inpath <- "C:/Users/cflagg/Documents/GitHub/mm2017/data/"   
}
#grab detailed results:
reg<-read.csv(paste0(inpath, "RegularSeasonDetailedResults.csv"), stringsAsFactors = FALSE, header = TRUE)
team <- read.csv(paste0(inpath, "Teams.csv"), stringsAsFactors = FALSE)
#create win and loss differential:
reg$Wdiff <- reg$Wscore - reg$Lscore
reg$Ldiff <- reg$Lscore - reg$Wscore

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
