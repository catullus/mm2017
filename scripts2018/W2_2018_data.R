## W2 2018 data
## Goal: 
# Mess with one reg2018ular season data and the previous_perf() function
# Avoid ranks
# Make sure function works with NCAA data

## seasons being used for this and other data sets
submission_target <- c(2018)

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
names(reg2018) <- tolower(names(reg2018))
## arrange by lowest Wteam, as that is how kaggle wants the IDs arranged (lowest team first)
reg2018 <- dplyr::arrange(reg2018, season, daynum, wteamid, lteamid) %>% dplyr::rename(wteam=wteamid, lteam=lteamid) %>% filter(season == submission_target)
team2018 <- read.csv(paste0(inpath, "data2018/Teams.csv"), stringsAsFactors = FALSE)
seeds2018 <- read.csv(paste0(inpath, "data2018/NCAATourneySeeds.csv"), stringsAsFactors = FALSE)
seeds2018$uid <- paste0(seeds2018$Season,"_",seeds2018$Team)
seeds2018$Seed_num <- as.integer(gsub("^W|^X|^Y|^Z|a|b","", seeds2018$Seed))

# create unique identifier for a game matchup e.g. "season_team1_team2"
reg2018$gameid <- paste0(reg2018$season, "_", reg2018$wteam, "_", reg2018$lteam)

## assign win/loss and total point diff
reg2018$wscore_diff <- reg2018$wscore - reg2018$lscore
reg2018$lscore_diff <- reg2018$lscore - reg2018$wscore

### give me one season for testing
reg2018 <- dplyr::filter(reg2018, season %in% c(2018)) ## comment out or modify to expand data set


#create adjusted shooting stats:  (fg = 2 or 3 pointer...)  let's partition 2 and 3 pointers:
######### wINNING TEAM #############
#2-pointers attempted:
reg2018$wfga2<-reg2018$wfga-reg2018$wfga3
#2-pointers made:
reg2018$wfgm2<-reg2018$wfgm-reg2018$wfgm3
#2-point made %
reg2018$wfgm2.pct<-reg2018$wfgm2/reg2018$wfga2
#2/3 point ratio attempts
reg2018$wfga23.rat<-reg2018$wfga2/reg2018$wfga3

#ft made %
reg2018$wftm.pct<-reg2018$wftm/reg2018$wfta

#3-point made %
reg2018$wfgm3.pct<-reg2018$wfgm3/reg2018$wfga3

#create a score check to make sure the fgm2, fgm3, and ftm add up to respective score:  #check should be 0
w.score.check<-reg2018$wscore-((reg2018$wfgm2*2)+(reg2018$wfgm3*3)+reg2018$wftm)

######### lOSING TEAM ##############
#2-pointers attempted:
reg2018$lfga2<-reg2018$lfga-reg2018$lfga3
#2-pointers made:
reg2018$lfgm2<-reg2018$lfgm-reg2018$lfgm3
#2-point made %
reg2018$lfgm2.pct<-reg2018$lfgm2/reg2018$lfga2
#2/3 point ratio attempts
reg2018$lfga23.rat<-reg2018$lfga2/reg2018$lfga3

#ft made %
reg2018$lftm.pct<-reg2018$lftm/reg2018$lfta

#3-point made %
reg2018$lfgm3.pct<-reg2018$lfgm3/reg2018$lfga3

#teams' shooting percentages:
reg2018$wshoot.prct<-reg2018$wfgm2+reg2018$wfgm3+reg2018$wftm/reg2018$wfga2+reg2018$wfga3+reg2018$wfta
reg2018$lshoot.prct<-reg2018$lfgm2+reg2018$lfgm3+reg2018$lftm/reg2018$lfga2+reg2018$lfga3+reg2018$lfta

#teams' weighted shooting percentages:
reg2018$wshoot.prct.wt<-(2*reg2018$wfgm2)+(3*reg2018$wfgm3)+reg2018$wftm/reg2018$wfga2+reg2018$wfga3+reg2018$wfta
reg2018$lshoot.prct.wt<-(2*reg2018$lfgm2)+(3*reg2018$lfgm3)+reg2018$lftm/reg2018$lfga2+reg2018$lfga3+reg2018$lfta

#rebound prct
#offensive rbds attempts w team = defensive rbds attempts l team
reg2018$wor.a<-reg2018$wor+reg2018$ldr
reg2018$wor.pct<-reg2018$wor/reg2018$wor.a
reg2018$ldr.a<-reg2018$wor.a
reg2018$ldr.pct<-reg2018$ldr/reg2018$ldr.a

#defensive rbds attempts w team = offensive rbds attempts l team
reg2018$wdr.a<-reg2018$wdr+reg2018$lor
reg2018$wdr.pct<-reg2018$wdr/reg2018$wdr.a
reg2018$lor.a<-reg2018$wdr.a
reg2018$lor.pct<-reg2018$lor/reg2018$lor.a

#### numer of possessions:####
reg2018$wposs<-reg2018$wfga2+reg2018$wfga3+reg2018$wfta+reg2018$wto   #multiply the turnovers by the shooting % of l team to get a more accurate picture of how detrimental the turnovers are.  Also, on the other end, multiple the defensive rbs by the shooting percentage of w team to see how advantageous the defensive rbs are...
reg2018$wposs.action<-(reg2018$wfgm2+reg2018$wfgm3+reg2018$wftm-(reg2018$wto*(reg2018$lshoot.prct/100))+(reg2018$wdr*(reg2018$wshoot.prct/100)))
reg2018$wposs.eff<-reg2018$wposs.action/reg2018$wposs
#using weighted shooting %
reg2018$wposs.action.wt<-(reg2018$wfgm2+reg2018$wfgm3+reg2018$wftm-(reg2018$wto*(reg2018$lshoot.prct.wt/100))+(reg2018$wdr*(reg2018$wshoot.prct.wt/100)))
#possession efficiency:
reg2018$wposs.eff.wt<-reg2018$wposs.action.wt/reg2018$wposs

#losing team:
reg2018$lposs<-reg2018$lfga2+reg2018$lfga3+reg2018$lfta+reg2018$ldr+reg2018$lto
#non-weighted shooting stats:
reg2018$lposs.action<-(reg2018$lfgm2+reg2018$lfgm3+reg2018$lftm-(reg2018$lto*(reg2018$wshoot.prct/100))+(reg2018$ldr*(reg2018$lshoot.prct/100)))
reg2018$lposs.eff<-reg2018$lposs.action/reg2018$lposs
#weighted shooting stats:
reg2018$lposs.action.wt<-(reg2018$lfgm2+reg2018$lfgm3+reg2018$lftm-(reg2018$lto*(reg2018$wshoot.prct.wt/100))+(reg2018$ldr*(reg2018$lshoot.prct.wt/100)))
reg2018$lposs.eff.wt<-reg2018$lposs.action.wt/reg2018$lposs

### generate ranks for the filtered reg2018ular season data set
reg2018 <-  plyr::ddply(reg2018, .(season), function(x) {ranker(x)})

reg2018$weekkey_loser <- NULL
reg2018$weekkey_winner <- NULL

"weekkey_loser" %in% names(reg2018)

#### rename "w" and "l" columns ####
reg2018_winning_stats <-reg2018[,grep("^w.*|week|season|daynum|gameid|team_rank", names(reg2018))]
reg2018_losing_stats <-reg2018[,grep("^l.*|week|season|daynum|gameid|team_rank",names(reg2018))] ## location doesn't get picked up for this one

names(reg2018_winning_stats)<-gsub("^w","",names(reg2018_winning_stats)) #remove w from col name
reg2018_winning_stats <- dplyr::rename(reg2018_winning_stats) %>% dplyr::select(-loc)
reg2018_winning_stats$win_loss <- "win"
names(reg2018_losing_stats)<-gsub("^l","",names(reg2018_losing_stats)) # remove l from col name
names(reg2018_losing_stats)<-gsub("^w","",names(reg2018_losing_stats)) # remove "w" from "wloc"
reg2018_losing_stats <- rename(reg2018_losing_stats)
reg2018_losing_stats$win_loss <- "loss"

## check names matching
#data.frame(names(reg2018_winning_stats) %in% names(reg2018_losing_stats), names(reg2018_winning_stats))
#data.frame(names(reg2018_losing_stats) %in% names(reg2018_winning_stats), names(reg2018_losing_stats))

#### "Stack win/loss data into the long format"  ####
reg2018_long_stats <- rbind(reg2018_winning_stats, reg2018_losing_stats)
reg2018_long_stats <- arrange(reg2018_long_stats, team, daynum)

#### calculate how previous performance from last 5 games (width = 5) predicts outcome of games ####
## where _pp_ == "previous performance" 
reg2018_pp_5w <- previous_perf(data = reg2018_long_stats, grouper = c("season", "team"), arranger = "daynum", width = 5, func = mean, exclude = c("season", "daynum", "loc", "win_loss", "gameid", "score", "win", "week")) # leaving score out...because it''s confusing -- this is averaging their final score over the last 5 games

reg2018_opp_5w <- merge(x = dplyr::filter(reg2018_pp_5w), 
                        y = dplyr::filter(reg2018_pp_5w), 
                        by.x = "gameid", 
                        by.y = "gameid")

## THIS STEP REMOVES DUPlICATE ROwS wITH ERRONEOUS DATA MATCHED UP
## IF YOU DON'T UNDESTAND THIS, lOOK AT THE object "TEST" ABOVE FOR RESUlTS OF THE MERGE
## Also removes rows with NA
reg2018_opp_5w <- dplyr::filter(reg2018_opp_5w, !is.na(team.x) & team.x != team.y) %>% select(-win_loss.y, -season.y, -daynum.y)

