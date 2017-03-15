
library(plyr)
library(dplyr)
library(stringr)
library(PlayerRatings)
library(car)
library(randomForest)
library(tidyr)
# library(gbm)

#inpath <- "C:/Users/jroberti/Git/mm2017/data/"
inpath <- "C:/Users/Amy/Documents/GitHub/mm2017/data/"
#inpath <- "C:/Users/cflagg/Documents/R_Projects/"

reg <- read.csv(paste0(inpath, "RegularSeasonCompactResults.csv"), stringsAsFactors = FALSE)
reg <- filter(reg, Season %in% c(2002:2013))

team <- read.csv(paste0(inpath, "Teams.csv"), stringsAsFactors = FALSE)
seasons <- read.csv(paste0(inpath, "Seasons.csv"), stringsAsFactors = FALSE)

tourney <- read.csv(paste0(inpath, "TourneyCompactResults.csv"), stringsAsFactors = FALSE)
tourney <- filter(tourney, Season %in% c(2002:2013))

seeds <- read.csv(paste0(inpath, "TourneySeeds.csv"), stringsAsFactors = FALSE)

#head(reg)




## Function: calculate and munge weekly ranks -- pass function to ddply


ranker <- function(data){
  
  # assign week of the year based on Daynum -- add year from 'Season'
  ## for the given day, assign the numeric week
  week_idx <- data.frame(day=seq(0, 161, 7), week=seq(1,24, 1))
  assign_week <- function(d, idx){
    for (i in 1:length(idx$day)){
      if (d >= idx[i,1] & d <= idx[i+1,1]){
        return(idx[i,2])
      }
    }
  }
  
  # assign 1 for win, 0 for loss, 0.5 for draw to team in left-most column
  data$Outcome <- ifelse(data$Wscore - data$Lscore>0, 1, 0)
  data$Week <- sapply(data$Daynum, assign_week, week_idx)
  data$weekkey_winner <- paste0(data$Wteam,"_",data$Week)
  data$weekkey_loser <- paste0(data$Lteam,"_",data$Week)
  
  # this uses the Daynum converted to week
  ranks <- steph(select(data, Week, Wteam, Lteam, Outcome), history=TRUE)
  
  # matrix comes in wide format
  wide_ranks <- as.data.frame(ranks$history)
  wide_ranks$player <- rownames(wide_ranks)
  
  # convert to long format
  long_ranks <- tidyr::gather(wide_ranks, value = player)
  names(long_ranks) <- c("player", "type", "value")
  ## this is screwing up the week -- split is a regex field
  long_ranks$week <- ldply(stringr::str_split(long_ranks$type,"[.]"))[,1]
  
  # only grab ratings
  weekly_ranks <- filter(long_ranks, str_detect(type, "Rating"))
  weekly_ranks$weekkey <- paste0(weekly_ranks$player,"_",weekly_ranks$week)
  
  # 
  reg_weekly_ranks <- merge(data, weekly_ranks, by.x=c("weekkey_winner"), by.y=c("weekkey"))
  reg_weekly_ranks <- rename(reg_weekly_ranks, Wteam_rank=value) %>% select(-player, -type, -week)
  reg_weekly_ranks <- merge(reg_weekly_ranks, weekly_ranks, by.x=c("weekkey_loser"), by.y=c("weekkey"))
  reg_weekly_ranks <- rename(reg_weekly_ranks, Lteam_rank=value) %>% select(-player, -type, -week)
  return(reg_weekly_ranks)
}

# 2008 is a problem with Week
#out <- plyr::ddply(filter(reg, Season %in% c(2008)), .(Season), function(x) {ranker(x)})



# Villanova is in top 5 for entire country (they won 2016 tourney)
# #1 ranked Kansas made it to Final Four, lost to Villanova
  # Miami FL is in top 15, and made it to Elite Eight
# Maryland is in top 30, and made it to Elite Eight
# Oregon is in top 3, and made it to Final Four
# Oklahoma is in top 15 and made it to Final Four
# Texas A&M made it to Elite Eight and is in top 20
# Duke is in top 30, made it to Elite Eight
# Syracuse only potential 'Dark Horse', made it to Final Four but was not Ranked Top 30 for 2016 season

## Munge: Calculate ranks throughout season


reg$wdiff <- reg$Wscore - reg$Lscore
reg$ldiff <- reg$Lscore - reg$Wscore


#start <- Sys.time()
regrank <-  plyr::ddply(reg, .(Season), function(x) {ranker(x)})
#end <- Sys.time()
#end-start



## Explore: Winning_Team_Rank / Losing_Team_Rank ~ Score Differential per game

# Do games with equal ranked teams result in smaller differences?
# For the most part, higher ranked teams win much more
# How many lower ranked teams beat higher ranked teams?
# Teams to the left of the vertical red line = lower ranked teams beating higher ranked teams
# Win differences seem to be smaller for lower ranked teams beating higher


#(regrank, aes(Wteam_rank/Lteam_rank, wdiff)) + geom_point() + geom_vline(xintercept = 1, colour = "red")


## Calculate: Weight Wins and Losses via the Wteam_rank and Lteam_rank


## WEIGHT ALG
#head(regrank)
regrank$win_weight <- (regrank$Lteam_rank/regrank$Wteam_rank)#1
regrank$loss_weight <- (regrank$Lteam_rank/regrank$Wteam_rank)#1

#(regrank, aes(win_weight, wdiff)) + geom_point()


## Munge: Process Regular Season data e.g. 'Stack' the win and loss data frames to quickly count end of season statistics (e.g. wins and losses)


wreg <- select(regrank, Season, Daynum, Wteam, Wscore, Wteam_rank, Wloc, Numot, wdiff, win_weight, Week) %>% rename(team=Wteam,score=Wscore,loc=Wloc,diff=wdiff, weight=win_weight, rank=Wteam_rank)
lreg <- select(regrank, Season, Daynum, Lteam, Lscore, Lteam_rank, Wloc, Numot, ldiff, loss_weight, Week) %>% rename(team=Lteam,score=Lscore,loc=Wloc,diff=ldiff, weight=loss_weight, rank=Lteam_rank)
outreg <- rbind(wreg,lreg)

## count wins and losses
outreg$outcome <- ifelse(outreg$diff > 0, "win", "loss")

proc_reg <- group_by(outreg, Season, team) %>%
  arrange(Week) %>% 
  dplyr::summarise(totwin=sum(str_count(outcome, "win")),
                   totloss=sum(str_count(outcome, "loss")),
                   totwin_weight=sum(ifelse(diff>0, as.numeric(weight), 0)),
                   totloss_weight=sum(ifelse(diff<0, as.numeric(weight), 0)),
                   final_rank=tail(rank, 1),
                   rank_sd=sd(rank),
                   wdiff_avg=mean(ifelse(diff>0, as.numeric(diff), 0)),
                   ldiff_avg=mean(ifelse(diff<0, as.numeric(diff), 0)),
                   score_avg=mean(score),
                   score_sd=sd(score),
                   wdiff_sd=sd(ifelse(diff>0, as.numeric(diff),0)),
                   ldiff_sd=sd(ifelse(diff<0, as.numeric(diff),0))
  )

##head(proc_reg)


### Calculate: Determine end of season performance


## how to pull the last N number of games
season_end_wins <- outreg %>% 
  group_by(Season, team) %>% 
  arrange(Daynum) %>% 
  top_n(6, wt=Daynum) %>% 
  #summarize(last6_win=sum(Outcome))
  summarize(last6_win=sum(str_count(outcome, "win")))

season_end_wins$key <- paste0(season_end_wins$Season,"_",season_end_wins$team)




tourney$wdiff <- tourney$Wscore - tourney$Lscore
tourney$ldiff <- tourney$Lscore - tourney$Wscore

wtourney <- select(tourney, Season, Daynum, Wteam, Wscore, Wloc, Numot, wdiff) %>% rename(team=Wteam,score=Wscore,loc=Wloc,diff=wdiff)
ltourney <- select(tourney, Season, Daynum, Lteam, Lscore, Wloc, Numot, ldiff) %>% rename(team=Lteam,score=Lscore,loc=Wloc,diff=ldiff)

outtourney <- rbind(wtourney,ltourney)
outtourney$outcome <- ifelse(outtourney$diff > 0, "win", "loss")

# 
proc_tourn <- group_by(outtourney, Season, team) %>%
  dplyr::summarise(totwin=sum(str_count(outcome, "win")), 
                   totloss=sum(str_count(outcome, "loss")),
                   wdiff_avg=mean(ifelse(diff>0, as.numeric(diff), 0)),
                   ldiff_avg=mean(ifelse(diff<0, as.numeric(diff), 0)),
                   score_avg=mean(score),
                   score_sd=sd(score),
                   wdiff_sd=sd(ifelse(diff>0, as.numeric(diff),0)),
                   ldiff_sd=sd(ifelse(diff<0, as.numeric(diff),0))
  )

## rename "T_" == tournament data
names(proc_tourn) <- paste0("T_",names(proc_tourn))


## Execute a merge so we can model tourney_outcomes ~ reg_season_data


## make keys to match the data between the two tables
proc_reg$key <- paste0(proc_reg$Season,"_",proc_reg$team)
proc_tourn$key <- paste0(proc_tourn$T_Season,"_",proc_tourn$T_team)

## the tournament results should be the left table, because the proc_reg table 
## has results of ALL teams that played (i.e. even teams that didn't make it to the tourney)
model_dat <- merge(proc_tourn, proc_reg, by.x="key", by.y="key")
model_dat <- merge(model_dat, season_end_wins)

model_dat$win_pct <- model_dat$totwin / (model_dat$totwin + model_dat$totloss)
model_dat$win_pct_weighted <- model_dat$totwin_weight / (model_dat$totwin_weight + model_dat$totloss_weight)


## EDA via Scatterplot Matrix



