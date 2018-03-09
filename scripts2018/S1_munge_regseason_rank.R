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

source("~/GitHub/mm2017/scripts2018/functions_2018.R")
source("~/GitHub/mm2017/scripts2018/function_previous_performance_NCAA.R")

##### READ RAW DATA #####
#inpath <- "C:/Users/Amy/Documents/GitHub/mm2017/data/"
inpath <- "~/GitHub/mm2017/data/"

reg <- read.csv(paste0(inpath, "RegularSeasonCompactResults.csv"), stringsAsFactors = FALSE)
#reg <- filter(reg, Season %in% c(2002:2013))

team <- read.csv(paste0(inpath, "Teams.csv"), stringsAsFactors = FALSE)
seasons <- read.csv(paste0(inpath, "Seasons.csv"), stringsAsFactors = FALSE)

tourney <- read.csv(paste0(inpath, "TourneyCompactResults.csv"), stringsAsFactors = FALSE)
#tourney <- filter(tourney, Season %in% c(2002:2013))

seeds <- read.csv(paste0(inpath, "TourneySeeds.csv"), stringsAsFactors = FALSE)
seed <- filter(seeds, Season %in% c(2002:2016))
seed$key <- paste0(seed$Season,"_",seed$Team)
seed$seedval <- as.numeric(str_extract(seed$Seed, "[0-9]{1,2}"))

seed_loser <- seed
names(seed_loser) <- paste0("L_", names(seed_loser))
seed_winner <- seed
names(seed_winner) <- paste0("W_", names(seed_winner))

#head(reg)

reg$wdiff <- reg$Wscore - reg$Lscore
reg$ldiff <- reg$Lscore - reg$Wscore

#### RANK TEAMS BY ranker() FUNCTION ####
#start <- Sys.time()
debug(ranker)
regrank <-  plyr::ddply(reg, .(Season), function(x) {ranker(x)})
#end <- Sys.time()
#end-start

## Explore: Winning_Team_Rank / Losing_Team_Rank ~ Score Differential per game

## WEIGHT ALG
#head(regrank)
regrank$win_weight <- (regrank$Lteam_rank/regrank$Wteam_rank)#1
regrank$loss_weight <- (regrank$Lteam_rank/regrank$Wteam_rank)#1

wreg <- select(regrank, Season, Daynum, Wteam, Wscore, Wteam_rank, Wloc, Numot, wdiff, win_weight, Week) %>% rename(team=Wteam,score=Wscore,loc=Wloc,diff=wdiff, weight=win_weight, rank=Wteam_rank)
lreg <- select(regrank, Season, Daynum, Lteam, Lscore, Lteam_rank, Wloc, Numot, ldiff, loss_weight, Week) %>% rename(team=Lteam,score=Lscore,loc=Wloc,diff=ldiff, weight=loss_weight, rank=Lteam_rank)
outreg <- rbind(wreg,lreg)

## count wins and losses
outreg$outcome <- ifelse(outreg$diff > 0, "win", "loss")

#### CALCULATE:END OF SEASON TEAM RANK, WINS, LOSSES ####
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

#create distributions of weighted possession efficieny for each team by season and week:
#summarize
proc_reg.W <- data.frame(group_by(regrank, Season, Wteam) %>%
                             arrange(Week) %>% 
                             dplyr::summarise(wt.mean<-mean(Wposs.eff.wt),
                                              wt.sd<-sd(Wposs.eff.wt),
                                              wt.diff.mean<-mean(Wposs.eff.wt.diff),
                                              wt.diff.sd<-sd(Wposs.eff.wt.diff),
                                              rank.team<-mean((Wteam_rank))/max(Lteam_rank,Wteam_rank),
                                              team.count<-length(Wteam)))
names(proc_reg.W)<-c("Season","Team","wt.mean","wt.sd","wt.diff.mean","wt.diff.sd","rank","N")
#names(proc_reg.W)<-c("Season","Team","wt.mean","wt.sd","rank","N")

proc_reg.L <- data.frame(group_by(regrank, Season, Lteam) %>%
                             arrange(Week) %>% 
                             dplyr::summarise(wt.mean<-mean(Lposs.eff.wt),
                                              wt.sd<-sd(Lposs.eff.wt),
                                              wt.diff.mean<-mean(Lposs.eff.wt.diff),
                                              wt.diff.sd<-sd(Lposs.eff.wt.diff),
                                              rank.team<-mean((Lteam_rank))/max(Lteam_rank,Wteam_rank),
                                              team.count<-length(Lteam)))
names(proc_reg.L)<-c("Season","Team","wt.mean","wt.sd","wt.diff.mean","wt.diff.sd","rank","N")
#(proc_reg.L)<-c("Season","Team","wt.mean","wt.sd","rank","N")
