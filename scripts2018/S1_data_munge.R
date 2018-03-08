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

##### READ RAW DATA #####
#inpath <- "C:/Users/Amy/Documents/GitHub/mm2017/data/"
inpath <- "~/Documents/GitHub/mm2017/data/"

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

reg$wdiff <- reg$Wscore - reg$Lscore
reg$ldiff <- reg$Lscore - reg$Wscore

#### RANK TEAMS BY ranker() FUNCTION ####
#start <- Sys.time()
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



