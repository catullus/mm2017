library(plyr)
library(dplyr)
library(stringr)
library(PlayerRatings)
library(car)
library(randomForest)
library(tidyr)


#using the tourney data from Kaggle:
path<-"C:/Users/jroberti/Git/mm2017/data2018"
setwd(path)
tourneyResults<-read.csv("NCAATourneyCompactResults.csv")
tourneySeeds<-read.csv("NCAATourneySeeds.csv")

if (length(list.files("C:/Users/jroberti/Git/mm2017/data/")) > 0){
    inpath <- "C:/Users/jroberti/Git/mm2017/data/"
} else if (length(list.files("C:/Users/cflagg/Documents/GitHub/mm2017/data/")) > 0 ) {
    inpath <- "C:/Users/cflagg/Documents/GitHub/mm2017/data2018/"   
}
#grab detailed results:
reg<-read.csv(paste0(inpath, "RegularSeasonDetailedResults.csv"), stringsAsFactors = FALSE, header = TRUE)

# create unique identifier for a game matchup e.g. "season_team1_team2"
reg$gameID <- paste0(reg$Season, "_", reg$Wteam, "_", reg$Lteam)

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

#efficiency differences (using weighted stats for this):
reg$Wposs.eff.wt.diff<-reg$Wposs.eff.wt-reg$Lposs.eff.wt
reg$Lposs.eff.wt.diff<-reg$Lposs.eff.wt-reg$Wposs.eff.wt

#run data thru ranking system:
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

#start <- Sys.time()
library(plyr)
regrank <-  plyr::ddply(reg, .(Season), function(x) {ranker(x)})

#### rename "W" and "L" columns ####
reg_winning_stats <-regrank[,grep("W.*|Week|Season|Daynum|gameID", names(regrank))]
reg_winning_stats$win_loss <- "win"
reg_losing_stats <-regrank[,grep("L.*|Week|Season|Daynum|Wloc|gameID",names(regrank))] ## location doesn't get picked up for this one
reg_losing_stats$win_loss <- "loss"

names(reg_winning_stats)<-gsub("^W","",names(reg_winning_stats)) #remove W from col name
names(reg_losing_stats)<-gsub("^L","",names(reg_losing_stats)) # remove L from col name
names(reg_losing_stats)<-gsub("^W","",names(reg_losing_stats)) # remove "W" from "Wloc"

#### "Stack win/loss data into the long format"  ####
reg_long_stats <- rbind(reg_winning_stats, reg_losing_stats)
reg_long_stats <- arrange(reg_long_stats, team, Daynum)

#create mean stats for the season:
meanSeasonStats.df<-aggregate(reg_long_stats[, -c(1,2)], list(reg_long_stats$team,reg_long_stats$Season), mean)

















# source("C:/Users/jroberti/Git/mm2017/scripts2018/function_previous_performance_NCAA.R")
# reg_pp_5w <- previous_perf(data = reg_long_stats, grouper = c("Season", "team"), arranger = "Daynum", width = 5, func = mean, exclude = c("Season", "Daynum", "loc", "win_loss", "gameID", "score")) # leaving score out...because it''s confusing -- this is averaging their final score over the last 5 games


#### opponent adjusted data set ####
### where "_opp_ == "opponent adjusted data"
reg_opp_season <- merge(x = dplyr::filter(reg_long_stats), 
                    y = dplyr::filter(reg_long_stats), 
                    by.x = "gameID", 
                    by.y = "gameID")
## THIS STEP REMOVES DUPLICATE ROWS WITH ERRONEOUS DATA MATCHED UP
## IF YOU DON'T UNDESTAND THIS, LOOK AT THE object "TEST" ABOVE FOR RESULTS OF THE MERGE
## Also removes rows with NA
reg_opp_season <- dplyr::filter(reg_opp_season, !is.na(team.x) & team.x != team.y) %>% select(-win_loss.y, -Season.y, -Daynum.y, -loc.y)
reg_opp_season$loc.x <- as.factor(reg_opp_season$loc.x)

head(reg_opp_season)

##########################################################################################
#############


# grab all the rank data for the last week of each season
finalRanksLose<-regrank[which(regrank$Week==15),c("weekkey_loser","Lteam_rank","Daynum","Season")]
names(finalRanksLose)<-c("Team","rank","Daynum","Season")
finalRanksWin<-regrank[which(regrank$Week==15),c("weekkey_winner","Wteam_rank","Daynum","Season")]
names(finalRanksWin)<-c("Team","rank","Daynum","Season")
#put all these into 1 df:
finalRankAll <- rbind(finalRanksLose, finalRanksWin)
#combine weekkey names with daynum:
finalRanks.df<-finalRankAll %>% group_by(Team,Season)  %>% arrange(Daynum) %>% slice(n())
#remove the week_ID on the teams columns:
finalRanks.df$Team<-substr(finalRanks.df$Team,0,4)
#get team means of all stats for each team by season using Regrank data:
#remove winning team location and Week (Wloc,Week)for the time being
regrank<-regrank[,-grep("Wloc|Week",names(regrank))]
#find all "winning team columns"
winningTeamStats<-regrank[,grep("W.*|Season",names(regrank))]
losingTeamStats<-regrank[,grep("L.*|Week|Season",names(regrank))]
#for both, make sure the names are identical, so remove the "W" and "L" for each df respectively:
names(winningTeamStats)<-gsub("^W","",names(winningTeamStats))
names(losingTeamStats)<-gsub("^L","",names(losingTeamStats))
#rbind these DFs together:
finalStatsAll <- rbind(winningTeamStats, losingTeamStats)
#get team averages of numeric columns:
#meanSeasonStats.df<-finalStatsAll %>% group_by(team,Season)  %>% mean(or.pct,na.rm=T)
#d %>% group_by(Name) %>% summarise_at(vars(-Month), funs(mean(., na.rm=TRUE)))
meanSeasonStats.df<-aggregate(finalStatsAll[, -c(1,2)], list(finalStatsAll$team,finalStatsAll$Season), mean)
#make team and Season names match among DFs:

#merge these with combined Stats:
NCAA.df<-merge(finalRanks.df,meanSeasonStats.df,by=intersect(names(finalRanks.df),names(meanSeasonStats.df)))









train<-regrank[1:round(0.75*nrow(regrank),0),]
test<-regrank[(round(0.75*nrow(regrank),0)+1):nrow(regrank),]


#now do it on the entire season's worth of data:
fullSeason<-readRDS("C:/Users/jroberti/Git/mm2017/data/data4Models.rds")







fit<-glm(Wscore~Wfgm2.pct+Wfga23.rat+Wfgm3.pct+Wor.pct+Wdr.pct+Wshoot.prct+Wstl+Wblk+
             Wposs.action.wt+Wposs.eff.wt,data=train)
summary(fit)
train$glmPredictScore<-predict(fit, type="response") 


names(fullSeason)<-paste0("W",names(fullSeason))
intersect(names(fullSeason),names(fit$coefficients))
fullSeason$predScore<-predict(object = fit, newdata = fullSeason)

#Ok so now I need to calculate the predicted score for each team based on their seasonal data, but I Still need to adjust for the opposing team...





#read.csv(paste0(inpath, "TourneyDetailedResults.csv"), stringsAsFactors = FALSE, header = TRUE)
fitWin<-glm(Wscore~Wfgm2.pct+Wfga23.rat+Wfgm3.pct+Wor.pct+Wdr.pct+Wshoot.prct+Wstl+Wblk+Lblk+Lstl+
                Wposs.action.wt+Wposs.eff.wt+Lposs.action.wt+Lposs.eff.wt+
                Wteam_rank+Lteam_rank,data=train)
fitLose<-glm(Lscore~Lfgm2.pct+Lfga23.rat+Lfgm3.pct+Lor.pct+Ldr.pct+Lshoot.prct+Lstl+Lblk+Wblk+Wstl+
                 Lposs.action.wt+Lposs.eff.wt+Wposs.action.wt+Wposs.eff.wt+
                 Lteam_rank+Wteam_rank,data=train)                

#fitLose<-glm(Lscore~Ldr.pct)

summary(fitWin)
train$glmPredictWin<-predict(fitWin, type="response") 
train$glmPredictLose<-predict(fitLose, type="response")
train$falseWin<-ifelse(train$glmPredictLose>=train$glmPredictWin,1,0)
accuracyTrain<-1-sum(train$falseWin)/nrow(train)

#run it with the test data:
test$glmPredictWin<-predict(object = fitWin, newdata = test)
test$glmPredictLose<-predict(object = fitLose, newdata = test)
test$falseWin<-ifelse(test$glmPredictLose>=test$glmPredictWin,1,0)
accuracyTest<-1-sum(test$falseWin)/nrow(test)




finalRanksLose<-regrank[which(regrank$Week==15),c("weekkey_loser","Lteam_rank","Daynum","Season")]
names(finalRanksLose)<-c("Team","rank","Daynum","Season")
finalRanksWin<-regrank[which(regrank$Week==15),c("weekkey_winner","Wteam_rank","Daynum","Season")]
names(finalRanksWin)<-c("Team","rank","Daynum","Season")
#put all these into 1 df:
finalRankAll <- rbind(finalRanksLose, finalRanksWin)
#combine weekkey names with daynum:
finalRanks.df<-finalRankAll %>% group_by(Team,Season)  %>% arrange(Daynum) %>% slice(n())
#remove the week_ID on the teams columns:
finalRanks.df$Team<-substr(finalRanks.df$Team,0,4)
finalRanks.df$TeamID_Season<-paste0(finalRanks.df$Team,"-",finalRanks.df$Season)

#add fake seed for winning team:
tourneyResults$WSeed<-NA
tourneyResults$LSeed<-NA
tourneyResults$WRank<-NA
tourneyResults$LRank<-NA
#go thru and for each teamID grab the ID and find it in winning team ID
tourneyResults$WTeamID_Season<-paste0(tourneyResults$WTeamID,"-",tourneyResults$Season)
tourneyResults$LTeamID_Season<-paste0(tourneyResults$LTeamID,"-",tourneyResults$Season)
tourneySeeds$TeamID_Season<-paste0(tourneySeeds$TeamID,"-",tourneySeeds$Season)
#convert teamID in final.df to numeric
hist(as.numeric(finalRanks.df$Team))


for(i in 1:nrow(tourneyResults)){
    #grab indices where teamID and season match seed file: (WINNING TEAM)
    matchingIndWin<-grep(tourneySeeds$TeamID_Season[i],tourneyResults$WTeamID_Season)
    #replace fake seed with team's actual seed:
    tourneyResults$WSeed[matchingIndWin]<-gsub("\\D","",tourneySeeds$Seed[i])
    #grab indices where teamID and season match seed file: (LOSING TEAM)
    matchingIndLose<-grep(tourneySeeds$TeamID_Season[i],tourneyResults$LTeamID_Season)
    #replace fake seed with team's actual seed:
    tourneyResults$LSeed[matchingIndLose]<-gsub("\\D","",tourneySeeds$Seed[i])
    
    #now do the end of season team ranks:
    matchingIndWinRank<-grep(tourneyResults$WTeamID_Season[i],finalRanks.df$TeamID_Season)
    #replace fake seed with team's actual seed:
    tourneyResults$WRank[i]<-ifelse(length(round(finalRanks.df$rank[matchingIndWinRank],0))>0,
                                    round(finalRanks.df$rank[matchingIndWinRank],0),NA)
    #grab indices where teamID and season match seed file: (LOSING TEAM)
    matchingIndLoseRank<-grep(tourneyResults$LTeamID_Season[i],finalRanks.df$TeamID_Season)
    #replace fake seed with team's actual seed:
    tourneyResults$LRank[i]<-ifelse(length(round(finalRanks.df$rank[matchingIndLoseRank],0))>0,
                                    round(finalRanks.df$rank[matchingIndLoseRank],0),NA)
}
#add Rounds:
tourneyResults$Round<-NA
for(i in 1:nrow(tourneyResults)){
    ############# DAYNUM 134 and 135 are play-in games (don't include in rounds)
    if(length(grep("136|137",tourneyResults$DayNum[i]))>0){
        tourneyResults$Round[i]<-1
    }
    if(length(grep("138|139",tourneyResults$DayNum[i]))>0){
        tourneyResults$Round[i]<-2
    }
    if(length(grep("143|144",tourneyResults$DayNum[i]))>0){
        tourneyResults$Round[i]<-3
    }
    if(length(grep("143|144",tourneyResults$DayNum[i]))>0){
        tourneyResults$Round[i]<-3
    }
    if(length(grep("145|146",tourneyResults$DayNum[i]))>0){
        tourneyResults$Round[i]<-4
    }
    if(length(grep("152",tourneyResults$DayNum[i]))>0){
        tourneyResults$Round[i]<-5
    }
    if(length(grep("154",tourneyResults$DayNum[i]))>0){
        tourneyResults$Round[i]<-6
    }
}

#will preserve same seed matchups
tourneyResults$UpsetSeed<-ifelse(tourneyResults$WSeed>tourneyResults$LSeed,1,0)
tourneyResults$UpsetRank<-ifelse(tourneyResults$WRank<tourneyResults$LRank,1,0)
#find how many first round upsets there are:
upsetSums<-tourneyResults %>% group_by(WSeed,LSeed,Round) %>% summarise(Frequency = sum(Upset))
#get upsets per year (average)
upsetSums$perYrAvg<-upsetSums$Frequency/length(unique(tourneyResults$Season))
#get upsets per year (probability)
upsetSums$perYrProb<-upsetSums$Frequency/(length(unique(tourneyResults$Season))*4)
#just grab first round upsets:
upsetsR1<-upsetSums[upsetSums$Round==1,]
upsetsR1.clean<-upsetsR1[complete.cases(upsetsR1),]
upsetsR2<-upsetSums[upsetSums$Round==2,]
upsetsR2.clean<-upsetsR2[complete.cases(upsetsR2),]
#save files:
saveRDS(upsetSums,"upsetsByRound.rds")
saveRDS(upsetsR1.clean,"round1Upsets.rds")

###############SCRAP ########################################
upsets<-read.csv("C:/Users/jroberti/DATA/mmNCAA/mmUpsets1stRound.csv")
#convert Southwest to midWest and southeast to south - for some reason the 2011 brackets had these other regions...
upsets$Conference<-gsub("Southwest","Midwest",upsets$Conference)
upsets$Conference<-gsub("Southeast","South",upsets$Conference)
upsets$seed_conf<-paste0(upsets$Conference,"-",upsets$Seed)
#get freuency table of seed + conference:
sort(table(upsets$seed_conf),decreasing = T)
sort(table(upsets$Seed),decreasing = T)
sort(table(upsets$Conference),decreasing = T)
upsetsProbBySeedPerYear<-sort(table(upsets$Seed),decreasing = T)/ (length(unique(upsets$Year))*4)
#total upset ranks by conference:
upsets %>% group_by(Conference) %>% summarise(Frequency = sum(Seed))
#upsets per year:
upsetsByYear<-table(upsets$Year)
plot(as.numeric(upsetsByYear),type="b")
abline(h=mean(as.numeric(upsetsByYear)),col="blue")
# library(zoo)
# lines(rollmean(upsetsByYear,3,align = "left"),col="red")

