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

train<-regrank[1:round(0.75*nrow(regrank),0),]
test<-regrank[(round(0.75*nrow(regrank),0)+1):nrow(regrank),]
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
sort(table(upsets$Conference),decreasing = T)
upsetsBySeedPerYear<-sort(table(upsets$Seed),decreasing = T)/ length(unique(upsets$Year))
#total upset ranks by conference:
upsets %>% group_by(Conference) %>% summarise(Frequency = sum(Seed))
#upsets per year:
upsetsByYear<-table(upsets$Year)
plot(as.numeric(upsetsByYear),type="b")
abline(h=mean(as.numeric(upsetsByYear)),col="blue")
# library(zoo)
# lines(rollmean(upsetsByYear,3,align = "left"),col="red")

