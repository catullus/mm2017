# The Flagg-Roberti MM Model
library(plyr)
library(dplyr)
library(stringr)
library(PlayerRatings)
library(car)
library(randomForest)
library(tidyr)


#Open the datasets:
#Open the datasets:
if (length(list.files("C:/Users/jroberti/Git/mm2017/data/")) > 0){
    inpath <- "C:/Users/jroberti/Git/mm2017/data/"
} else if (length(list.files("C:/Users/cflagg/Documents/GitHub/mm2017/data/")) > 0 ) {
    inpath <- "C:/Users/cflagg/Documents/GitHub/mm2017/data2018/"   
}
#grab detailed results:
reg<-read.csv(paste0(inpath, "RegularSeasonDetailedResults.csv"), stringsAsFactors = FALSE, header = TRUE)
reg2<-read.csv(paste0(inpath, "RegularSeasonCompactResults.csv"), stringsAsFactors = FALSE, header = TRUE)
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

#graph it:
Wteam.eff.wt<-density(reg$Wposs.eff.wt)
Lteam.eff.wt<-density(reg$Lposs.eff.wt)
# plot(Wteam.eff.wt, main="Possession Efficiency",xlim=c(0,1),ylim=c(0,6))
# polygon(Wteam.eff.wt,col="green")
# lines(Lteam.eff.wt)
# polygon(Lteam.eff.wt,col="red")
# #add means as vertical lines:
# abline(v=mean(reg$Wposs.eff.wt))
# abline(v=mean(reg$Lposs.eff.wt))

#efficiency differences (using weighted stats for this):
reg$Wposs.eff.wt.diff<-reg$Wposs.eff.wt-reg$Lposs.eff.wt
reg$Lposs.eff.wt.diff<-reg$Lposs.eff.wt-reg$Wposs.eff.wt
# plot(density(reg$Wposs.eff.wt.diff))
# lines(density(reg$Lposs.eff.wt.diff))

# #subset data - only want data past day #30 for each season to avoid lumping preseason etc.
# reg.sub.train<-reg[which(reg$Daynum>30 & reg$Season>2005 & reg$Season<2014),]
# #tourney<-read.csv(paste0(inpath, "TourneyDetailedResults.csv"), stringsAsFactors = FALSE, header = TRUE)
# reg.sub.test<-reg[which(reg$Daynum>30 & reg$Season>=2014),]



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
#regrank <-  plyr::ddply(reg.sub.train, .(Season), function(x) {ranker(x)})
regrank <-  plyr::ddply(reg, .(Season), function(x) {ranker(x)})


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

#merge the W and L data and convert names:
combinedStats<-merge(proc_reg.W,proc_reg.L,by = c("Season","Team"))
names(combinedStats)<-gsub(".x",".win",names(combinedStats))
names(combinedStats)<-gsub(".y",".loss",names(combinedStats))
#create ratio column of wins and loss counts
combinedStats$N.win.rat<-(combinedStats$N.win/(combinedStats$N.win+combinedStats$N.loss))
combinedStats$N.loss.rat<-(1-combinedStats$N.win.rat)
#create schedule adjusted possession efficiency stat based on rank differentials of matchups:
combinedStats$wt.mean.win.adj<-combinedStats$wt.mean.win*combinedStats$rank.win
combinedStats$wt.mean.loss.adj<-combinedStats$wt.mean.loss*combinedStats$rank.loss
#create schedule adjusted possession efficiency stat based on differences in score:
combinedStats$wt.diff.mean.win.adj<-combinedStats$wt.diff.mean.win*combinedStats$rank.win
combinedStats$wt.diff.mean.loss.adj<-combinedStats$wt.diff.mean.loss*combinedStats$rank.loss
#create final "rank" for each team:
# combinedStats$multiplier.rank<-(combinedStats$N.win.rat*combinedStats$rank.win+
#                                         combinedStats$N.loss.rat*combinedStats$rank.loss)/min(combinedStats$rank.win)

#create monte carlo iterations for wins and losses based on 1000 "games" (use win/loss ratio)
#create distributions (monte carlo "wins")
# set.seed(1234)
# season.dist.win<-apply(combinedStats[,c("wt.diff.mean.win.adj","wt.sd.win","N.win.rat")], 1, 
#                           function(x) rnorm(1000*x[3],x[1],x[2]))
# season.dist.loss<-apply(combinedStats[,c("wt.diff.mean.loss.adj","wt.sd.loss","N.loss.rat")], 1, 
#                    function(x) rnorm(1000*x[3],x[1],x[2]))
# #now, combine the distributions:
# final.dists<-apply( unname(cbind( season.dist.win, season.dist.loss )) , 1 , unlist )
# #name the nested vectors by team ID:
# names(final.dists)<-paste(combinedStats$Season,combinedStats$Team,sep=".")
# #test the distributions:
# hist(final.dists$`2010.1250`)
# winning.prob<-function(dist.1,dist.2){
#     team1<-sample(dist.1,1)
#     team2<-sample(dist.2,1)
#     result<-team1-team2
#     winner<-ifelse(team1>team2,1,2)
#    # output<-c(winner,result)
#     return(winner)
# }
# game.simulation<-replicate(n=10000,winning.prob(dist.1=final.dists$`2010.1393`,dist.2=final.dists$`2010.1293`))
# hist(game.simulation,ylim=c(0,10000))
# length(which(game.simulation==1))
# length(which(game.simulation==2))
# 
# 
# m1.prct <- lm(N.win ~ ., data = combinedStats)
# 
# summary(m1.prct)

################ GLM FIT #################
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

#testing  #testing  #testing  #testing  #testing  #testing  #testing  #testing  
#make score differential:
regrank$scoreDiff<-regrank$Wscore-regrank$Lscore
train<-regrank[1:round(0.75*nrow(regrank),0),]
test<-regrank[(round(0.75*nrow(regrank),0)+1):nrow(regrank),]
new.glm<-glm(scoreDiff~Wfgm2.pct+Wfga23.rat+Wfgm3.pct+Wor.pct+Wdr.pct+Wshoot.prct+Wstl+Wblk+
                Wposs.action.wt+Wposs.eff.wt+Wteam_rank+Lfgm2.pct+Lfga23.rat+Lfgm3.pct+Lor.pct+
                  Ldr.pct+Lshoot.prct+Lstl+Lblk+Lposs.action.wt+Lposs.eff.wt+Lteam_rank,data=train)   
summary(new.glm)
train$predScore.glm<-predict(new.glm, type="response") 
train$residual.glm<-train$score-train$predScore.glm

#now I need the season totals to feed into the model:
#team A vs Team B, then I'll have the correct output


#crunch mean stats for each team:
# meanStats.df<-aggregate(reg_long_stats[, -c(1,2,3,40)], list(reg_long_stats$team,reg_long_stats$Season), mean)

# grab all the rank data for the last week of each season
#finalRanksLose<-regrank[which(regrank$Week==15),c("weekkey_loser","Lteam_rank","Daynum","Season")]
# finalRanksLose<-regrank[,grep("W.*|Week|Season|Daynum|gameID", names(regrank))]
# #names(finalRanksLose)<-c("Team","rank","Daynum","Season")
# #finalRanksWin<-regrank[which(regrank$Week==15),c("weekkey_winner","Wteam_rank","Daynum","Season")]
# finalRanksWin<-regrank[,grep("L.*|Week|Season|Daynum|Wloc|gameID",names(regrank))]
# #names(finalRanksWin)<-c("Team","rank","Daynum","Season")
# #put all these into 1 df:
# finalRankAll <- rbind(finalRanksLose, finalRanksWin)
#combine weekkey names with daynum:
finalRanks.df<-reg_long_stats %>% group_by(team,Season)  %>% arrange(Daynum) %>% slice(n())
#remove the week_ID on the teams columns:
#finalRanks.df$Team<-substr(finalRanks.df$Team,0,4)
#get team means of all stats for each team by season using Regrank data:
#remove winning team location and Week (Wloc,Week)for the time being
regrank2<-regrank[,-grep("Wloc|Week",names(regrank))]
#find all "winning team columns"
winningTeamStats<-regrank2[,grep("W.*|Season",names(regrank2))]
losingTeamStats<-regrank2[,grep("L.*|Week|Season",names(regrank2))]
#for both, make sure the names are identical, so remove the "W" and "L" for each df respectively:
names(winningTeamStats)<-gsub("^W","",names(winningTeamStats))
names(losingTeamStats)<-gsub("^L","",names(losingTeamStats))
#rbind these DFs together:
finalStatsAll <- rbind(winningTeamStats, losingTeamStats)
#get team averages of numeric columns:
#meanSeasonStats.df<-finalStatsAll %>% group_by(team,Season)  %>% mean(or.pct,na.rm=T)
#d %>% group_by(Name) %>% summarise_at(vars(-Month), funs(mean(., na.rm=TRUE)))
meanSeasonStats.df<-aggregate(finalStatsAll[, -c(1,2)], list(finalStatsAll$team,finalStatsAll$Season), mean)

colnames(meanSeasonStats.df)[colnames(meanSeasonStats.df) == 'Group.1'] <- 'team'
colnames(meanSeasonStats.df)[colnames(meanSeasonStats.df) == 'Group.2'] <- 'season'
saveRDS(meanSeasonStats.df,"C:/users/jroberti/Git/mm2017/data2018/meanSeasonStats.rds")

#Build the mode:
train<-meanSeasonStats.df[1:round(0.75*nrow(meanSeasonStats.df),0),]
test<-meanSeasonStats.df[(round(0.75*nrow(meanSeasonStats.df),0)+1):nrow(meanSeasonStats.df),]


#read.csv(paste0(inpath, "TourneyDetailedResults.csv"), stringsAsFactors = FALSE, header = TRUE)
fit.glm<-glm(score~fgm2.pct+fga23.rat+fgm3.pct+or.pct+dr.pct+shoot.prct+stl+blk+
                poss.action.wt+poss.eff.wt+team_rank,data=train)
summary(fit.glm)
fit.RF<-randomForest(score~fgm2.pct+fga23.rat+fgm3.pct+or.pct+dr.pct+shoot.prct+stl+blk+
                poss.action.wt+poss.eff.wt+team_rank,data=train)
varImpPlot(fit.RF,type=2)
# fitLose<-glm(Lscore~Lfgm2.pct+Lfga23.rat+Lfgm3.pct+Lor.pct+Ldr.pct+Lshoot.prct+Lstl+Lblk+Wblk+Wstl+
#                  Lposs.action.wt+Lposs.eff.wt+Wposs.action.wt+Wposs.eff.wt+
#                  Lteam_rank+Wteam_rank,data=train)                

#fitLose<-glm(Lscore~Ldr.pct)


train$predScore.glm<-predict(fit.glm, type="response") 
train$predScore.RF<-predict(fit.RF, type="response")
#train$glmPredictLose<-predict(fitLose, type="response")
#train$falseWin<-ifelse(train$glmPredictLose>=train$glmPredictWin,1,0)
train$residual.glm<-train$score-train$predScore.glm
train$residual.RF<-train$score-train$predScore.RF
#accuracyTrain<-1-sum(train$falseWin)/nrow(train)

#run it with the test data:
test$predScore.glm<-predict(object = fit.glm, newdata = test)
test$predScore.RF<-predict(object = fit.RF, newdata = test)
#test$glmPredictLose<-predict(object = fitLose, newdata = test)
test$residual.glm<-test$score-test$predScore.glm
test$residual.RF<-test$score-test$predScore.RF
#accuracyTest<-1-sum(test$falseWin)/nrow(test)

################# USE MODEL TO PREDICT RESULTS OF TOURNEY GAMES ###############
tourney<-read.csv(paste0(inpath, "TourneyDetailedResults.csv"), stringsAsFactors = FALSE, header = TRUE)
#set the data up in the same way - ultimately I'll need the team ID, and season to predict points because I'll be 
#pulling the data from the respective season; then predict points for each team and find Win or loss:
testing123<-tourney[,c("Season","Wteam")]





