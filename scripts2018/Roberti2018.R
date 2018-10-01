# Roberti Model
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
#reg2<-read.csv(paste0(inpath, "RegularSeasonCompactResults.csv"), stringsAsFactors = FALSE, header = TRUE)
#team <- read.csv(paste0(inpath, "Teams.csv"), stringsAsFactors = FALSE)

#function to add ancillary stats to the dataset:
statCreate<-function(x){
    
    #create win and loss differential:
    x$Wdiff <- x$Wscore - x$Lscore
    x$Ldiff <- x$Lscore - x$Wscore
    
    #create adjusted shooting stats:  (fg = 2 or 3 pointer...)  let's partition 2 and 3 pointers:
    ######### WINNING TEAM #############
    #2-pointers attempted:
    x$Wfga2<-x$Wfga-x$Wfga3
    #2-pointers made:
    x$Wfgm2<-x$Wfgm-x$Wfgm3
    #2-point made %
    x$Wfgm2.pct<-x$Wfgm2/x$Wfga2
    #2/3 point ratio attempts
    x$Wfga23.rat<-x$Wfga2/x$Wfga3
    
    #ft made %
    x$Wftm.pct<-x$Wftm/x$Wfta
    
    #3-point made %
    x$Wfgm3.pct<-x$Wfgm3/x$Wfga3
    
    #create a score check to make sure the fgm2, fgm3, and ftm add up to respective score:  #check should be 0
    W.score.check<-x$Wscore-((x$Wfgm2*2)+(x$Wfgm3*3)+x$Wftm)
    
    ######### LOSING TEAM ##############
    #2-pointers attempted:
    x$Lfga2<-x$Lfga-x$Lfga3
    #2-pointers made:
    x$Lfgm2<-x$Lfgm-x$Lfgm3
    #2-point made %
    x$Lfgm2.pct<-x$Lfgm2/x$Lfga2
    #2/3 point ratio attempts
    x$Lfga23.rat<-x$Lfga2/x$Lfga3
    
    #ft made %
    x$Lftm.pct<-x$Lftm/x$Lfta
    
    #3-point made %
    x$Lfgm3.pct<-x$Lfgm3/x$Lfga3
    
    #teams' shooting percentages:
    x$Wshoot.prct<-x$Wfgm2+x$Wfgm3+x$Wftm/x$Wfga2+x$Wfga3+x$Wfta
    x$Lshoot.prct<-x$Lfgm2+x$Lfgm3+x$Lftm/x$Lfga2+x$Lfga3+x$Lfta
    
    #teams' weighted shooting percentages:
    x$Wshoot.prct.wt<-(2*x$Wfgm2)+(3*x$Wfgm3)+x$Wftm/x$Wfga2+x$Wfga3+x$Wfta
    x$Lshoot.prct.wt<-(2*x$Lfgm2)+(3*x$Lfgm3)+x$Lftm/x$Lfga2+x$Lfga3+x$Lfta
    
    #rebound prct
    #offensive rbds attempts W team = defensive rbds attempts L team
    x$Wor.a<-x$Wor+x$Ldr
    x$Wor.pct<-x$Wor/x$Wor.a
    x$Ldr.a<-x$Wor.a
    x$Ldr.pct<-x$Ldr/x$Ldr.a
    
    #defensive rbds attempts W team = offensive rbds attempts L team
    x$Wdr.a<-x$Wdr+x$Lor
    x$Wdr.pct<-x$Wdr/x$Wdr.a
    x$Lor.a<-x$Wdr.a
    x$Lor.pct<-x$Lor/x$Lor.a
    
    #numer of possessions:
    x$Wposs<-x$Wfga2+x$Wfga3+x$Wfta+x$Wto   #multiply the turnovers by the shooting % of L team to get a more accurate picture of how detrimental the turnovers are.  Also, on the other end, multiple the defensive rbs by the shooting percentage of W team to see how advantageous the defensive rbs are...
    x$Wposs.action<-(x$Wfgm2+x$Wfgm3+x$Wftm-(x$Wto*(x$Lshoot.prct/100))+(x$Wdr*(x$Wshoot.prct/100)))
    x$Wposs.eff<-x$Wposs.action/x$Wposs 
    #using weighted shooting %
    x$Wposs.action.wt<-(x$Wfgm2+x$Wfgm3+x$Wftm-(x$Wto*(x$Lshoot.prct.wt/100))+(x$Wdr*(x$Wshoot.prct.wt/100)))
    #possession efficiency:
    x$Wposs.eff.wt<-x$Wposs.action.wt/x$Wposs  
    #losing team:
    x$Lposs<-x$Lfga2+x$Lfga3+x$Lfta+x$Ldr+x$Lto
    #non-weighted shooting stats:
    x$Lposs.action<-(x$Lfgm2+x$Lfgm3+x$Lftm-(x$Lto*(x$Wshoot.prct/100))+(x$Ldr*(x$Lshoot.prct/100)))
    x$Lposs.eff<-x$Lposs.action/x$Lposs  
    #weighted shooting stats:
    x$Lposs.action.wt<-(x$Lfgm2+x$Lfgm3+x$Lftm-(x$Lto*(x$Wshoot.prct.wt/100))+(x$Ldr*(x$Lshoot.prct.wt/100)))
    x$Lposs.eff.wt<-x$Lposs.action.wt/x$Lposs  
    
    #graph it:
    Wteam.eff.wt<-density(x$Wposs.eff.wt)
    Lteam.eff.wt<-density(x$Lposs.eff.wt)
    # plot(Wteam.eff.wt, main="Possession Efficiency",xlim=c(0,1),ylim=c(0,6))
    # polygon(Wteam.eff.wt,col="green")
    # lines(Lteam.eff.wt)
    # polygon(Lteam.eff.wt,col="red")
    # #add means as vertical lines:
    # abline(v=mean(x$Wposs.eff.wt))
    # abline(v=mean(x$Lposs.eff.wt))
    
    #efficiency differences (using weighted stats for this):
    x$Wposs.eff.wt.diff<-x$Wposs.eff.wt-x$Lposs.eff.wt
    x$Lposs.eff.wt.diff<-x$Lposs.eff.wt-x$Wposs.eff.wt
    # plot(density(reg$Wposs.eff.wt.diff))
    # lines(density(reg$Lposs.eff.wt.diff))
    return(x)
}
#crunch ancillary stats:
reg<-statCreate(reg)
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

# ################ GLM FIT #################
# reg_winning_stats <-regrank[,grep("W.*|Week|Season|Daynum|gameID", names(regrank))]
# reg_winning_stats$win_loss <- "win"
# reg_losing_stats <-regrank[,grep("L.*|Week|Season|Daynum|Wloc|gameID",names(regrank))] ## location doesn't get picked up for this one
# reg_losing_stats$win_loss <- "loss"
# 
# names(reg_winning_stats)<-gsub("^W","",names(reg_winning_stats)) #remove W from col name
# names(reg_losing_stats)<-gsub("^L","",names(reg_losing_stats)) # remove L from col name
# names(reg_losing_stats)<-gsub("^W","",names(reg_losing_stats)) # remove "W" from "Wloc"
# 
# #### "Stack win/loss data into the long format"  ####
# reg_long_stats <- rbind(reg_winning_stats, reg_losing_stats)
# reg_long_stats <- arrange(reg_long_stats, team, Daynum)

############################# GLM MODEL GLM MODEL GLM MODEL #################
# make training dataset
train<-regrank[1:round(0.75*nrow(regrank),0),]
#take the training set and split it in half (one half will reflect winning team)
trainWin<-train[1:round(0.5*nrow(train),0),]
#make team A = winning team, make Team B = Losing team (opposite of the train.mirror df)
names(trainWin)<-gsub("^L","teamB_",names(trainWin))
names(trainWin)<-gsub("^W","teamA_",names(trainWin))
#remove the first two columns:
trainWin<-trainWin[,-c(1,2)]
#fix Week and location column:
colnames(trainWin)[colnames(trainWin) == 'teamA_eek'] <- 'Week'
colnames(trainWin)[colnames(trainWin) == 'teamA_loc'] <- 'wloc'
#make dataframe where team A and B are reversed 
trainLose<-train[(round(0.5*nrow(train),0)+1):nrow(train),]
#move Losing team data under Winning team data:
trainLoseStart<-trainLose[,grep("L.*",names(trainLose))]
#call teams A and B and not Win and Lose:
names(trainLoseStart)<-gsub("^L","teamA_",names(trainLoseStart))
#make sequence for logic to grab non-L.* names:
nameSeq<-1:length(names(trainLose))
#grab winning team indicies:
trainLoseEnd<-trainLose[,which(nameSeq %in% grep("L.*",names(trainLose))==FALSE)]
#remove the first two columns:
trainLoseEnd<-trainLoseEnd[,-c(1,2)]
#rename to team B:
names(trainLoseEnd)<-gsub("^W","teamB_",names(trainLoseEnd))
#fix Week column:
colnames(trainLoseEnd)[colnames(trainLoseEnd) == 'teamB_eek'] <- 'Week'
colnames(trainLoseEnd)[colnames(trainLoseEnd) == 'teamB_loc'] <- 'wloc'
#cbind the two dataframes together (TEAM A is losing team, TEAM B is winning team; this is opposite of train.win
train.mirror<-cbind(trainLoseStart,trainLoseEnd)
#rbind the two above dataframes:
train.updated<-rbind(trainWin,train.mirror)

#test<-regrank[(round(0.75*nrow(regrank),0)+1):nrow(regrank),]
fit.glm<-glm(teamA_diff~teamA_fgm2.pct+teamA_fga23.rat+teamA_fgm3.pct+teamA_or.pct+teamA_dr.pct+teamA_shoot.prct+
                 teamA_stl+teamA_blk+teamA_poss.action.wt+teamA_poss.eff.wt+teamA_team_rank+
                 teamB_fgm2.pct+teamB_fga23.rat+teamB_fgm3.pct+teamB_shoot.prct+teamB_stl+
                 teamB_blk+teamB_poss.action.wt+teamB_poss.eff.wt+teamB_team_rank,data=train.updated)

# 
# train$predScoreDiff.glm<-predict(fit.glm, type="response") 
# fit.glm<-glm(Wscore~Wfgm2.pct+Wfga23.rat+Wfgm3.pct+Wor.pct+Wdr.pct+Wshoot.prct+Wstl+Wblk+
#                  Wposs.action.wt+Wposs.eff.wt+Wteam_rank+Lfgm2.pct+Lfga23.rat+Lfgm3.pct+
#                  Lshoot.prct+Lstl+Lblk+Lposs.action.wt+Lposs.eff.wt+Lteam_rank,data=train)   
summary(fit.glm)
length(fit.glm$coefficients) > fit.glm$rank  #should be FALSE!
train.updated$predScoreDiff.glm<-predict(fit.glm, type="response") 
train.updated$residual.glm<-train.updated$teamA_diff-train.updated$predScoreDiff.glm
plot(train.updated$residual.glm)
############################# RF MODEL RF MODEL RF MODEL #################
#make smaller trainign dataset for RF:
#trainRF<-regrank[1:round(0.10*nrow(regrank),0),]
#sample 5000 rows from updated dataframe for RF modeL:
trainRF<-train.updated[sample(nrow(train.updated), 20000),]
fit.rf<-randomForest(teamA_diff~teamA_fgm2.pct+teamA_fga23.rat+teamA_fgm3.pct+teamA_or.pct+teamA_dr.pct+teamA_shoot.prct+
                         teamA_stl+teamA_blk+teamA_poss.action.wt+teamA_poss.eff.wt+teamA_team_rank+
                         teamB_fgm2.pct+teamB_fga23.rat+teamB_fgm3.pct+teamB_shoot.prct+teamB_stl+
                         teamB_blk+teamB_poss.action.wt+teamB_poss.eff.wt+teamB_team_rank,data=trainRF,do.trace=20)

# 
# fit.RF<-randomForest(Wdiff~Wfgm2.pct+Wfga23.rat+Wfgm3.pct+Wor.pct+Wdr.pct+Wshoot.prct+Wstl+Wblk+
#                          Wposs.action.wt+Wposs.eff.wt+Wteam_rank+Lfgm2.pct+Lfga23.rat+Lfgm3.pct+Lor.pct+
#                          Ldr.pct+Lshoot.prct+Lstl+Lblk+Lposs.action.wt+Lposs.eff.wt+Lteam_rank,data=trainRF,do.trace=20)
varImpPlot(fit.rf,type=2)
trainRF$predScoreDiff.rf<-predict(fit.rf, type="response") 
trainRF$residual.rf<-trainRF$teamA_diff-trainRF$predScoreDiff.rf
plot(trainRF$residual.rf)

#now I need the season totals to feed into the model:
#team A vs Team B, then I'll have the correct output


#crunch mean stats for each team:
# meanStats.df<-aggregate(reg_long_stats[, -c(1,2,3,40)], list(reg_long_stats$team,reg_long_stats$Season), mean)

# grab all the rank data for the last week of each season
#finalRanksLose<-regrank[which(regrank$Week==15),c("weekkey_loser","Lteam_rank","Daynum","Season")]
# finalRanksLose<-regrank[,grep("L.*|Week|Season|Daynum|gameID", names(regrank))]
# # #names(finalRanksLose)<-c("Team","rank","Daynum","Season")
# # #finalRanksWin<-regrank[which(regrank$Week==15),c("weekkey_winner","Wteam_rank","Daynum","Season")]
# finalRanksWin<-regrank[,grep("W.*|Week|Season|Daynum|Wloc|gameID",names(regrank))]
# # #names(finalRanksWin)<-c("Team","rank","Daynum","Season")
# # #put all these into 1 df:
# finalRankAll <- rbind(finalRanksLose, finalRanksWin)
# #combine weekkey names with daynum:
# finalRanks.df<-reg_long_stats %>% group_by(team,Season)  %>% arrange(Daynum) %>% slice(n())
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

#read meanSeasonStats so I don't have to do some of the above:


# #Build the mode:
# train<-meanSeasonStats.df[1:round(0.75*nrow(meanSeasonStats.df),0),]
# test<-meanSeasonStats.df[(round(0.75*nrow(meanSeasonStats.df),0)+1):nrow(meanSeasonStats.df),]
# 
# 
# #read.csv(paste0(inpath, "TourneyDetailedResults.csv"), stringsAsFactors = FALSE, header = TRUE)
# fit.glm<-glm(score~fgm2.pct+fga23.rat+fgm3.pct+or.pct+dr.pct+shoot.prct+stl+blk+
#                 poss.action.wt+poss.eff.wt+team_rank,data=train)
# summary(fit.glm)
# fit.RF<-randomForest(score~fgm2.pct+fga23.rat+fgm3.pct+or.pct+dr.pct+shoot.prct+stl+blk+
#                 poss.action.wt+poss.eff.wt+team_rank,data=train)
# varImpPlot(fit.RF,type=2)
# # fitLose<-glm(Lscore~Lfgm2.pct+Lfga23.rat+Lfgm3.pct+Lor.pct+Ldr.pct+Lshoot.prct+Lstl+Lblk+Wblk+Wstl+
# #                  Lposs.action.wt+Lposs.eff.wt+Wposs.action.wt+Wposs.eff.wt+
# #                  Lteam_rank+Wteam_rank,data=train)                
# 
# #fitLose<-glm(Lscore~Ldr.pct)
# 
# 
# train$predScore.glm<-predict(fit.glm, type="response") 
# train$predScore.RF<-predict(fit.RF, type="response")
# #train$glmPredictLose<-predict(fitLose, type="response")
# #train$falseWin<-ifelse(train$glmPredictLose>=train$glmPredictWin,1,0)
# train$residual.glm<-train$score-train$predScore.glm
# train$residual.RF<-train$score-train$predScore.RF
# #accuracyTrain<-1-sum(train$falseWin)/nrow(train)
# 
# #run it with the test data:
#test$predScore.glm<-predict(object = fit.glm, newdata = test)
# test$predScore.RF<-predict(object = fit.RF, newdata = test)
# #test$glmPredictLose<-predict(object = fitLose, newdata = test)
# test$residual.glm<-test$score-test$predScore.glm
# test$residual.RF<-test$score-test$predScore.RF
#accuracyTest<-1-sum(test$falseWin)/nrow(test)

################# USE MODEL TO PREDICT RESULTS OF TOURNEY GAMES ###############
tourney<-read.csv(paste0(inpath, "TourneyDetailedResults.csv"), stringsAsFactors = FALSE, header = TRUE)
#open the tourney seeds:
tourneySeeds<-read.csv(paste0(inpath, "TourneySeeds.csv"), stringsAsFactors = FALSE, header = TRUE)
################create win probs based on seed:
#create game ID to link ending DFs together:
tourney$gameID<-paste(tourney$Season,tourney$Wteam,tourney$Lteam,sep="_")
#add Roundsto tounrey data:
tourney$Round<-NA
tourney$Round[grep("136|137",tourney$Daynum)]<-1
tourney$Round[grep("138|139",tourney$Daynum)]<-2
tourney$Round[grep("143|144",tourney$Daynum)]<-3
tourney$Round[grep("145|146",tourney$Daynum)]<-4
tourney$Round[grep("152",tourney$Daynum)]<-5
tourney$Round[grep("154",tourney$Daynum)]<-6

###Winning Team
tourneyWinners<-tourney[,c("Season","Wteam","gameID","Round")]
names(tourneyWinners)<-c("Season","Team","gameID","Round")
tourneyWinners<-merge(tourneyWinners,tourneySeeds,by=intersect(names(tourneyWinners),names(tourneySeeds)))
#remove letters from seeds:
tourneyWinners$Seed<-gsub("\\D","",tourneyWinners$Seed)
###Losing Team
tourneyLosers<-tourney[,c("Season","Lteam","gameID","Round")]
names(tourneyLosers)<-c("Season","Team","gameID","Round")
tourneyLosers<-merge(tourneyLosers,tourneySeeds,by=intersect(names(tourneyLosers),names(tourneySeeds)))
#remove letters from seeds:
tourneyLosers$Seed<-gsub("\\D","",tourneyLosers$Seed)
#merge on gameID:
tourneyProbs.df<-merge(tourneyWinners,tourneyLosers,by= c("gameID","Season","Round"))
#rename:
names(tourneyProbs.df)<-c("gameID","Season","Round","teamW","seedW","teamL","seedL")
#create column with both team's seeds:
tourneyProbs.df$seedMatchup<-paste0(tourneyProbs.df$seedW,"_",tourneyProbs.df$seedL)
#make Round numeric:
tourneyProbs.df$Round<-as.numeric(tourneyProbs.df$Round)
#grab frequency stats:
matchupFreqbyRound<-tourneyProbs.df %>% group_by(seedMatchup,Round) %>% summarise(Frequency = n())
#make frequencies relative to all like-matchups (can exclude Round)
matchupFreq<-tourneyProbs.df %>% group_by(seedMatchup) %>% summarise(Frequency = n())
matchupFreq$winProb<-NA
#create a list of matchups that have either never occured or the lower seed has alwayus lost
missingMatchups<-list()
for(i in 1:nrow(matchupFreq)){
    #grab index of opposite matchup:
    findMatchup<-paste0(substr(matchupFreq$seedMatchup[i],4,5),"_",substr(matchupFreq$seedMatchup[i],1,2))
    mirrorInd<-grep(findMatchup,matchupFreq$seedMatchup)
    #create probability based on all like-matchups:
    if(length(mirrorInd)>0){
        matchupFreq$winProb[i]<-matchupFreq$Frequency[i]/(matchupFreq$Frequency[i]+matchupFreq$Frequency[mirrorInd])
    }
    else{
        matchupFreq$winProb[i]<-matchupFreq$Frequency[i]/matchupFreq$Frequency[i]
        missingMatchups[[i]]<-data.frame(seedMatchup=findMatchup,Frequency=0,winProb=0)
    }
}
#combine all the missing matchups into one DF:
missingMatchups.df<-do.call(rbind,missingMatchups)
#combine this with matchupFreq df:
matchupFreq<-rbind(matchupFreq,missingMatchups.df)
#save matchup file:
saveRDS(matchupFreq,"C:/Users/jroberti/Git/mm2017/data2018/matchupProbabilities.rds")
# matchupFreq$Round<-as.integer(matchupFreq$Round)
# #how many games from each round are played:
# gamesPerRound<-table(na.omit(tourneyProbs.df$Round))
# #gamesPerRoundAdj<-c(gamesPerRound[1]/(32*8),gamesPerRound[2]/16,gamesPerRound[3]/4,gamesPerRound[4]/2,gamesPerRound[5]/16
# #get probability of wins:
# matchupFreq$freqPerYear<-matchupFreq$Frequency/length(unique(tourneyProbs.df$Season))
# matchupFreq$Prob<-0
# match
# #Round 1 probs
# matchupFreq$Prob[grep(1,matchupFreq$Round)]<-matchupFreq$Frequency[grep(1,matchupFreq$Round)]/(length(unique(tourneyProbs.df$Season))*4)
#     #((gamesPerRound[1]/length(unique(tourneyProbs.df$Season)))*4)
#         #gamesPerRound[grep("1",names(gamesPerRound))]
# 
# 
# 
# 
# probs_table<-sort(table(tourneyProbs.df$seedMatchup),decreasing =T)/(length(unique(tourneyProbs.df$Season))*4)
# 
# #convert to df:
# probsFinal.df<-data.frame(t(probs_table))
# probsFinal.df

#set the data up in the same way - ultimately I'll need the team ID, and season to predict points because I'll be 
#pulling the data from the respective season; then predict points for each team and find Win or los
modelNCAA<-function(stats.df, model, tourney.df, seeds, winProbs){
    #truncate the tourney data to start at 2003 (matches tourneyDetailedResults file)
    seeds<-seeds[which(seeds$Season>=tourney.df$Season[1]),]
    #browser()
    #split tourney.df in half:
    tourney.df1<-tourney.df[1:round(0.5*nrow(tourney.df),0),] #team A = winning, team B = losing
    #make team A = winning team, make Team B = Losing team (opposite of the train.mirror df)
    names(tourney.df1)<-gsub("^L","teamB_",names(tourney.df1))
    names(tourney.df1)<-gsub("^W","teamA_",names(tourney.df1))
    #fix Week and location column:
    #(trainWin)[colnames(tourney.df1) == 'teamA_eek'] <- 'Week'
    colnames(tourney.df1)[colnames(tourney.df1) == 'teamA_loc'] <- 'wloc'
    #make dataframe where team A and B are reversed 
    tourney.df2<-tourney.df[(round(0.5*nrow(tourney.df),0)+1):nrow(tourney.df),]
    #move Losing team data under Winning team data:
    tourney.df2.start<-tourney.df2[,grep("L.*",names(tourney.df2))]
    #call teams A and B and not Win and Lose:
    names(tourney.df2.start)<-gsub("^L","teamA_",names(tourney.df2.start))
    #make sequence for logic to grab non-L.* names:
    nameSeq<-1:length(names(tourney.df2))
    #grab winning team indicies:
    tourney.df2.end<-tourney.df2[,which(nameSeq %in% grep("L.*",names(tourney.df2))==FALSE)]
    #remove the first two columns:
    #trainLoseEnd<-trainLoseEnd[,-c(1,2)]
    #rename to team B:
    names(tourney.df2.end)<-gsub("^W","teamB_",names(tourney.df2.end))
    #fix Week column:
    #colnames(trainLoseEnd)[colnames(trainLoseEnd) == 'teamB_eek'] <- 'Week'
    colnames(tourney.df2.end)[colnames(tourney.df2.end) == 'teamB_loc'] <- 'wloc'
    #cbind the two dataframes together (TEAM A is losing team, TEAM B is winning team; this is opposite of train.win
    tourney.mirror<-cbind(tourney.df2.start,tourney.df2.end)
    #rbind the two above dataframes:
    tourney.updated<-rbind(tourney.df1,tourney.mirror)
    
    # tourney.df1<-
    # gsub("^L","teamB_",names(trainWin))
    #names(tourney.df)
    
    #crunch score differential for the tourney data relative to the Winning team
    tourney.updated$teamA_diff <- tourney.updated$teamA_score - tourney.updated$teamB_score
    #grab all the matchups:
    matchups<-tourney.updated[,c("Season","teamA_team","teamB_team")]
    #make empty list:
    df.tourney<-list()
    df.seed<-list()
    #browser()
    #use Season, and teamIDs in matchup to find seasonal team data and predict score differential:
    for(i in 1:nrow(matchups)){
        #find correct Season and teams in stats:
        seasonInd<-grep(matchups$Season[i],stats.df$season)
        team1Ind<-grep(matchups$teamA_team[i],stats.df$team)
        team2Ind<-grep(matchups$teamB_team[i],stats.df$team)
        #grab correct Season and teams in seeds:
        seasonIndSeed<-grep(matchups$Season[i],seeds$Season)
        team1IndSeed<-grep(matchups$teamA_team[i],seeds$Team)
        team2IndSeed<-grep(matchups$teamB_team[i],seeds$Team)
        #map correct season with correct teams in stats:
        team1Season<-meanSeasonStats.df[intersect(seasonInd,team1Ind),]
        #map correct season with correct teams in seeds:
        team1Season$seed<-as.numeric(gsub("\\D","",seeds$Seed[intersect(seasonIndSeed,team1IndSeed)]))
        #add "W" to all names:
        names(team1Season)<-paste0("teamA_",names(team1Season))
        team2Season<-meanSeasonStats.df[intersect(seasonInd,team2Ind),]
        #map correct season with correct teams in seeds:
        team2Season$seed<-as.numeric(gsub("\\D","",seeds$Seed[intersect(seasonIndSeed,team2IndSeed)]))
        #add "L to all names:
        names(team2Season)<-paste0("teamB_",names(team2Season))
        #Create dataframe with both teams' stats:
        df.tourney[[i]]<-cbind(team1Season,team2Season)
        df.seed[[i]]<-cbind(team1Season$teamA_seed,team2Season$teamB_seed)
    }
    out<-do.call(rbind,df.tourney)
    out.seed<-do.call(rbind,df.seed)
    
    #run the model:
    tourney.updated$teamA_diff.pred<-predict(object = model, newdata = out)
    #cbind the seeds:
    tourney.updated<-cbind(tourney.updated,out.seed)
    #change the names to teamA_seed and teamB_seed:
    colnames(tourney.updated)[colnames(tourney.updated) == '1'] <- 'teamA_seed'
    colnames(tourney.updated)[colnames(tourney.updated) == '2'] <- 'teamB_seed'
    #add binary to show if teamA won:
    tourney.updated$teamA_win<-ifelse(tourney.updated$teamA_score>tourney.updated$teamB_score,1,0)
    tourney.updated$teamA_win.pred<-ifelse(tourney.updated$teamA_diff.pred>0,1,0)
    #adjust prediction to account for point spread and team seed:
    #browser()
    tourney.updated$seedDiff<-tourney.updated$teamA_seed-tourney.updated$teamB_seed
    tourney.updated$teamA_win.predAdj<-tourney.updated$teamA_win.pred
    
    #browser()
    #make seed combo column:
    team1Matchup<-ifelse(nchar(tourney.updated$teamA_seed)==1,paste0("0",tourney.updated$teamA_seed),tourney.updated$teamA_seed)
    team2Matchup<-ifelse(nchar(tourney.updated$teamB_seed)==1,paste0("0",tourney.updated$teamB_seed),tourney.updated$teamB_seed)

    tourney.updated$seedMatchup<-paste0(team1Matchup,"_",team2Matchup)
    tourney.updated$teamA_win.histProb<-0
    #add weights:
    for(i in 1:nrow(tourney.updated)){
        #initial probabilities
        tourney.updated$teamA_win.histProb[i]<-winProbs$winProb[grep(tourney.updated$seedMatchup[i],winProbs$seedMatchup)]
        #adjust the winPred:
        if(tourney.updated$teamA_diff.pred[i]>=-1 & tourney.updated$teamA_seed[i]>tourney.updated$teamB_seed[i]){
            tourney.updated$teamA_win.predAdj[i]<-1   
        }
        
        #print(i)
    }
    #browser()
    #binary win solely based on seed probability wins from matchups:
    tourney.updated$teamA_winProbBinary<-NA
    tourney.updated$teamA_winProbBinary<-ifelse(tourney.updated$teamA_win.histProb>=0.5,1,0)
    #enemble results:
    tourney.updated$teamA_winProbFinal<-(((tourney.updated$teamA_win.pred+tourney.updated$teamA_win.predAdj+
                                            tourney.updated$teamA_winProbBinary)/3)+(2*tourney.updated$teamA_win.histProb))/3
    #finalBinary:
    tourney.updated$teamA_winProbBinaryAdj<-ifelse(tourney.updated$teamA_winProbFinal>=0.5,1,0)
    
    
    
    
    #browser()
    
    #output data frame:
    return(tourney.updated)
}
#run it!
results<-modelNCAA(stats.df=meanSeasonStats.df,model=fit.rf,tourney.df=tourney, 
                   seeds=tourneySeeds,winProbs = matchupFreq)
#accuracy of non-adjusted model:
length(which(results$teamA_win==results$teamA_win.pred))/nrow(results)
#accuracy of sed-adjusted model:
length(which(results$teamA_win==results$teamA_win.predAdj))/nrow(results)
#accuracy of historic probabilities:
length(which(results$teamA_win==results$teamA_winProbBinary))/nrow(results)


#log loss info:
library(MLmetrics)
LogLoss(results$teamA_winProbFinal, results$teamA_win)
LogLoss(results$teamA_win.histProb, results$teamA_win)
# logLoss<- sum(results$teamA_win*log(results$teamA_winProbFinal)+(1-results$teamA_win)+log(1-results$teamA_winProbFinal))
# #(-1/nrow(results))*

#fit the model to the 2018 data:
inpath2<-gsub("data","data2018",inpath)
reg2018 <-read.csv(paste0(inpath2, "RegularSeasonDetailedResults.csv"), stringsAsFactors = FALSE, header = TRUE)
names(reg2018) <- tolower(names(reg2018))
submission_target <- c(2018)
## arrange by lowest Wteam, as that is how kaggle wants the IDs arranged (lowest team first)
reg2018 <- dplyr::arrange(reg2018, season, daynum, wteamid, lteamid) %>% dplyr::rename(wteam=wteamid, lteam=lteamid) %>% filter(season == submission_target)
team2018 <- read.csv(paste0(inpath2, "Teams.csv"), stringsAsFactors = FALSE)
seeds2018 <- read.csv(paste0(inpath2, "NCAATourneySeeds.csv"), stringsAsFactors = FALSE)
seeds2018$uid <- paste0(seeds2018$Season,"_",seeds2018$Team)
seeds2018$Seed_num <- as.integer(gsub("^W|^X|^Y|^Z|a|b","", seeds2018$Seed))
reg2018$gameid <- paste0(reg2018$season, "_", reg2018$wteam, "_", reg2018$lteam)

#add ancillary stats:
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
#run data thru ranking system:
ranker2 <- function(data){
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
    data$Outcome <- ifelse(data$wscore - data$lscore>0, 1, 0)
    data$Week <- sapply(data$daynum, assign_week, week_idx)
    data$weekkey_winner <- paste0(data$wteam,"_",data$Week)
    data$weekkey_loser <- paste0(data$lteam,"_",data$Week)
    
    # this uses the Daynum converted to week
    ranks <- steph(select(data, Week, wteam, lteam, Outcome), history=TRUE)
    
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
    reg_weekly_ranks <- rename(reg_weekly_ranks, wteam_rank=value) %>% select(-player, -type, -week)
    reg_weekly_ranks <- merge(reg_weekly_ranks, weekly_ranks, by.x=c("weekkey_loser"), by.y=c("weekkey"))
    reg_weekly_ranks <- rename(reg_weekly_ranks, lteam_rank=value) %>% select(-player, -type, -week)
    return(reg_weekly_ranks)
}

reg2018 <-  plyr::ddply(reg2018, .(season), function(x) {ranker2(x)})

reg2018$weekkey_loser <- NULL
reg2018$weekkey_winner <- NULL

"weekkey_loser" %in% names(reg2018)

regrank2018<-reg2018[,-grep("wloc|Week",names(reg2018))]
#find all "winning team columns"
winningTeamStats2018<-regrank2018[,grep("^w.*|season",names(regrank2018))]
losingTeamStats2018<-regrank2018[,grep("^l.*|Week|season",names(regrank2018))]
#for both, make sure the names are identical, so remove the "W" and "L" for each df respectively:
names(winningTeamStats2018)<-gsub("^w","",names(winningTeamStats2018))
names(losingTeamStats2018)<-gsub("^l","",names(losingTeamStats2018))
#rbind these DFs together:
finalStatsAll2018 <- rbind(winningTeamStats2018, losingTeamStats2018)
#get team averages of numeric columns:
#meanSeasonStats.df<-finalStatsAll %>% group_by(team,Season)  %>% mean(or.pct,na.rm=T)
#d %>% group_by(Name) %>% summarise_at(vars(-Month), funs(mean(., na.rm=TRUE)))
meanSeasonStats2018.df<-aggregate(finalStatsAll2018[, -c(1,2)], list(finalStatsAll2018$team,finalStatsAll2018$season), mean)

colnames(meanSeasonStats2018.df)[colnames(meanSeasonStats2018.df) == 'Group.1'] <- 'team'
colnames(meanSeasonStats2018.df)[colnames(meanSeasonStats2018.df) == 'Group.2'] <- 'season'
saveRDS(meanSeasonStats2018.df,"C:/Users/jroberti/Git/mm2017/data2018/meanSeasonStats2018.rds")

#grab 2018 tourney stuff:
seeds2018 <- read.csv(paste0(inpath2, "NCAATourneySeeds.csv"), stringsAsFactors=FALSE)
seeds2018 <- dplyr::filter(seeds2018, Season == 2018)

## this should result in 68 rows i.e. the 68 teams playing in the tourny
#submission_data <- reg2018_pp_5w %>% filter(team %in% seeds2018$TeamID) %>% arrange(desc(daynum)) %>% group_by(team) %>%  slice(n()) 

# stolen from: https://stackoverflow.com/questions/17171148/non-redundant-version-of-expand-grid
expand.grid.unique <- function(x, y, include.equals=FALSE)
{
    x <- unique(x)
    
    y <- unique(y)
    
    g <- function(i)
    {
        z <- setdiff(y, x[seq_len(i-include.equals)])
        
        if(length(z)) cbind(x[i], z, deparse.level=0)
    }
    
    do.call(rbind, lapply(seq_along(x), g))
}

all_matchups <- expand.grid.unique(seeds2018$TeamID, seeds2018$TeamID)
all_matchups <- data.frame(all_matchups)
names(all_matchups) <- c("x", "y")

all_matchups <- arrange(all_matchups, x)
all_matchups$id <- paste0("2018_", all_matchups$x, "_", all_matchups$y)

############ USE MATCHUP FILE FROM EMAIL:
all_matchups<-read.csv("C:/Users/jroberti/Git/mm2017/data2018/all_tournament_matchups_2018.csv",
                       header=T,stringsAsFactors = F)


#merge(x = all_matchups, y = reg2018_pp_5w) 
NCAAmodel2018<-function(stats.df, model, tourney.df, seeds, winProbs){
    df.tourney<-list()
    df.seed<-list()
    #browser()
    matchups<-tourney.df[,c("team_x","team_y")]
    matchups$Season<-substr(tourney.df$ID,0,4)
    names(matchups)<-c("teamA_team","teamB_team","Season")
    
    #browser()
    #use Season, and teamIDs in matchup to find seasonal team data and predict score differential:
    for(i in 1:nrow(matchups)){
        #find correct Season and teams in stats:
        seasonInd<-grep(matchups$Season[i],stats.df$season)
        team1Ind<-grep(matchups$teamA_team[i],stats.df$team)
        team2Ind<-grep(matchups$teamB_team[i],stats.df$team)
        #grab correct Season and teams in seeds:
        seasonIndSeed<-grep(matchups$Season[i],seeds$Season)
        team1IndSeed<-grep(matchups$teamA_team[i],seeds$Team)
        team2IndSeed<-grep(matchups$teamB_team[i],seeds$Team)
        #map correct season with correct teams in stats:
        team1Season<-meanSeasonStats.df[intersect(seasonInd,team1Ind),]
        #map correct season with correct teams in seeds:
        team1Season$seed<-as.numeric(gsub("\\D","",seeds$Seed[intersect(seasonIndSeed,team1IndSeed)]))
        #add "W" to all names:
        names(team1Season)<-paste0("teamA_",names(team1Season))
        team2Season<-meanSeasonStats.df[intersect(seasonInd,team2Ind),]
        #map correct season with correct teams in seeds:
        team2Season$seed<-as.numeric(gsub("\\D","",seeds$Seed[intersect(seasonIndSeed,team2IndSeed)]))
        #add "L to all names:
        names(team2Season)<-paste0("teamB_",names(team2Season))
        #Create dataframe with both teams' stats:
        df.tourney[[i]]<-cbind(team1Season,team2Season)
        df.seed[[i]]<-cbind(team1Season$teamA_seed,team2Season$teamB_seed)
    }
    out<-do.call(rbind,df.tourney)
    out.seed<-do.call(rbind,df.seed)
    #browser()
    
    
    #run the model:
    teamA_diff.pred<-predict(object = model, newdata = out)
    #cbind the seeds:
    tourney.updated<-cbind(tourney.df,out.seed)
    #change the names to teamA_seed and teamB_seed:
    colnames(tourney.updated)[colnames(tourney.updated) == '1'] <- 'teamA_seed'
    colnames(tourney.updated)[colnames(tourney.updated) == '2'] <- 'teamB_seed'
    #add binary to show if teamA won:
    #tourney.updated$teamA_win<-ifelse(tourney.updated$teamA_score>tourney.updated$teamB_score,1,0)
    tourney.updated$teamA_win.pred<-ifelse(teamA_diff.pred>0,1,0)
    #adjust prediction to account for point spread and team seed:
    #browser()
    tourney.updated$seedDiff<-tourney.updated$teamA_seed-tourney.updated$teamB_seed
    tourney.updated$teamA_win.predAdj<-tourney.updated$teamA_win.pred
    
    #browser()
    #make seed combo column:
    team1Matchup<-ifelse(nchar(tourney.updated$teamA_seed)==1,paste0("0",tourney.updated$teamA_seed),tourney.updated$teamA_seed)
    team2Matchup<-ifelse(nchar(tourney.updated$teamB_seed)==1,paste0("0",tourney.updated$teamB_seed),tourney.updated$teamB_seed)
    
    tourney.updated$seedMatchup<-paste0(team1Matchup,"_",team2Matchup)
    tourney.updated$teamA_win.histProb<-0
    #browser()
    #add weights:
    for(i in 1:nrow(tourney.updated)){
        #initial probabilities
        matchupFind<-grep(tourney.updated$seedMatchup[i],winProbs$seedMatchup)
        if(length(matchupFind)==0){
            tourney.updated$teamA_win.histProb[i]<-0.5-(tourney.updated$seedDiff[i]/30) #set to 50/50 chance
        }
        else{
            tourney.updated$teamA_win.histProb[i]<-winProbs$winProb[grep(tourney.updated$seedMatchup[i],winProbs$seedMatchup)]
            
        }
        #tourney.updated$teamA_win.histProb[i]<-winProbs$winProb[grep(tourney.updated$seedMatchup[i],winProbs$seedMatchup)]
        #adjust the winPred:
        # if(tourney.updated$teamA_diff.pred[i]>=-1 & tourney.updated$teamA_seed[i]>tourney.updated$teamB_seed[i]){
        #     tourney.updated$teamA_win.predAdj[i]<-1   
        # }
        
        #print(i)
    }
    #browser()
    #binary win solely based on seed probability wins from matchups:
    tourney.updated$teamA_winProbBinary<-NA
    tourney.updated$teamA_winProbBinary<-ifelse(tourney.updated$teamA_win.histProb>=0.5,1,0)
    #enemble results:
    tourney.updated$teamA_winProbFinal<-(((tourney.updated$teamA_win.pred+tourney.updated$teamA_win.predAdj+
                                               tourney.updated$teamA_winProbBinary)/3)+(2*tourney.updated$teamA_win.histProb))/3
    #finalBinary:
    tourney.updated$teamA_winProbBinaryAdj<-ifelse(tourney.updated$teamA_winProbFinal>=0.5,1,0)
    return(tourney.updated)
    
    
}
results2018<-NCAAmodel2018(stats.df = meanSeasonStats2018.df,model=fit.rf,tourney.df=all_matchups, 
                           seeds=seeds2018,winProbs = matchupFreq)
intersect(names(results2018),names(all_matchups))
#merge probabilities to the submission file:
submissionFile<-merge(all_matchups,results2018,by=intersect(names(results2018),names(all_matchups)))
#remove columns that aren't necessary:
submissionFile<-submissionFile[,c("ID","teamA_win.histProb")]
#change column names:
names(submissionFile)<-c("id","pred")
#save the file:
write.csv(submissionFile,"C:/Users/jroberti/Git/mm2017/data2018/RobertiSubmission.csv",row.names = F)
