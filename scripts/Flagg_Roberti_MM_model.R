# The Flagg-Roberti MM Model
library(plyr)
library(dplyr)
library(stringr)
library(PlayerRatings)
library(car)
library(randomForest)
library(tidyr)


#Open the datasets:
inpath <- "C:/Users/jroberti/Git/mm2017/data/"
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

#graph it:
Wteam.eff.wt<-density(reg$Wposs.eff.wt)
Lteam.eff.wt<-density(reg$Lposs.eff.wt)
plot(Wteam.eff.wt, main="Possession Efficiency",xlim=c(0,1),ylim=c(0,6))
polygon(Wteam.eff.wt,col="green")
lines(Lteam.eff.wt)
polygon(Lteam.eff.wt,col="red")
#add means as vertical lines:
abline(v=mean(reg$Wposs.eff.wt))
abline(v=mean(reg$Lposs.eff.wt))

#efficiency differences (using weighted stats for this):
reg$Wposs.eff.wt.diff<-reg$Wposs.eff.wt-reg$Lposs.eff.wt
reg$Lposs.eff.wt.diff<-reg$Lposs.eff.wt-reg$Wposs.eff.wt
plot(density(reg$Wposs.eff.wt.diff))
lines(density(reg$Lposs.eff.wt.diff))

#subset data - only want data past day #30 for each season to avoid lumping preseason etc.
reg.sub.train<-reg[which(reg$Daynum>30 & reg$Season>2005 & reg$Season<2014),]
#tourney<-read.csv(paste0(inpath, "TourneyDetailedResults.csv"), stringsAsFactors = FALSE, header = TRUE)
reg.sub.test<-reg[which(reg$Daynum>30 & reg$Season>=2014),]

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
regrank <-  plyr::ddply(reg.sub.train, .(Season), function(x) {ranker(x)})

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
set.seed(1234)
season.dist.win<-apply(combinedStats[,c("wt.diff.mean.win.adj","wt.sd.win","N.win.rat")], 1, 
                          function(x) rnorm(1000*x[3],x[1],x[2]))
season.dist.loss<-apply(combinedStats[,c("wt.diff.mean.loss.adj","wt.sd.loss","N.loss.rat")], 1, 
                   function(x) rnorm(1000*x[3],x[1],x[2]))
#now, combine the distributions:
final.dists<-apply( unname(cbind( season.dist.win, season.dist.loss )) , 1 , unlist )
#name the nested vectors by team ID:
names(final.dists)<-paste(combinedStats$Season,combinedStats$Team,sep=".")
#test the distributions:
hist(final.dists$`2010.1250`)
winning.prob<-function(dist.1,dist.2){
    team1<-sample(dist.1,1)
    team2<-sample(dist.2,1)
    result<-team1-team2
    winner<-ifelse(team1>team2,1,2)
   # output<-c(winner,result)
    return(winner)
}
game.simulation<-replicate(n=10000,winning.prob(dist.1=final.dists$`2010.1393`,dist.2=final.dists$`2010.1293`))
hist(game.simulation,ylim=c(0,10000))
length(which(game.simulation==1))
length(which(game.simulation==2))


m1.prct <- lm(N.win ~ ., data = combinedStats)

summary(m1.prct)

################ GLM FIT #################
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
#merge these with combined Stats:
NCAA.df<-merge(finalRanks.df,combinedStats,by=intersect(names(finalRanks.df),names(combinedStats)))
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
#ddply(finalStatsAll, .(team,Season), summarize,  mean(team_rank))

# 
#  numericCols<-unname(which(unlist(apply(finalStatsAll, 2, function(x) is.numeric(x)))))
# meanSeasonStats.df<-aggregate(. ~ c(team,Season), finalStatsAll[numericCols], mean)
# 

#create Losing team's location (so data frames are equal):
# losingTeamStats$Lloc<-rep("N",nrow(losingTeamStats)) #prefill with neutral court
# losingTeamStats$Lloc[grep("H",winningTeamStats$Wloc)]<-"A" #if winning team was home put losing team as away
# losingTeamStats$Lloc[grep("A",winningTeamStats$Wloc)]<-"H"
                     
avgStats<-finalRankAll <- rbind(winningTeamStats, losingTeamStats)

finalRankAll <- plyr::rbind.fill(winningTeamStats, losingTeamStats)

aggregate(regrank[,3:4], list(d$Name), mean)





library(pscl)
train <- combinedStats[1:round(nrow(combinedStats)*0.725,0),] #make training data
test <- combinedStats[(round(nrow(combinedStats)*0.725,0)+1):nrow(combinedStats),] #make test data
fit<-glm(N.win~.,data=train)
summary(fit) # display results
anova(fit, test="Chisq") #ANOVA 
pR2(fit) # assess the fit of the model
library(ROCR)
p <- predict(fit, newdata=test)
pr <- prediction(p, test$N.win)
prf <- performance(pr, measure = "tpr", x.measure = "fpr")
plot(prf)


confint(fit) # 95% CI for the coefficients
exp(coef(fit)) # exponentiated coefficients
exp(confint(fit)) # 95% CI for exponentiated coefficients
predict(fit, type="response") # predicted values
residuals(fit, type="deviance") # residuals



#weight the data by elo ranking:
plot(density(regrank$Wposs.eff.wt.diff))
lines(density(regrank$Lposs.eff.wt.diff))

#create adjusted possession efficiency based on team rank and opponent rank:
#first, create elo differnce ratio for each game:
regrank$Wteam_rank_rat<-regrank$Wteam_rank/max(regrank$Wteam_rank)
#still going to use the maximum elo ranking from W side for denomonator:
regrank$Lteam_rank_rat<-regrank$Lteam_rank/max(regrank$Wteam_rank)
#ranking differences:
#regrank$Wteam_rank_rat_diff<-regrank$Wteam_rank_rat-regrank$Lteam_rank_rat
#adjusted possession efficiency:
regrank$Wposs.eff.adj<-regrank$Wposs.eff.wt*regrank$Wteam_rank_rat
regrank$Lposs.eff.adj<-regrank$Lposs.eff.wt*regrank$Lteam_rank_rat

#get the max ELO rating for each week:
maxElo.W.week<-ddply(regrank,c('Week'),function(x) x[which(x$Wteam_rank_rat==max(x$Wteam_rank_rat)),c("Week","Wteam_rank")])
#remove duplicate rows: some weeks are coming up duplicated for some reason:
maxElo.W.week<-maxElo.W.week[!duplicated(maxElo.W.week), ]
#maxmimum losing team check: 
maxElo.L.week<-ddply(regrank,c('Week'),function(x) x[which(x$Lteam_rank_rat==max(x$Lteam_rank_rat)),c("Week","Lteam_rank")])

######################adjust possession statistics##########################
#numer of possessions:
# reg$Wposs<-reg$Wfga2+reg$Wfga3+reg$Wfta+reg$Wto   #multiply the turnovers by the shooting % of L team to get a more accurate picture of how detrimental the turnovers are.  Also, on the other end, multiple the defensive rbs by the shooting percentage of W team to see how advantageous the defensive rbs are...
# reg$Wposs.action<-(reg$Wfgm2+reg$Wfgm3+reg$Wftm-(reg$Wto*(reg$Lshoot.prct/100))+(reg$Wdr*(reg$Wshoot.prct/100)))
#reg$Wposs.eff<-reg$Wposs.action/reg$Wposs 
#using weighted shooting %
#reg$Wposs.action.wt<-(reg$Wfgm2+reg$Wfgm3+reg$Wftm-(reg$Wto*(reg$Lshoot.prct.wt/100))+(reg$Wdr*(reg$Wshoot.prct.wt/100)))
#possession efficiency:
#test using week 10:
reg.week10<-regrank[which(regrank$Week==10),]
reg.week10.previous<-regrank[which(regrank$Week==10),]
#adjust the winning team possession efficiency via the losing team's rank:
reg.week10$Wposs.eff.wt<-(reg.week10$Wposs.action.wt/reg.week10$Wposs)* (1-(reg.week10$Lteam_rank/maxElo.W.week[which(maxElo.W.week$Week==10),"Wteam_rank"]))
#adjust the losing team possession efficiency via winning team's rank:
reg.week10$Lposs.eff.wt<-(reg.week10$Lposs.action.wt/reg.week10$Lposs)* (1-(reg.week10$Wteam_rank/maxElo.W.week[which(maxElo.W.week$Week==10),"Wteam_rank"]))
#difference:
reg.week10$Wposs.eff.wt.dif<-reg.week10$Wposs.eff.wt-reg.week10$Lposs.eff.wt
reg.week10$Lposs.eff.wt.dif<-reg.week10$Lposs.eff.wt-reg.week10$Wposs.eff.wt
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
plot(Wteam.eff.wt, main="Possession Efficiency",xlim=c(0,1),ylim=c(0,6))
polygon(Wteam.eff.wt,col="green")
lines(Lteam.eff.wt)
polygon(Lteam.eff.wt,col="red")
#add means as vertical lines:
abline(v=mean(reg$Wposs.eff.wt))
abline(v=mean(reg$Lposs.eff.wt))

#efficiency differences (using weighted stats for this):
reg$Wposs.eff.wt.diff<-reg$Wposs.eff.wt-reg$Lposs.eff.wt
reg$Lposs.eff.wt.diff<-reg$Lposs.eff.wt-reg$Wposs.eff.wt
plot(density(reg$Wposs.eff.wt.diff))
lines(density(reg$Lposs.eff.wt.diff))




