#exploratory analysis with mm2017 data:
#read in data:
inpath <- "C:/Users/jroberti/Git/mm2017/data/"
#"C:/Users/Amy/Documents/GitHub/mm2017/data/"

# reg <- read.csv(paste0(inpath, "RegularSeasonCompactResults.csv"), stringsAsFactors = FALSE)
# 
# team <- read.csv(paste0(inpath, "Teams.csv"), stringsAsFactors = FALSE)
# seasons <- read.csv(paste0(inpath, "Seasons.csv"), stringsAsFactors = FALSE)
# 
# tourney <- read.csv(paste0(inpath, "TourneyCompactResults.csv"), stringsAsFactors = FALSE)
#grab detailed results:
reg.details<-read.csv(paste0(inpath, "RegularSeasonDetailedResults.csv"), stringsAsFactors = FALSE, header = TRUE)
#tourney.details<-read.csv(paste0(inpath, "TourneyDetailedResults.csv"), stringsAsFactors = FALSE, header = TRUE)


#create adjusted shooting stats:  (fg = 2 or 3 pointer...)  let's partition 2 and 3 pointers:
######### WINNING TEAM #############
#2-pointers attempted:
reg.details$Wfga2<-reg.details$Wfga-reg.details$Wfga3
#2-pointers made:
reg.details$Wfgm2<-reg.details$Wfgm-reg.details$Wfgm3
#2-point made %
reg.details$Wfgm2.pct<-reg.details$Wfgm2/reg.details$Wfga2
#2/3 point ratio attempts
reg.details$Wfga23.rat<-reg.details$Wfga2/reg.details$Wfga3

#ft made %
reg.details$Wftm.pct<-reg.details$Wftm/reg.details$Wfta

#3-point made %
reg.details$Wfgm3.pct<-reg.details$Wfgm3/reg.details$Wfga3

#create a score check to make sure the fgm2, fgm3, and ftm add up to respective score:  #check should be 0
W.score.check<-reg.details$Wscore-((reg.details$Wfgm2*2)+(reg.details$Wfgm3*3)+reg.details$Wftm)

######### LOSING TEAM ##############
#2-pointers attempted:
reg.details$Lfga2<-reg.details$Lfga-reg.details$Lfga3
#2-pointers made:
reg.details$Lfgm2<-reg.details$Lfgm-reg.details$Lfgm3
#2-point made %
reg.details$Lfgm2.pct<-reg.details$Lfgm2/reg.details$Lfga2
#2/3 point ratio attempts
reg.details$Lfga23.rat<-reg.details$Lfga2/reg.details$Lfga3

#ft made %
reg.details$Lftm.pct<-reg.details$Lftm/reg.details$Lfta

#3-point made %
reg.details$Lfgm3.pct<-reg.details$Lfgm3/reg.details$Lfga3

#create a score check to make sure the fgm2, fgm3, and ftm add up to respective score:  #check should be 0
L.score.check<-reg.details$Lscore-((reg.details$Lfgm2*2)+(reg.details$Lfgm3*3)+reg.details$Lftm)

#find indices where fgm2 + fgm3 + ftm don't add up to respective score and remove from dataset:
bad.scoreSheet<-which(W.score.check!=0 | L.score.check!=0) 
#remove these indices from record if record is bad:
if(length(bad.scoreSheet)>0){
    reg.details<-reg.details[-bad.scoreSheet,]
}

#create metric for number of rebounds actually possible.  As a rough estimate I'm going to divide (fta-ftm) by 2 and round up because usually half of the freethrows will not have a rebound possible since the ball is dead after the first shot
#winning team attempted offensive rebounds = losing team attempted defensive rebounds
reg.details$Wor.a<-reg.details$Wor+reg.details$Ldr
reg.details$Wor.pct<-reg.details$Wor/reg.details$Wor.a

reg.details$Ldr.a<-reg.details$Wor.a
reg.details$Ldr.pct<-reg.details$Ldr/reg.details$Ldr.a
    # tried calculating this way the first time, but doesn't really add up: 
    # reg.details$Wfga-reg.details$Wfgm)+ (round((reg.details$Wfta-reg.details$Wftm)/2,0))

#Losing team attempted offensive rebounds = winning team attempted defensive rebounds
#reg.details$Lor.a<-(reg.details$Lfga-reg.details$Lfgm)+ (round((reg.details$Lfta-reg.details$Lftm)/2,0))
reg.details$Lor.a<-reg.details$Lor+reg.details$Wdr
reg.details$Lor.pct<-reg.details$Lor/reg.details$Lor.a

reg.details$Wdr.a<-reg.details$Lor.a 
reg.details$Wdr.pct<-reg.details$Wdr/reg.details$Wdr.a
#check the data: #Wor.a should roughly equal Wor+Ldr.  Might be off by a few since I've made the ft assumption above:
#reg.details$Wor.a.check<-reg.details$Wor.a - (reg.details$Wor+reg.details$Ldr)


# partition steals from the turnover stat:  A steal is the ONLY turnover where play continues. Play "stops" with all other turnovers, e.g., double dribble, ball out of bounds, backcourt violation, etc., which in turn allows both teams to reset and reposition..... a note on turnovers:  http://forums.rotoworld.com/topic/269757-how-all-statistics-are-counted-a-primer/
#Turnovers: "A turnover is any mistake caused by an offensive player that gives the defensive team possession of the ball without taking a field goal attempt. Having your shot blocked is not a turnover, as that is a field goal attempt." 
reg.details$Wto.ns<-reg.details$Wto-reg.details$Lstl
reg.details$Lto.ns<-reg.details$Lto-reg.details$Wstl

## let's create a block % for each team:
reg.details$Wblk.pct<-reg.details$Wblk/reg.details$Lfga
reg.details$Lblk.pct<-reg.details$Lblk/reg.details$Wfga


#create differential statistics:
#grab all Winning team statistics:
Wteam<-reg.details[,grep("W.*",names(reg.details))]
Lteam<-reg.details[,grep("L.*",names(reg.details))]

#create differential stats:
reg.details.diff<- Wteam[,c(2,4:18,23,25,27)]-Lteam[,c(2:17,22,24,26)]
#rename:
names.diff<-gsub("W","diff.",names(reg.details.diff))
names(reg.details.diff)<-names.diff

#create differential stats:
reg.details.diff<- Wteam[,c(2,4:27)]-Lteam[,c(2:26)]
#rename:
names.diff<-gsub("W","diff.",names(reg.details.diff))
names(reg.details.diff)<-names.diff
#keep non-duplicated stats:
reg.details.diff2<-reg.details.diff[,c(1,4:16)]

#cbind other relevant info:
reg.details.diff3<-cbind(reg.details$Wloc,reg.details.diff2)
names(reg.details.diff3)[names(reg.details.diff3) == "reg.details$Wloc"] <- "Wloc"


#create the training dataset (2003-2013)
reg.train<-reg.details.diff3[which(reg.details$Season<=2013),]
#reg.train.test<-reg.train[1:1000,c("diff.score","diff.fgm","diff.or","diff.dr","diff.blk","Wloc")]
reg.train$Wloc<-as.factor(reg.train$Wloc)


m1 <- lm(diff.score ~ ., data = reg.train)
summary(m1)

#apply a random forest:
modFit <- caret::train(diff.score~ .,data=reg.train.test,method="rf",prox=TRUE)
modFit
