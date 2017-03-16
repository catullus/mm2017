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

#compare 2-pointer and 3-pointer ratio
two.three.rat.compare<-reg.details$Wfga23.rat-reg.details$Lfga23.rat

#come up with a random stat for fun and see if it's a good predictor:
nervous.stat.w<-(reg.details$Wfga23.rat*reg.details$Wto)
nervous.stat.l<-(reg.details$Lfga23.rat*reg.details$Lto)

#summarize data for each team:
data.melt <- reshape2::melt(reg.details,id.vars=c("Season","Wteam"),
                            measure.vars=c("Wfgm2.pct","Wfgm3.pct","Wfga23.rat","Wftm.pct",
                                           "Wor.pct","Wdr.pct","Wto"), 
                            variable.name="Stats")

#summary stats:
summary.results<-plyr::ddply(data.melt, c("Season", "Wteam"),plyr::summarize,
                       mean = mean(value), sd = sd(value))

dfx <- data.frame(
    group = c(rep('A', 8), rep('B', 15), rep('C', 6)),
    sex = sample(c("M", "F"), size = 29, replace = TRUE),
    age = runif(n = 29, min = 18, max = 54)
)

library(plyr)
test<-ddply(dfx, .(group, sex), summarize,
            mean = round(mean(age), 2),
            sd = round(sd(age), 2))


## shot attmempt type % (percentage of shot type vs all shots):
#winning team:
reg.details$Wfga3.pct.tot<-reg.details$Wfga3/(reg.details$Wfga3+reg.details$Wfga2+reg.details$Wfta)
reg.details$Wfga2.pct.tot<-reg.details$Wfga2/(reg.details$Wfga3+reg.details$Wfga2+reg.details$Wfta)
reg.details$Wfta.pct.tot<-reg.details$Wfta/(reg.details$Wfga3+reg.details$Wfga2+reg.details$Wfta)
#losing team
reg.details$Lfga3.pct.tot<-reg.details$Lfga3/(reg.details$Lfga3+reg.details$Lfga2+reg.details$Lfta)
reg.details$Lfga2.pct.tot<-reg.details$Lfga2/(reg.details$Lfga3+reg.details$Lfga2+reg.details$Lfta)
reg.details$Lfta.pct.tot<-reg.details$Lfta/(reg.details$Lfga3+reg.details$Lfga2+reg.details$Lfta)

reg.details$Wto.pct<-reg.details$Wto/(reg.details$Wto+reg.details$Lto)
reg.details$Lto.pct<-reg.details$Lto/(reg.details$Wto+reg.details$Lto)
#### come up with a weighted shot efficiency metric:
reg.details$W.weightedScore<-((reg.details$Wfgm3*3)/reg.details$Wfga3.pct.tot)+
    ((reg.details$Wfgm2*2)/reg.details$Wfga2.pct.tot)+
    (reg.details$Wftm/reg.details$Wfta.pct.tot)*
    (0.5*reg.details$Wor.pct+0.3*reg.details$Wdr.pct-0.1*reg.details$Wto.pct)
#add something here where I can multiply these results by some sort of ELO rating weight.

reg.details$L.weightedScore<-((reg.details$Lfgm3*3)/reg.details$Lfga3.pct.tot)+
    ((reg.details$Lfgm2*2)/reg.details$Lfga2.pct.tot)+
    (reg.details$Lftm/reg.details$Lfta.pct.tot)*
    (0.5*reg.details$Lor.pct+0.3*reg.details$Ldr.pct-0.1*reg.details$Lto.pct)
#add something here where I can multiply these results by some sort of ELO rating weight.


# 
# reg.details$L.weightedScore<-((reg.details$Lfgm3*3)/reg.details$Lfga3.pct.tot)+
#     ((reg.details$Lfgm2*2)/reg.details$Lfga2.pct.tot)+((reg.details$Lftm)/reg.details$Lfta.pct.tot)
#look at it:
plot(testing$W.weightedScore-testing$L.weightedScore,ylim=c(0,100),col="green")
points(testing$Wscore-testing$Lscore,col="blue")

# reg.details$L.weightedScore<-(reg.details$Lfga3.pct.tot*reg.details$Lfgm3*3)+
#     (reg.details$Lfga2.pct.tot*reg.details$Lfgm2*2)+(reg.details$Lfta.pct.tot*reg.details$Lftm)
testing<-reg.details[c("W.weightedScore","Wscore","L.weightedScore","Lscore")]
plot(testing$W.weightedScore,col="green")
points(testing$L.weightedScore,col="blue")

hist(testing$W.weightedScore)
hist(testing$L.weightedScore)
hist(testing$Wscore)
hist(testing$Lscore)

mean(testing$W.weightedScore,na.rm = TRUE)
sd(testing$W.weightedScore,na.rm = TRUE)
mean(testing$L.weightedScore,na.rm = TRUE)
sd(testing$L.weightedScore,na.rm = TRUE)

mean(testing$Wscore,na.rm = TRUE)
sd(testing$Wscore,na.rm = TRUE)
mean(testing$Lscore,na.rm = TRUE)
sd(testing$Lscore,na.rm = TRUE)

#GOOD DEFENSE - forcing teams to the outside to shoot 3s:
#First, find avg 2-point/3-point attempt ratio:
two.three.rat<-c(reg.details$Wfga23.rat,reg.details$Lfga23.rat)
fgm3.prct.tot<-c(reg.details$Wfgm3.pct,reg.details$Lfgm3.pct)
or.prct.tot<-c(reg.details$Wor.pct,reg.details$Lor.pct)
dr.prct.tot<-c(reg.details$Wdr.pct,reg.details$Ldr.pct)
#using average 2:3 point ratio - 1 SD, create cutoffs:
cutoff.fga23a.rat<-mean(two.three.rat)-sd(two.three.rat)
cutoff.frm3.prct<-mean(fgm3.prct.tot)-sd(fgm3.prct.tot)
cutoff.or.prct<-mean(or.prct.tot)-sd(or.prct.tot)
cutoff.dr.prct<-mean(dr.prct.tot)-sd(dr.prct.tot)
#look into rebound data:

#looking for teams with a "low" 2:3 point attempt ratio and a low fgm3 point prct (%).  
#This should show that these teams are being forced to shoot 3's and can't get in below the arc
#which may indicate poor offense / good defense.  Maybe we should also investigate offensive rebounds
mean(reg.details$Lfga23.rat)
mean(reg.details$Wfga23.rat)
mean(reg.details$Lfgm3.pct)
mean(reg.details$Wfgm3.pct)

forced3pts.win<-dplyr::filter(reg.details,Wfga23.rat>cutoff.fga23a.rat & Wfgm3.pct > cutoff.frm3.prct)

forced3pts.loss<-dplyr::filter(reg.details,Lor.pct<cutoff.r.prct & Lfga23.rat<cutoff.fga23a.rat )
#forced3pts.win<-dplyr::filter(reg.details,Wor.pct<cutoff.r.prct & Wfga23.rat<cutoff.fga23a.rat )
plot(forced3pts.win$Wscore,col="green")
points(forced3pts.loss$Lscore,col="red")


forced3pts<-dplyr::filter(reg.details,Lfga23.rat<cutoff.fga23a.rat | Wfga23.rat<cutoff.fga23a.rat )
plot(forced3pts$Wfga23.rat,col="green")
points(forced3pts$Lfga23.rat,col="red")
#create differential statistics:
#grab all Winning team statistics:
Wteam<-reg.details[,grep("W.*",names(reg.details))]
Lteam<-reg.details[,grep("L.*",names(reg.details))]


melted <- reshape2::melt(reg.details, id.vars=c("Wteam","Lteam"),measure.vars=c("Wscore","Wfga23.rat",
                                                                                "Wfgm2.pct","Wftm.pct","Wfgm3.pct",
                                                                                "Lfgm2.pct","Lftm.pct","Lfgm3.pct"))
#summary stats:
summary.results<-plyr::ddply(melted, c("Wteam", "Lteam"),
      mean = mean(value), sd = sd(value),
      sem = sd(value)/sqrt(length(value)))


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

#keep non-duplicated stats that are percentages
reg.details.diff.prct<-reg.details.diff[,c(1,17,19:20,22,24)]

#cbind other relevant info:
reg.details.diff3<-cbind(reg.details$Wloc,reg.details.diff.prct)
names(reg.details.diff3)[names(reg.details.diff3) == "reg.details$Wloc"] <- "Wloc"



#create the training dataset (2003-2013)
reg.train<-reg.details.diff3[which(reg.details$Season<=2013 & reg.details$Wloc=="N"),]
reg.test<-reg.details.diff3[which(reg.details$Season>2013 & reg.details$Wloc=="N"),]
#reg.train.test<-reg.train[1:1000,c("diff.score","diff.fgm","diff.or","diff.dr","diff.blk","Wloc")]
reg.train$Wloc<-as.factor(reg.train$Wloc)
reg.test$Wloc<-as.factor(reg.test$Wloc)

#if only using "N" gameset:
reg.train<-reg.train[,-which(names(reg.train)=="Wloc")]
reg.test<-reg.test[,-which(names(reg.test)=="Wloc")]

#fit a glm with training dataset:
m1.prct <- lm(diff.score ~ ., data = reg.train)
summary(m1.prct)


#ADD IN 2-3 point ratio stat differernces to this model.  Also want to weight everything once Cody sends rankings.
#run testing dataset through (percentage stats:)
test.pred<-summary(m1.prct)$coeff[1]+
    (summary(m1.prct)$coeff[2]*reg.test$diff.fgm2.pct)+
    (summary(m1.prct)$coeff[3]*reg.test$diff.ftm.pct)+
    (summary(m1.prct)$coeff[4]*reg.test$diff.fgm3.pct)+
    (summary(m1.prct)$coeff[5]*reg.test$diff.or.pct)+
    (summary(m1.prct)$coeff[6]*reg.test$diff.dr.pct)
    
#fit a glm with training dataset:
m1 <- lm(diff.score ~ ., data = reg.train)
summary(m1) 

#lots of coeffs returned.  Keep only stats that are significant:
# intercept
# diff.fgm3
# diff.ftm
# diff.to
# diff.fgm2

#run testing dataset through non-prct stats
test.pred<-summary(m1.prct)$coeff[1]+
    (summary(m1.prct)$coeff[2]*reg.test$diff.fgm2.pct)+
    (summary(m1.prct)$coeff[3]*reg.test$diff.ftm.pct)+
    (summary(m1.prct)$coeff[4]*reg.test$diff.fgm3.pct)+
    (summary(m1.prct)$coeff[5]*reg.test$diff.or.pct)+
    (summary(m1.prct)$coeff[6]*reg.test$diff.dr.pct)


#apply a random forest:
modFit <- caret::train(diff.score~ .,data=reg.train.test,method="rf",prox=TRUE)
modFit
