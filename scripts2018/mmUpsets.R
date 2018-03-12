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

#using the tourney data from Kaggle:
path<-"C:/Users/jroberti/Git/mm2017/data2018"
setwd(path)
tourneyResults<-read.csv("NCAATourneyCompactResults.csv")
tourneySeeds<-read.csv("NCAATourneySeeds.csv")
#add fake seed for winning team:
tourneyResults$WSeed<-17
tourneyResults$LSeed<-17
#go thru and for each teamID grab the ID and find it in winning team ID
tourneyResults$WTeamID_Season<-paste0(tourneyResults$WTeamID,"-",tourneyResults$Season)
tourneyResults$LTeamID_Season<-paste0(tourneyResults$LTeamID,"-",tourneyResults$Season)
tourneySeeds$TeamID_Season<-paste0(tourneySeeds$TeamID,"-",tourneySeeds$Season)
WTeamMatch<-list()
for(i in 1:nrow(tourneyResults)){
    #grab indices where teamID and season match seed file: (WINNING TEAM)
    matchingIndWin<-grep(tourneySeeds$TeamID_Season[i],tourneyResults$WTeamID_Season)
    #replace fake seed with team's actual seed:
    tourneyResults$WSeed[matchingIndWin]<-gsub("\\D","",tourneySeeds$Seed[i])
    #grab indices where teamID and season match seed file: (LOSING TEAM)
    matchingIndLose<-grep(tourneySeeds$TeamID_Season[i],tourneyResults$LTeamID_Season)
    #replace fake seed with team's actual seed:
    tourneyResults$LSeed[matchingIndLose]<-gsub("\\D","",tourneySeeds$Seed[i])
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
#make upset column:
tourneyResults$Upset<-NA
#will preserve same seed matchups
tourneyResults$Upset<-ifelse(tourneyResults$WSeed>tourneyResults$LSeed,1,0)
#find how many first round upsets there are:
upsetSums<-tourneyResults %>% group_by(WSeed,Round) %>% summarise(Frequency = sum(Upset))
#get upsets per year (average)
upsetSums$perYrAvg<-upsetSums$Frequency/length(unique(tourneyResults$Season))
#just grab first round upsets:
upsetsR1<-upsetSums[upsetSums$Round==1,]
upsetsR1.clean<-upsetsR1[complete.cases(upsetsR1),]
#upsetsPerYear on average
