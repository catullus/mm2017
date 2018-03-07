#MM2018 Player Stats    
#this script grabs all player data for a team via rotowire:

URL<-"https://www.rotowire.com/cbasketball/player.htm?id=11472"
library(rvest)
#read html
dailyLines<-read_html(URL)
dailyLines.list<- dailyLines %>% html_nodes("table") %>%  html_table(fill = TRUE)
############## CLEAN UP RESPECTIVE DF ########################
#remove table partition line in html:
dailyLines.list[[2]]<-dailyLines.list[[2]][-which(nchar(dailyLines.list[[2]]$Date)>10),]
#remove empty columns:
lastCol<-grep("FOULS",names(dailyLines.list[[2]]))
dailyLines.list[[2]]<-dailyLines.list[[2]][,1:lastCol]
#replace all "did not play" stats to 0 (applicable columns only)
dailyLines.list[[2]][,4:length(dailyLines.list[[2]])]<-apply(dailyLines.list[[2]][,4:length(dailyLines.list[[2]])], 2,
                                                             function(x) as.numeric(gsub("Did Not Play", 0, x)))
#replace "*" in Started Game (ST) with a binary (0=did not start, 1 = did start)
dailyLines.list[[2]]$ST<-ifelse(dailyLines.list[[2]]$ST=="*",1,0)
#clean up opponent (remove @ symbol and add binary home /away column):
dailyLines.list[[2]]$HOME<-rep(1,nrow(dailyLines.list[[2]]))
dailyLines.list[[2]]$HOME[grep("@",dailyLines.list[[2]]$Opp)]<-0
dailyLines.list[[2]]$Opp<-gsub("@","",dailyLines.list[[2]]$Opp)
#fix dates:
dailyLines.list[[2]]$POSIX<-as.character(as.Date(dailyLines.list[[2]]$Date, format= "%b %d"))
dailyLines.list[[2]]$POSIX<-as.Date(ifelse(as.numeric(substr(dailyLines.list[[2]]$POSIX,6,7))>=10,
                                   gsub("2018","2017",dailyLines.list[[2]]$POSIX),
                                   dailyLines.list[[2]]$POSIX))
###############################################################
#write out a file with team name and player name:
playerTeam<-grep("team=",dailyLines[2])  #need to fix this part!
