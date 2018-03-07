#MM2018 Player Stats    
#this script grabs all player data for a team via rotowire:

#create a sequence of playerIDs for the rotowire site:
getStatsNCAA<-function(playerID=NULL){
    URL<-paste0("https://www.rotowire.com/cbasketball/player.htm?id=",playerID)
    library(rvest)
    #read html
    dailyLines<-read_html(URL)
    #browser()
    dailyLines.list<- dailyLines %>% html_nodes("table") %>%  html_table(fill = TRUE)
    #if a list is actually returned
    if(length(dailyLines.list)>0){
        ############## CLEAN UP RESPECTIVE DF ########################
        if(nrow(dailyLines.list[[2]])>0){
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
            #find the player's name and team:
            findInfo<- dailyLines %>% html_nodes("div") %>% html_nodes("div")  %>% html_nodes("div")
            teamInd<-grep("team\\=",findInfo)[1]
            if(!is.na(teamInd)){
                playerTeam1<-gsub(".*splitshead\">|</p>.*|Depth Chart", "", findInfo[teamInd])
                playerTeam2<-paste0(stringr::word(playerTeam1,
                                 1:(stringr::str_count(playerTeam1, ' ')-1)),collapse=" ")
            }
            else{
                playerTeam2<-"NO TEAM"
            }
            playerInd<-grep("College Basketball Stats",findInfo)[1]
            playerName<-trimws(gsub(".*10px;\">|College Basketball Stats.*", "", findInfo[playerInd]),"both")
            #create dynamic filename:
            filename<-paste0("C:/Users/jroberti/Git/mm2017/data/playerStats/",playerTeam2,"_",playerName,".rds")
            #save the file:
            saveRDS(dailyLines.list[[2]],filename)
        }
        #browser()
    }
    print(playerID)
}

#run the script:
playerSeq<-seq(11001,12000)
lapply(playerSeq, function(x) getStatsNCAA(playerID = x))
