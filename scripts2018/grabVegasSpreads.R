#grab Vegas spreads:
date<-"03-17-2017"
URL<-paste0("http://www.vegasinsider.com/college-basketball/scoreboard/scores.cfm/game_date/",date)
library(rvest)
#read html
dailyLines<-read_html(URL)
#browser()
dailyLines.list<- dailyLines %>% html_nodes("table") %>% html_nodes("tr") %>% html_nodes("table") %>% html_table(fill = TRUE)
#grab the scores and spreads (want DFs that are 4x6)
keep1<-which(lapply(dailyLines.list, function(x) dim(x)[1])==4)
keep2<-which(lapply(dailyLines.list, function(x) dim(x)[2])==6)
#find intersection:
keepThese<-intersect(keep1,keep2)
#grab the scores and odds:
raw.odds.list<-dailyLines.list[keepThese]
#clean up!
for(i in 1:length(raw.odds.list)){
    #make the second row of each nested DF the column names:
    names(raw.odds.list[[i]])<-raw.odds.list[[i]][2,]
    #remove the 1st and 2nd rows of DF (don't need)
    raw.odds.list[[i]]<-raw.odds.list[[i]][3:4,]
    #clean up any weird characters in the names:
    raw.odds.list[[i]]$Teams<-gsub("\\d","",raw.odds.list[[i]]$Teams)
    #grab first word to get rid of random text:
    for(j in 1:length(raw.odds.list[[i]]$Teams)){
        raw.odds.list[[i]]$Teams[j]<-strsplit(raw.odds.list[[i]]$Teams[j], " ")[[1]][2]
    }
}
#clean list:
clean.odds.list<-raw.odds.list
