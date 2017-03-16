## analyze results of mrf2
inpath <- "C:/Users/cflagg/Documents/GitHub/mm2017/data/"

res <- read.csv(paste0(inpath, "cf_preds_mrf2.csv"), stringsAsFactors = FALSE)
teams <- read.csv(paste0(inpath, "Teams.csv"), stringsAsFactors = FALSE)

#reg_det<-read.csv(paste0(inpath, "RegularSeasonDetailedResults.csv"), stringsAsFactors = FALSE, header = TRUE)
#tourney.details<-read.csv(paste0(inpath, "TourneyDetailedResults.csv"), stringsAsFactors = FALSE, header = TRUE)
#tourney <- read.csv(paste0(inpath, "TourneyCompactResults.csv"), stringsAsFactors = FALSE)
seeds <- read.csv(paste0(inpath, "TourneySeeds.csv"), stringsAsFactors = FALSE)

seeds2017 <- filter(seeds, Season==2017)
seeds2017$key <- paste0(seeds2017$Season,"_",seeds2017$Team)

head(res)
head(teams)

resm <- merge(res, teams, by.x="Wteam", by.y="Team_Id")
resm <- merge(resm, teams, by.x="Lteam", "Team_Id")

resm <- dplyr::rename(resm, win_team=Team_Name.x, lose_team=Team_Name.y)

write.csv(resm, paste0(inpath, "results_mrf2_teamNames.csv"))
