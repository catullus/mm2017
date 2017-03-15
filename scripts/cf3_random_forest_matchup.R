

library(plyr)
library(dplyr)
library(stringr)
library(PlayerRatings)
library(car)
#library(randomForest)
library(tidyr)
# library(gbm)

# https://www.kaggle.com/ajniggles/march-machine-learning-mania-2017/logistic-regression-and-game-round-calculator

#sourcepath <- "C:/Users/Amy/Documents/GitHub/mm2017/scripts/"

inpath <- "C:/Users/Amy/Documents/GitHub/mm2017/data/"
reg_det<-read.csv(paste0(inpath, "RegularSeasonDetailedResults.csv"), stringsAsFactors = FALSE, header = TRUE)
#tourney.details<-read.csv(paste0(inpath, "TourneyDetailedResults.csv"), stringsAsFactors = FALSE, header = TRUE)

# 32 seconds to do all processing
#source(file = paste0(sourcepath,"proc_data.R"))



## Munge: End of Season Stats


# stack these
reg_det_win <- reg_det
## rename
## winning teams
names(reg_det_win) <- str_replace(names(reg_det_win), pattern = "^W", "A")
names(reg_det_win) <- str_replace(names(reg_det_win), pattern = "^L", "B")
reg_det_win <-  select(reg_det_win, -Bteam, -Bscore, -Aloc, Numot) %>% rename(team=Ateam, score=Ascore)
reg_det_win$win <- "win"
reg_det_win$win_num <- 1

## losing teams
reg_det_lose <- reg_det
names(reg_det_lose) <- str_replace(names(reg_det_lose), pattern = "^L", "A")
names(reg_det_lose) <- str_replace(names(reg_det_lose), pattern = "^W", "B")
## the second team, i.e. the losing team, inadvertently gets changed because of the str_replace above
reg_det_lose <- select(reg_det_lose, -Bteam, -Bscore, -Bloc, Numot) %>% rename(team=Ateam, score=Ascore) 
reg_det_lose$win <- "loss"
reg_det_lose$win_num <- 0

## combine
h2h_reg_det <- rbind(reg_det_win, reg_det_lose)

mod_h2h_reg_det <- filter(h2h_reg_det, Season %in% c(2015:2016)) %>% select(-win, -Season, -Daynum, -team, -score, -Numot) %>% sample


## build a simple model of team a vs. team b matchups by detailed stats
m_rf2 <- randomForest::randomForest(as.factor(win) ~ Adr + Bdr + Afgm + Bfgm + Ato + Bto, data = filter(h2h_reg_det, Season %in% c(2014:2016)))

## now sample the data and pit some teams against each other
tourney_test <- filter(tourney, Season %in% c(2014:2016))


############################# prepare head to head inputs
# stack these
reg_det_win <- select(reg_det, -Lfgm, -Lfga, -Lfgm3, -Lfga3, -Lftm, -Lfta, -Lor, -Ldr, -Last, -Lto, -Lstl, -Lblk, -Lpf, -Lteam, -Lscore, -Wloc)
## rename
names(reg_det_win) <- str_replace(names(reg_det_win), pattern = "^W", "")

reg_det_lose <- select(reg_det, -Wfgm, -Wfga, -Wfgm3, -Wfga3, -Wftm, -Wfta, -Wor, -Wdr, -Wast, -Wto, -Wstl, -Wblk, -Wpf, -Wteam, -Wscore, -Wloc)

names(reg_det_lose) <- str_replace(names(reg_det_lose), pattern = "^L", "")

out_reg_det <- rbind(reg_det_win, reg_det_lose)



head(h2h_reg_det)

proc_reg_det <- group_by(out_reg_det, Season, team) %>%
  arrange(Daynum) %>% 
  dplyr::summarise(fgm=mean(fgm),
                   fga=mean(fga),
                   fg_pct=(fgm/fga),
                   fgm3=mean(fgm3),
                   fga3=mean(fga3),
                   fg3_pct=(fgm3/fga3),
                   ftm=mean(ftm),
                   fta=mean(fta),
                   ft_pct=(ftm/fta),
                   or=mean(or),
                   dr=mean(dr),
                   ast=mean(ast),
                   to=mean(to),
                   stl=mean(stl),
                   blk=mean(blk),
                   pf=mean(pf))

####### COMBINE DATA TO RUN THROUGH RANDOM FOREST MODEL

proc_reg_det_test <- proc_reg_det %>% filter(Season %in% (2014:2016)) %>% select(dr,fgm,to, Season, team)
proc_reg_det_test$team_key <- paste0(proc_reg_det_test$Season,"_", proc_reg_det_test$team)

tourney_test$Wteam_key <- paste0(tourney_test$Season,"_",tourney_test$Wteam)
tourney_test$Lteam_key <- paste0(tourney_test$Season,"_",tourney_test$Lteam)

tourney_test_model <- merge(tourney_test, proc_reg_det_test, by.x="Wteam_key", by.y="team_key")
tourney_test_model <- select(tourney_test_model, -Season.y, team) %>% rename(Adr=dr, Afgm=fgm, Ato=to)
tourney_test_model <- merge(tourney_test_model, proc_reg_det_test, by.x="Lteam_key", by.y="team_key")
tourney_test_model <- select(tourney_test_model, -team.x, -Season, -team.y) %>% rename(Bdr=dr, Bfgm=fgm, Bto=to)

###### FEED INTO RANDOM FOREST MODEL

tourney_test_model_results <- predict(object = m_rf2, newdata = select(tourney_test_model, Adr, Bdr, Afgm, Bfgm, Ato, Bto), type="prob")

# # each row in tourney_test_model is a win for the Ateam by default
pred_outcome <- cbind(tourney_test_model, tourney_test_model_results)
## did the model correctly predict this based on the stats?
pred_outcome$correct <- 1-pred_outcome$win
## how correct was this simple model? 41% -- that's fine, this is only 3 variables!
table(pred_outcome$correct>0.5)/sum(table(pred_outcome$correct>0.5))
