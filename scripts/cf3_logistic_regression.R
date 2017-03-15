

library(plyr)
library(dplyr)
library(stringr)
library(PlayerRatings)
library(car)
#library(randomForest)
library(tidyr)
# library(gbm)

# https://www.kaggle.com/ajniggles/march-machine-learning-mania-2017/logistic-regression-and-game-round-calculator

sourcepath <- "C:/Users/Amy/Documents/GitHub/mm2017/scripts/"

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
out_reg_det <- rbind(reg_det_win, reg_det_lose)

mod_out_reg_det <- filter(out_reg_det, Season %in% c(2015:2016)) %>% select(-win, -Season, -Daynum, -team, -score, -Numot) %>% sample

m_lr <- glm(win_num ~ Adr + Bdr + Afgm + Bfgm +Apf + Bpf + Aast + Bast + Ato + Bto, data = mod_out_reg_det, family = binomial(link = "logit"))
summary(m_lr)
predict(m_lr)

m_rf2 <- randomForest::randomForest(as.factor(win) ~ Adr + Bdr + Afgm + Bfgm +Apf + Bpf + Aast + Bast + Ato + Bto, data = filter(out_reg_det, Season %in% c(2014:2016)))
