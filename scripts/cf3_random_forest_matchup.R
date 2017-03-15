

library(plyr)
library(dplyr)
library(stringr)
library(PlayerRatings)
library(car)
library(randomForest)
library(tidyr)
# library(gbm)

# https://www.kaggle.com/ajniggles/march-machine-learning-mania-2017/logistic-regression-and-game-round-calculator

#sourcepath <- "C:/Users/Amy/Documents/GitHub/mm2017/scripts/"

#inpath <- "C:/Users/Amy/Documents/GitHub/mm2017/data/"
inpath <- "C:/Users/cflagg/Documents/GitHub/mm2017/data/"
reg_det<-read.csv(paste0(inpath, "RegularSeasonDetailedResults.csv"), stringsAsFactors = FALSE, header = TRUE)
#tourney.details<-read.csv(paste0(inpath, "TourneyDetailedResults.csv"), stringsAsFactors = FALSE, header = TRUE)
tourney <- read.csv(paste0(inpath, "TourneyCompactResults.csv"), stringsAsFactors = FALSE)


# 32 seconds to do all processing
#source(file = paste0(sourcepath,"proc_data.R"))

## Munge: Regular Season Stats
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

mod_h2h_reg_det <- filter(h2h_reg_det, Season %in% c(2015:2016)) %>% select(-win, -Season, -Daynum, -team, -score, -Numot) 

## calculate ratios of field goals, 3 pointers, and free throws
mod_h2h_reg_det$Afg_pct <- mod_h2h_reg_det$Afgm / mod_h2h_reg_det$Afga
mod_h2h_reg_det$Afg3_pct <- mod_h2h_reg_det$Afg3m / mod_h2h_reg_det$Afg3a
mod_h2h_reg_det$Aft_pct <- mod_h2h_reg_det$Aftm / mod_h2h_reg_det$Afta
mod_h2h_reg_det$Bfg_pct <- mod_h2h_reg_det$Bfgm / mod_h2h_reg_det$Bfga 
mod_h2h_reg_det$Bfg3_pct <- mod_h2h_reg_det$Bfg3m / mod_h2h_reg_det$Bfg3a
mod_h2h_reg_det$Bft_pct <- mod_h2h_reg_det$Bftm / mod_h2h_reg_det$Bfta

############## build a simple model of team a vs. team b matchups by detailed stats
start <- Sys.time()
m_rf2 <- randomForest::randomForest(as.factor(win) ~ Adr + Bdr + Afgm + Bfgm + Ato + Bto, data = filter(h2h_reg_det, Season %in% c(2014:2016)))
end <- Sys.time(); end - start

############## Bigger Model 

m_rf3_dat <- filter(h2h_reg_det, Season %in% c(2002:2016)) %>% select(-Numot, -win_num, -Daynum, -team, -Season, -score)

## calculate ratios of field goals, 3 pointers, and free throws
m_rf3_dat$Afg_pct <- m_rf3_dat$Afgm / m_rf3_dat$Afga
m_rf3_dat$Afg3_pct <- m_rf3_dat$Afg3m / m_rf3_dat$Afg3a
m_rf3_dat$Aft_pct <- m_rf3_dat$Aftm / m_rf3_dat$Afta
m_rf3_dat$Bfg_pct <- m_rf3_dat$Bfgm / m_rf3_dat$Bfga 
m_rf3_dat$Bfg3_pct <- m_rf3_dat$Bfg3m / m_rf3_dat$Bfg3a
m_rf3_dat$Bft_pct <- m_rf3_dat$Bftm / m_rf3_dat$Bfta

start <- Sys.time()
m_rf3 <- randomForest::randomForest(as.factor(win) ~ ., data = m_rf3_dat)
end <- Sys.time(); end - start

## now sample the data and pit some teams against each other
tourney_test <- filter(tourney, Season %in% c(2002:2016))


############################# prepare head to head inputs
# stack these
#reg_det_win <- select(reg_det, -Lfgm, -Lfga, -Lfgm3, -Lfga3, -Lftm, -Lfta, -Lor, -Ldr, -Last, -Lto, -Lstl, -Lblk, -Lpf, -Lteam, -Lscore, -Wloc)
# stack these
reg_det_win <- select(reg_det, -Lfgm, -Lfga, -Lfgm3, -Lfga3, -Lftm, -Lfta, -Lor, -Ldr, -Last, -Lto, -Lstl, -Lblk, -Lpf, -Lteam, -Lscore, -Wloc)
names(reg_det_win) <- str_replace(names(reg_det_win), pattern = "^W", "")
reg_det_lose <- select(reg_det, -Wfgm, -Wfga, -Wfgm3, -Wfga3, -Wftm, -Wfta, -Wor, -Wdr, -Wast, -Wto, -Wstl, -Wblk, -Wpf, -Wteam, -Wscore, -Wloc)
names(reg_det_lose) <- str_replace(names(reg_det_lose), pattern = "^L", "")

out_reg_det <- rbind(reg_det_win, reg_det_lose)

###### Summarize end of season results
proc_reg_det <- group_by(out_reg_det, Season, team) %>%
  arrange(Daynum) %>% 
  dplyr::summarise(score=mean(score),
                  fgm=mean(fgm),
                   fga=mean(fga),
                   fg_pct=(fgm/fga),
                   ft_pct=(ftm/fta),
                   fg3_pct=(fgm3/fga3),
                   fgm3=mean(fgm3),
                   fga3=mean(fga3),
                   ftm=mean(ftm),
                   fta=mean(fta),
                   or=mean(or),
                   dr=mean(dr),
                   ast=mean(ast),
                   to=mean(to),
                   stl=mean(stl),
                   blk=mean(blk),
                   pf=mean(pf))

####### COMBINE DATA TO RUN THROUGH RANDOM FOREST MODEL
####### TURN THIS INTO A FUNCTION

#find_pred_inputs <- function(proc_reg_det, tourney_data, pred_names, Season){
proc_reg_det_test <- proc_reg_det %>% filter(Season %in% (2002:2016))
proc_reg_det_test$team_key <- paste0(proc_reg_det_test$Season,"_", proc_reg_det_test$team)

tourney_test$Wteam_key <- paste0(tourney_test$Season,"_",tourney_test$Wteam)
tourney_test$Lteam_key <- paste0(tourney_test$Season,"_",tourney_test$Lteam)

tourney_test_model <- merge(tourney_test, proc_reg_det_test, by.x="Wteam_key", by.y="team_key")

#org_name <- names(tourney_test_model[,13:ncol(tourney_test_model)])

tourney_test_model <- select(tourney_test_model, -Season.y, -team, -fg3_pct, -ft_pct, -fg_pct) %>% 
  rename(Ascore=score, Afgm=fgm,Afga=fga, Afgm3=fgm3, Afga3=fga3, Aftm=ftm, Afta=fta, Aor=or, Adr=dr, Aast=ast, Ato=to, Astl=stl, Ablk=blk, Apf=pf)

names(tourney_test)

tourney_test_model <- merge(tourney_test_model, proc_reg_det_test, by.x="Lteam_key", by.y="team_key")

tourney_test_model <- select(tourney_test_model, -Season, -team, -fg3_pct, -ft_pct, -fg_pct) %>% 
  rename(Bscore=score, Bfgm=fgm,Bfga=fga, Bfgm3=fgm3, Bfga3=fga3, Bftm=ftm, Bfta=fta, Bor=or, Bdr=dr, Bast=ast, Bto=to, Bstl=stl, Bblk=blk, Bpf=pf)
#}

### FIX THE SCORE in the RF model
tourney_test_model <- tourney_test_model %>% rename(score=Wscore)

###### FEED INTO RANDOM FOREST MODEL
tourney_test_model_results <- predict(object = m_rf2, newdata = tourney_test_model, type="prob")

## each row in tourney_test_model is a win for the Ateam by default
pred_outcome <- cbind(tourney_test_model, tourney_test_model_results)
## did the model correctly predict this based on the stats?

## count how many games the model predicted 'correctly' i.e. win >= 0.50
table(pred_outcome$win>=0.505)/sum(table(pred_outcome$win))

simple_rf2 <- select(pred_outcome, Wscore, Lscore, loss, win)
all_rf3 <- select(pred_outcome, Wscore, Lscore, loss, win)
