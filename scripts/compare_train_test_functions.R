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

## modify_reg_det with new statistics here
head(reg_det)
## BASIC ratio stats field goals, 3's, free throws
reg_det$Wfg_pct <- reg_det$Wfgm/reg_det$Wfga
reg_det$Lfg_pct <- reg_det$Lfgm/reg_det$Wfga
reg_det$Wft_pct<- (reg_det$Wftm/reg_det$Wfta)
reg_det$Wfg3_pct<- (reg_det$Wfgm3/reg_det$Wfga3)
reg_det$Lft_pct<- (reg_det$Lftm/reg_det$Lfta)
reg_det$Lfg3_pct<- (reg_det$Lfgm3/reg_det$Lfga3)
#################################################################################################
## Efficiency
#multiply the turnovers by the shooting % of L team to get a more accurate picture of how detrimental the turnovers are.  Also, on the other end, multiple the defensive rbs by the shooting percentage of W team to see how advantageous the defensive rbs are...
reg_det$Wfga2<-reg_det$Wfga-reg_det$Wfga3
#2-pointers made:
reg_det$Wfgm2<-reg_det$Wfgm-reg_det$Wfgm3
reg_det$Lfga2<-reg_det$Lfga-reg_det$Lfga3
#2-pointers made:
reg_det$Lfgm2<-reg_det$Lfgm-reg_det$Lfgm3

reg_det$Wposs<-reg_det$Wfga2+reg_det$Wfga3+reg_det$Wfta+reg_det$Wto   
reg_det$Wshoot.prct<-reg_det$Wfgm2+reg_det$Wfgm3+reg_det$Wftm/reg_det$Wfga2+reg_det$Wfga3+reg_det$Wfta
reg_det$Lshoot.prct<-reg_det$Lfgm2+reg_det$Lfgm3+reg_det$Lftm/reg_det$Lfga2+reg_det$Lfga3+reg_det$Lfta
reg_det$Wposs.action<-(reg_det$Wfgm2+reg_det$Wfgm3+reg_det$Wftm-(reg_det$Wto*(reg_det$Lshoot.prct/100))+(reg_det$Wdr*(reg_det$Wshoot.prct/100)))
reg_det$Wposs.eff<-reg_det$Wposs.action/reg_det$Wposs
reg_det$Lposs<-reg_det$Lfga2+reg_det$Lfga3+reg_det$Lfta+reg_det$Ldr+reg_det$Lto
reg_det$Lposs.action<-(reg_det$Lfgm2+reg_det$Lfgm3+reg_det$Lftm-(reg_det$Lto*(reg_det$Wshoot.prct/100))+(reg_det$Ldr*(reg_det$Lshoot.prct/100)))
reg_det$Lposs.eff<-reg_det$Lposs.action/reg_det$Lposs



#################################################################################################
## Munge: End of Season Stats
# stack these -- model needs win as 1 and 0 or win/loss
#################################################################################################
model_vars <- c("Wfgm", "Lfgm", "Wor", "Lor")
## this sets up the data for a binary model
prep_reg_det_model <- function(data, model_vars){
  #vars <- match(model_vars, names(data))
  reg_det_win <- data
  ## rename
  ## winning teams
  names(reg_det_win) <- str_replace(names(reg_det_win), pattern = "^W", "A")
  names(reg_det_win) <- str_replace(names(reg_det_win), pattern = "^L", "B")
  # vars_a <- 
  reg_det_win <-  select(reg_det_win, -Bteam, -Ascore, -Bscore, -Aloc, -Numot) %>% rename(team=Ateam)# %>% select(vars)
  reg_det_win$win <- "win"
  reg_det_win$win_num <- 1
  
  ## losing teams
  reg_det_lose <- data
  names(reg_det_lose) <- str_replace(names(reg_det_lose), pattern = "^L", "A")
  names(reg_det_lose) <- str_replace(names(reg_det_lose), pattern = "^W", "B")
  ## the second team, i.e. the losing team, inadvertently gets changed because of the str_replace above
  reg_det_lose <- select(reg_det_lose, -Bteam, -Ascore,-Bscore, -Bloc, -Numot) %>% rename(team=Ateam)# %>% select(vars)
  reg_det_lose$win <- "loss"
  reg_det_lose$win_num <- 0
  ## combine
  h2h_reg_det <- rbind(reg_det_win, reg_det_lose)
  return(h2h_reg_det)
}

#prep_reg_det_model(reg_det, c("Wfg_pct", "Lfg_pct"))

h2h_reg_det <- prep_reg_det_model(reg_det, mymod);head(h2h_reg_det)

## run model e.g. 
mrf1 <- randomForest(as.factor(win)~Aposs.eff+Bposs.eff+Afg_pct+Bfg_pct+Afg3_pct+Bfg_pct, data=filter(h2h_reg_det, Season %in% c(2015:2016)))

#################################################################################################
## feed this processed season results (i.e. lines of team statistics up to line 46 above)
prep_season_end_stats <- function(data){
  reg_det_win <- select(data, -Lfg_pct, -Lft_pct,-Lfg3_pct,-Lposs, -Lposs.action, -Lposs.eff, -Lshoot.prct, -Lfgm, -Lfga, -Lfgm3, -Lfga3, -Lftm, -Lfta, -Lor, -Ldr, -Last, -Lto, -Lstl, -Lblk, -Lpf, -Lteam, -Lscore, -Wloc, -Lfga2, -Lfgm2)
  names(reg_det_win) <- str_replace(names(reg_det_win), pattern = "^W", "")
  
  reg_det_lose <- select(data, -Wfg_pct, -Wft_pct, -Wfg3_pct,  -Wposs, -Wposs.action,-Wposs.eff,-Wshoot.prct,-Wfgm, -Wfga, -Wfgm3, -Wfga3, -Wftm, -Wfta, -Wor, -Wdr, -Wast, -Wto, -Wstl, -Wblk, -Wpf, -Wteam, -Wscore, -Wloc, -Wfga2, -Wfgm2)
  names(reg_det_lose) <- str_replace(names(reg_det_lose), pattern = "^L", "")
  
  # stack again
  out_reg_det <- rbind(reg_det_win, reg_det_lose)
  
  ###### Summarize end of season results
  proc_reg_det <- group_by(out_reg_det, Season, team) %>%
    arrange(Daynum) %>% 
    dplyr::summarise(scores=mean(score),
                     score_sd=sd(score, na.rm=TRUE),
                     poss.ef=mean(poss.eff),
                     fgm=mean(fgm),
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
}

seasonEnd <- prep_season_end_stats(reg_det) 

#################################################################################################
## this is to feed into the model object for predictions
## -- need to match end of season stats to tourney match ups 
## feed this the summary data from end of season and the tournament data (i.e. matchups)
join_seasonEnd_tourney <- function(end_season_data, tourney_matchups){
  
  end_season_data$team_key <- paste0(end_season_data$Season,"_", end_season_data$team)
  
  tourney_matchups$Wteam_key <- paste0(tourney_matchups$Season,"_",tourney_matchups$Wteam)
  tourney_matchups$Lteam_key <- paste0(tourney_matchups$Season,"_",tourney_matchups$Lteam)
  
  tourney_matchups_model <- merge(tourney_matchups, end_season_data, by.x="Wteam_key", by.y="team_key")
  
  #org_name <- names(tourney_matchups_model[,13:ncol(tourney_matchups_model)])
  
  tourney_matchups_model <- select(tourney_matchups_model, -Season.y, -team) %>% 
    rename(Ascore=scores, Afgm=fgm,Afga=fga, Afgm3=fgm3, Afga3=fga3, Aftm=ftm, Afta=fta, Aor=or, Adr=dr, 
           Aast=ast, Ato=to, Astl=stl, Ablk=blk, Apf=pf, Afg_pct=fg_pct, Aft_pct=ft_pct, 
           Afg3_pct=fg3_pct, Aposs.eff=poss.ef, Ascore_sd=score_sd)
  

  #names(tourney_matchups)
  
  tourney_matchups_model <- merge(tourney_matchups_model, end_season_data, by.x="Lteam_key", by.y="team_key")
  
  tourney_matchups_model <- select(tourney_matchups_model, -Season, -team) %>% 
    rename(Bscore=scores, Bfgm=fgm,Bfga=fga, Bfgm3=fgm3, Bfga3=fga3, Bftm=ftm, 
           Bfta=fta, Bor=or, Bdr=dr, Bast=ast, Bto=to, Bstl=stl, Bblk=blk, Bpf=pf, 
           Bfg_pct=fg_pct, Bft_pct=ft_pct, Bfg3_pct=fg3_pct, Bposs.eff=poss.ef,Bscore_sd=score_sd)
}

tournament_match <- join_seasonEnd_tourney(seasonEnd, tourney)

#################################################################################################
## Assess Model params = (model_object, test_data)
###### FEED INTO RANDOM FOREST MODEL
pred_outcome1 <- predict(mrf1, newdata=tournament_match, type="prob")
pred_outcome1 <- cbind(tournament_match, pred_outcome1)
table(pred_outcome1$win>=0.505)/sum(table(pred_outcome1$win))
