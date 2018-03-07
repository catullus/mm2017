library(plyr)
library(dplyr)
library(stringr)
library(PlayerRatings)
library(car)
library(randomForest)
library(tidyr)
library(ggplot2)
# library(gbm)

inpath <- "C:/Users/Amy/Documents/GitHub/mm2017/data/"
reg_det<-read.csv(paste0(inpath, "RegularSeasonDetailedResults.csv"), stringsAsFactors = FALSE, header = TRUE)
#tourney.details<-read.csv(paste0(inpath, "TourneyDetailedResults.csv"), stringsAsFactors = FALSE, header = TRUE)
tourney <- read.csv(paste0(inpath, "TourneyCompactResults.csv"), stringsAsFactors = FALSE)

# 32 seconds to do all processing
#source(file = paste0(sourcepath,"proc_data.R"))

## modify_reg_det with new statistics here
head(reg_det)
## BASIC ratio stats field goals, 3's, free throws
reg_det$Wfg_pct <- reg_det$Wfgm/reg_det$Wfga
reg_det$Lfg_pct <- reg_det$Lfgm/reg_det$Wfga
reg_det$Wft_pct<- (reg_det$Wftm/reg_det$Wfta)
reg_det$Wfg3_pct<- (reg_det$Wfgm3/reg_det$Wfga3)
reg_det$Lft_pct<- (reg_det$Lftm/reg_det$Lfta)
reg_det$Lfg3_pct<- (reg_det$Lfgm3/reg_det$Lfga3)

## forced turnovers -- defensive rating
reg_det$Wforced_to <- reg_det$Lto
reg_det$Lforced_to <- reg_det$Wto
reg_det$Wpts_allow <- reg_det$Lscore
reg_det$Lpts_allow <- reg_det$Wscore

## response vars
reg_det$Wdiff <- reg_det$Wscore - reg_det$Lscore
reg_det$Ldiff <- reg_det$Lscore - reg_det$Wscore

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



### 
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
  # response vars
  reg_det_win$win <- "win"
  reg_det_win$win_num <- 1
  reg_det_win$diff <- reg_det_win$Adiff
  
  ## losing teams
  reg_det_lose <- data
  names(reg_det_lose) <- str_replace(names(reg_det_lose), pattern = "^L", "A")
  names(reg_det_lose) <- str_replace(names(reg_det_lose), pattern = "^W", "B")
  ## the second team, i.e. the losing team, inadvertently gets changed because of the str_replace above
  reg_det_lose <- select(reg_det_lose, -Bteam, -Ascore,-Bscore, -Bloc, -Numot) %>% rename(team=Ateam)# %>% select(vars)
  ## response vars
  reg_det_lose$win <- "loss"
  reg_det_lose$win_num <- 0
  reg_det_lose$diff <- reg_det_lose$Adiff
  ## combine
  h2h_reg_det <- rbind(reg_det_win, reg_det_lose)
  return(h2h_reg_det)
}


h2h_reg_det <- prep_reg_det_model(reg_det, mymod);head(h2h_reg_det)

## 1.34% variance explained...
#m1 <- randomForest(Wpts_allow ~ Wforced_to + Wblk + Wstl + Wdr, data = reg_det);m1

#scatterplotMatrix(~Wpts_allow +Lpts_allow+ Wforced_to+ Wblk + Wstl + Wdr + Lforced_to+Lblk+Lstl+Ldr, data = filter(reg_det, Season %in% 2015:2016))


scatterplotMatrix(~Lpts_allow +Lforced_to+ Lblk + Lstl + Ldr, data = filter(reg_det, Season %in% 2016))



