## proc_data
library(dplyr)
library(ggplot2)
library(randomForest)

# read in data
df_in <- read.csv('RegularSeasonDetailedResults.csv', stringsAsFactors = FALSE)

## subset to one stat of interest
test <- select(df_in, Season, DayNum, WTeamID, WScore, WFGA, WFTA, WOR, WTO, LTeamID, LScore, LFGA, LFTA, LOR, LTO)

# calculate possession
# TeamX_possession = 0.96*[(Field Goal Attempts)+(Turnovers)+0.44*(Free Throw Attempts)-(Offensive Rebounds)]
test2 <- test %>%
    mutate(Wposs = 0.96 * (WFGA + WTO + (0.44 * WFTA) - WOR), 
           Lposs = 0.96 * (LFGA + LTO + (0.44 * LFTA) - LOR),
           WOE = WScore/Wposs,
           LOE = LScore/Lposs) %>% 
    dplyr::select(Season, DayNum, WTeamID, WOE, LTeamID, LOE, WOR, LOR, WFTA, LFTA, WFTA, WFGA, LFGA)

## create unique gameID
test2$gameID <- paste0(test2$Season, "_", test2$DayNum, "_", test2$WTeamID, "_", test2$LTeamID)

# test with this game
target_game <- "2012_23_1160_1102"
target <- "2012"

##### STACK DATA #####
# rbind() winning and losing data
wdata <- select(test2, gameID, Season, DayNum, starts_with("W"))
wdata$result <- 1
ldata <- select(test2, gameID, Season, DayNum, starts_with("L"))
ldata$result <- 0

# test subset
wdata2 <- filter(wdata, Season == target)
ldata2 <- filter(ldata, Season == target)

# remove W and L from column names to prep for merging
names(wdata2)<-gsub("^W","", names(wdata2)) 
names(ldata2)<-gsub("^L","", names(ldata2)) 

# rbind data
df_int <- rbind(wdata2, ldata2)

# summary
# xtabs(~result + OE_diff, data = df_model)

#### CALCULATE PREVIOUS PERFORMANCE #####
## calculate previous performance metric before feeding to model -- does previous efficiency predict next game?
# https://towardsdatascience.com/predicting-upsets-in-the-ncaa-tournament-with-machine-learning-816fecf41f01

## order games sequentially for each team arrange(Season + DayNum)
df_int <- arrange(df_int, TeamID, Season, DayNum)

## calculate previous performance of one week
df_int_pp <- pp_wrap(data = df_int)

## sanity check
filter(df_int_pp, gameID == target_game) # lagged data set
filter(df_int, gameID == target_game) # actual game outcome
filter(test2, (LTeamID == 1160 | WTeamID == 1160) & DayNum < 24 & Season == target) # OE history of winning team
filter(test2, (LTeamID == 1102 | WTeamID == 1102) & DayNum < 24 & Season == target) # OE history of losing team

#### Merge with OPPONENT STATS #####
# merge with self
out <- merge_oppadj(df_int_pp, matcher = "gameID")
df_model <- select(out, -result_opp, -Season_opp)
df_model$OE_diff = df_model$OE - df_model$OE_opp

##### PREPARE TRAINING AND TEST SETS #####
# Various Methods: https://stackoverflow.com/questions/17200114/how-to-split-data-into-training-testing-sets-using-sample-function
train_ind <- sample(seq_len(nrow(df_model)), size = nrow(df_model)*0.8, replace = FALSE)
train <- df_model[train_ind, ]
test <- df_model[-train_ind, ]


