# E1 - random forests
library(ggplot2)

hist(pred_results$pred_binary.win)

ggplot(arrange(rg_imp, desc(importance)), aes(var, importance)) + geom_bar(stat='identity') + coord_flip()

## input data objects
# pred_results (output from test data run through M1.R object)
# pred_totals (simple stats of pred_results e.g. # correct predictions / # of total predictions)
# 

#### QUESTION 1 ####
## where are test prediction results off?
## first round predictions?
## low vs. high seed games?


## step one, join output pred_results data with tourny and tourny seed data
pred_results$t_gameid <- paste0(pred_results$Season, "_", pred_results$Wteam, pred_results$Lteam)

ddply(pred_results, ~pred_binary, summarize, 
      count = length(pred_binary),
      percent_oftotal = round(length(pred_binary)/nrow(pred_results),2),
      calcloss_avg = mean(pred_prob.loss), 
      calcwin_avg=mean(pred_prob.win))


ddply(pred_results, ~pred_binary+WSeed_num, summarize, 
      count = length(pred_binary), 
      percent_oftotal = round(length(pred_binary)/nrow(pred_results),2),
      calcloss_avg = mean(pred_prob.loss), 
      calcwin_avg=mean(pred_prob.win))

## ENSEMBLE MODEL APPROACH = look at the number of "wrong" predictions for 1-5 seeds (i.e. labeled as 'loss')
## can "boost" prediction accuracy by a few percent if we default classify 1-2 seed outcomes in first round to a 'win'
## ...there are not many the model actually classifies as a 1-5 seed loss in first round (daynum 136-139)
ddply(pred_results, ~pred_binary+WSeed_num+Daynum, summarize, 
      count = length(pred_binary), 
      percent_oftotal = round(length(pred_binary)/nrow(pred_results),2),
      calcloss_avg = mean(pred_prob.loss), 
      calcwin_avg=mean(pred_prob.win))

dplyr::filter(pred_results, WSeed_num == 16) ## these look like 16 vs 16 wild card entries, just remove
dplyr::filter(pred_results, WSeed_num == 1)
