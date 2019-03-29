# data = long data frame (i.e. rbind) 
merge_oppadj <- function(data, matcher){
    dat = merge(x = na.omit(data), y = na.omit(data), by.x = matcher, by.y= matcher, suffixes = c("", "_opp")) %>% dplyr::filter(TeamID != TeamID_opp)
}
