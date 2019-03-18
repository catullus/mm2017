## function - previous perf for NCAA team box stats
## test 
# t1 <- previous_perf(select(gstats, scored, Last.Name, First.Name, gameid, week, Home.Away, Position, Goals, Shots, ShotsOnGoal), grouper = "Last.Name", width = 3, exclude = c("Last.Name", "First.Name","Position", "Home.Away", "Opponent", "gameid","week", "scored", "X"))

### function_previous_perf_v5
### wrapper function -- calls pp() to reduce amount of code and object passing
previous_perf <- function(data, grouper="team", arranger = "Daynum", exclude = c("Season", "Daynum", "loc", "win_loss"), func = mean, width = 3, ...){
    ## data = numeric columns you want to analyze/summarise, includes the factor/descriptive columns as exclude
    ## grouper = variable you want to split data by
    require(dplyr)
    ## arrange rows in order 
    data <- data %>% arrange_(arranger)
    out_p <- data %>% group_by_(grouper) %>% do(., data.frame(pp(., exclude = c(exclude), func = func, width = width)))
    return(out_p)
}

pp <- function(data, width=3, func=mean, fill=NA, exclude = c("Season", "Daynum", "loc", "win_loss"), ...){
    require(zoo)
    require(dplyr)
    # data = a data frame with meta-data and stats
    # width = how many "rows" back you want the function to 
    # exclude = columns that wont be averaged i.e. dates and text data
    # func = the type of summary function you want to use e.g. median, st dev, mean
    # fill = what you want blank rows represented as, NA by default, could use zero but might heavily bias
    groupdata = dplyr::select(data, -one_of(exclude)) 
    
    # calculate previous value width = -1 means "use previous row, ignore current", partial = "don't toss data"
    calc = zoo::rollapplyr(groupdata, width=list(-width:-1), func, fill=fill, partial=TRUE)
    
    # join back with rest of data
    rejoin = dplyr::select(data, one_of(exclude))
    
    # recombine with meta-data
    out = cbind(rejoin, calc)
    
    # export data
    return(out)
}

