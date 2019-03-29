library(zoo)
pp <- function(data, exclude = c("TeamID", "gameID", "result"), width=1, func=mean, ...){
    # data = a data frame with meta-data and stats
    # width = how many "rows" back you want the function to 
    # exclude columns that wont be averaged i.e. dates and text data
    dat = dplyr::select(data, -one_of(exclude)) 
    
    # calculate previous value width = -1 means "use previous row, ignore current", partial = "don't toss data"
    calc = rollapplyr(dat, width=list(-width:-1), func, fill=NA, partial=TRUE)
    
    # join back with rest of data
    rejoin = dplyr::select(data, one_of(exclude))
    
    # recombine with meta-data
    out = cbind(rejoin, calc)
    
    # export data
    return(out)
}

pp_wrap <- function(data, exclusion = c("TeamID", "gameID", "result", "Season", "DayNum"), width = 1, func = mean){
    plyr::ddply(data, ~TeamID, function(x){
   pp(x, exclude = exclusion, width = width, func = func)
})
}


## how to implement pp()
#ddply(mystats, ~team, function(x){
#   pp(x, exclude = exclusion, width = 1)
#})


#### test 
#### type 1 ####
## works, kind of ugly output
#previous_perfo <- by(mystats, mystats[,"team"], pp, simplify = FALSE);previous_perfo

## https://magesblog.com/post/2012-01-28-say-it-in-r-with-by-apply-and-friends/

##### TYPE 2 ####
## pass to ddply
#exclusion <- c("team", "GameID", "win")

#ddply(mystats, ~team, function(x){
#   pp(x, exclude = exclusion, width = 1)
#})
