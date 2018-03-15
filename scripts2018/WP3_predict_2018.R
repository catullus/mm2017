#### WP3_predict_2018

# That is their last 5 game average before getting into the tournament
# output data = reg_long_laststat
#Open the datasets:
if (length(list.files("C:/Users/jroberti/Git/mm2017/data2018/")) > 0){
    inpath <- "C:/Users/jroberti/Git/mm2017/"
} else if (length(list.files("C:/Users/cflagg/Documents/GitHub/mm2017/data2018/")) > 0 ) {
    inpath <- "C:/Users/cflagg/Documents/GitHub/mm2017/"   
} else if (length(list.files("C:/Users/Amy/Documents/GitHub/mm2017/data2018/")) > 0) {
    inpath <- "C:/Users/Amy/Documents/GitHub/mm2017/"
}

seeds2018 <- read.csv(paste0(inpath, "data2018/NCAATourneySeeds.csv"), stringsAsFactors=FALSE)
seeds2018 <- dplyr::filter(seeds2018, Season == 2018)

## this should result in 68 rows i.e. the 68 teams playing in the tourny
submission_data <- reg2018_pp_5w %>% filter(team %in% seeds2018$TeamID) %>% arrange(desc(daynum)) %>% group_by(team) %>%  slice(n()) 

# stolen from: https://stackoverflow.com/questions/17171148/non-redundant-version-of-expand-grid
expand.grid.unique <- function(x, y, include.equals=FALSE)
{
    x <- unique(x)
    
    y <- unique(y)
    
    g <- function(i)
    {
        z <- setdiff(y, x[seq_len(i-include.equals)])
        
        if(length(z)) cbind(x[i], z, deparse.level=0)
    }
    
    do.call(rbind, lapply(seq_along(x), g))
}

all_matchups <- expand.grid.unique(seeds2018$TeamID, seeds2018$TeamID)
all_matchups <- data.frame(all_matchups)
names(all_matchups) <- c("x", "y")

all_matchups <- arrange(all_matchups, x)
all_matchups$id <- paste0("2018_", all_matchups$x, "_", all_matchups$y)
