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
all_matchups <- as.data.frame(all_matchups)
names(all_matchups) <- c("team_x", "team_y")

all_matchups$ID <- paste0("2018_", all_matchups$team_x, "_", all_matchups$team_y)

"2018_1104_1158" %in% all_matchups$ID
