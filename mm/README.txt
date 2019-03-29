Code Order of Operations

1. S1 (script 1) - Process Detailed = script that takes regular season detailed box stats
-FOCUS: the stats of a game tell us how a team performed, but does not necessarily tell us whether those stats can 
predict or correlate with the outcomes of future games
-Original data structure = 1 row with a winning AND losing team and those team's stats (wide format)
-Example calculates basic possession and points per possession (described as Offensive Efficiency or column_name ="OE")
-Stack/rbind() data into long format
-Modify column names to remove "W" and "L" to make stacking work
-Arrange games per Season per team per DayNum
-For each Team estimate "previous performance", calculate the average of a team's stats UP TO a game's result (pp_wrap())
-Merge the previous previous for teams of each gameID
-Create test and training data set

2. S2 (script 2) - Plot various relationships
-within game stats: offensive efficiency is nearly always higher for winning team in 99% of games -- a logistic regression will not converge because of perfect separation
-The trick then is to estimate how large of an interval is relevant to a team's future game performance, the last 3 games, the last 10?

3. S3 (script 3) - Create models for predictions