# E1 - random forests
library(ggplot2)
ggplot(arrange(rg_imp, desc(importance)), aes(var, importance)) + geom_bar(stat='identity') + coord_flip()

arrange(rg_imp, desc(importance))
