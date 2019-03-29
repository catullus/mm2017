## S2 Plotting 
# plot
ggplot(df_model, aes(x = OE, y = FGA, color = result)) + geom_point()
ggplot(train, aes(x = OE_diff, color = result, group = result)) + geom_histogram() + facet_wrap(~result)

