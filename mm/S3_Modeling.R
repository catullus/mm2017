# S3 - Modelling
# logit test
# takes into account previous performance and adjusts for opponents incoming stats
m1 <- glm(formula = result ~ OE_diff, family = "binomial", data = na.omit(train));summary(m1)

# randomForest
m2 <- randomForest(as.factor(result) ~ OE_diff + OR + OR_opp + FTA + FTA_opp, data = train);m2
