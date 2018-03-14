## school admission probability
admin <- read.csv("https://stats.idre.ucla.edu/stat/data/binary.csv")

#### glm test ####
glm1 <- glm(admit ~ gre + gpa + rank, data = admin, family = "binomial")

admin$glm_pred <- predict(glm1, type = "response")

head(admin)

#### random forest test ####
rf1 <- randomForest(as.factor(admit) ~ gre + gpa + rank, data = admin, ncores=8, parallel = TRUE)

admin$rf_pred <- predict(rf1, type = 'prob')[,2]
admin$rf_pred_bin <- predict(rf1, type = 'response')

#### ranger test ####
ranger1 <- ranger(as.factor(admit) ~ gre + gpa + rank, data = admin)
ranger1_prob <- ranger(as.factor(admit) ~ gre + gpa + rank, data = admin, probability = TRUE)

admin$ranger_pred <- predict(ranger1_prob, data = admin)$prediction[,2]

admin$ranger_pred_bin <- predict(ranger1, data = admin)$prediction

xtabs(~admit + ranger_pred_bin + rf_pred_bin, data = admin)

#### caret training #### 
library(caret)
trControl = trainControl(method = "cv", number = 10, allowParallel = TRUE, verboseIter = FALSE)
modfit <- train(as.factor(admit) ~ ., data = admin, method = "rf", prox = FALSE, trControl = trControl)
modfit2 <- train(as.factor(admit) ~ ., data = admin, method = "rf", prox = FALSE, trControl = trControl)
admin$caret_pred <- testclass <- predict(modfit, newdata = admin)

sa <- combine(modfit, modfit2)
