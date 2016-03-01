# recordar como hacer modelos y usar confusion matrix
library(ElemStatLearn)
library(caret)
data(vowel.train)
data(vowel.test)
vowel.train$y <- as.factor(vowel.train$y)
vowel.test$y <- as.factor(vowel.test$y)
set.seed(33833)
mod.rf <- train(y~., data = vowel.train, method = 'rf')
mod.boost <- train(y~., data= vowel.train, method = 'gbm', verbose = FALSE)
pred.rf <- predict(mod.rf, vowel.test)
pred.boost <- predict(mod.boost, vowel.test)
confusionMatrix(pred.rf, vowel.test$y)
confusionMatrix(pred.boost, vowel.test$y)
confusionMatrix(pred.boost, pred.rf)

# ensemble models

library(caret)
library(gbm)
set.seed(3433)
library(AppliedPredictiveModeling)
data(AlzheimerDisease)
adData = data.frame(diagnosis,predictors)
inTrain = createDataPartition(adData$diagnosis, p = 3/4)[[1]]
training = adData[ inTrain,]
testing = adData[-inTrain,]

set.seed(62433)
mod.rf <- train(diagnosis~., data = training, method = 'rf')
mod.boost <- train(diagnosis~., data= training, method = 'gbm', verbose = FALSE)
mod.lda <- train(diagnosis~., data= training, method = 'lda')
pred.rf <- predict(mod.rf, testing)
pred.boost <- predict(mod.boost, testing)
pred.lda <- predict(mod.lda, testing)

stack <- data.frame(pred.rf, pred.boost, pred.lda, diagnosis = testing$diagnosis )
mod.ens <- train(diagnosis~., data = stack, method = 'rf')
pred.ens <- predict(mod.ens, testing$diagnosis)
confusionMatrix(pred.rf, testing$diagnosis)
confusionMatrix(pred.boost, testing$diagnosis)
confusionMatrix(pred.lda, testing$diagnosis)
confusionMatrix(pred.ens, testing$diagnosis)

# lasso

set.seed(3523)
library(AppliedPredictiveModeling)
data(concrete)
inTrain = createDataPartition(concrete$CompressiveStrength, p = 3/4)[[1]]
training = concrete[ inTrain,]
testing = concrete[-inTrain,]
set.seed(233)
mod.lasso <- train(CompressiveStrength~., data = concrete, method = "lasso")
plot.enet(mod.lasso$finalModel, xvar="penalty", use.color = TRUE)

#time series

library(forecast)
library(lubridate) # For year() function below
dat = read.csv("gaData.csv")
training = dat[year(dat$date) < 2012,]
testing = dat[(year(dat$date)) > 2011,]
tstrain = ts(training$visitsTumblr)
tstest = ts(testing$visitsTumblr)
mod.ts <- bats(tstrain)
h <- dim(testing)[1]
fcast <- forecast(mod.ts, level = 95, h = h)
accuracy(fcast, testing$visitsTumblr)
sum(testing$visitsTumblr<fcast$upper & testing$visitsTumblr>fcast$lower)/length(testing$visitsTumblr)

#

set.seed(3523)
library(AppliedPredictiveModeling)
library(e1071)
data(concrete)
inTrain = createDataPartition(concrete$CompressiveStrength, p = 3/4)[[1]]
training = concrete[ inTrain,]
testing = concrete[-inTrain,]
set.seed(325)
mod.svm <- svm(CompressiveStrength~., data = training)
pred.svm <- predict(mod.svm, testing)
accuracy(pred.svm, testing$CompressiveStrength)
