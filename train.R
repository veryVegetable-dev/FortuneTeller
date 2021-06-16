library(rpart)

logPath = "pred"

# train data 
log <- read.delim(logPath)
y = log$fixed_luck
period = log$period
sleeping = log$sleeping
resting_heart_rate = log$resting_heart_rate
task=log$task
duration=log$duration

# model fitting
rpart_param = rpart.control(minsplit = 1, minbucket = 1)
cfit <- rpart(y ~ period + sleeping + resting_heart_rate + task + duration, data = log, method = 'class', control = rpart_param)
saveRDS(cfit, "teller.model")


