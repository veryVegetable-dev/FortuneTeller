library(rpart)

logPath = "train_data.txt"

# train data 
log <- read.delim(logPath)
y = log$real_luck
period = log$period
sleeping = log$sleeping
resting_heart_rate = log$resting_heart_rate
task=log$task
duration=log$duration

# model fitting
rpart_param = rpart.control(minsplit = 1, minbucket = 1)
cfit <- rpart(y ~ period + sleeping + resting_heart_rate + task + duration, data = log, method = 'class', control = rpart_param)
saveRDS(cfit, "teller.model")
my_model = readRDS("teller.model")
my_model

# predict on new data 
test_data = log[nrow(log), ]
data.frame(predict(my_model, newdata=test_data, type="prob"))[, 2]


a = strsplit("1 1 1", split = " ")
df = data.frame(c(1, 2, 3))



