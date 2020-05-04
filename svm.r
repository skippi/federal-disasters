library(tidyr)

replace.date = function(df, pred_name) {
  names = c(sprintf("%s.Month", pred_name), sprintf("%s.Day", pred_name), sprintf("%s.Year", pred_name))
  
  splited = separate(df, pred_name, names, "/", convert=TRUE)
  
  return(splited)
}

omit.cols = function(df, names) {
  return(df[, !(names(df) %in% names)])
}

omitted_cols = c(
  "Declaration.Number",
  "Declaration.Type",
  "Disaster.Title",
  "Close.Date"
)

data = read.csv("data.csv")
data = omit.cols(data, omitted_cols)
data = replace.date(data, "Declaration.Date")
data = replace.date(data, "Start.Date")
data = replace.date(data, "End.Date")

set.seed(0)
train <- sample(nrow(data), 0.25 * nrow(data))

library(e1071)
svm.fit = svm(Disaster.Type ~ ., data=data[train,], kernel="radial", gamma=1, cost=1)
svm.pred <- predict(svm.fit,newdata=data[-train,])
mean(svm.pred != data[-train,]$Disaster.Type) # misclassification error rate


## Tune

svm.tuned = tune(svm, Disaster.Type ~ ., data=data[train,],
                 kernel="radial",
                 ranges=list(
                   cost=c(0.1,1,10,100,1000),
                   gamma=c(0.5,1,2,3,4))
                 )

summary(tune.out)

