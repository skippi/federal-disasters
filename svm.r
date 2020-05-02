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
  "Disaster.Title"
)

data = read.csv("data.csv")
data = omit.cols(data, omitted_cols)
data = replace.date(data, "Declaration.Date")
data = replace.date(data, "Start.Date")
data = replace.date(data, "End.Date")
data = replace.date(data, "Close.Date")

set.seed(0)
train <- sample(nrow(data), 0.25 * nrow(data))

library(e1071)
svm.fit = svm(Disaster.Type ~ ., data=data[train,], kernel="radial", gamma=1, cost=1)
svm.pred <- predict(svm.fit,newdata=data[-train,])
mean(svm.pred != data[-train,]$Disaster.Type) # misclassification error rate
