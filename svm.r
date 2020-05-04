rm(list=ls())

library(tidyr)

Date.season = function(DATES) {
  winter = as.Date("2012-12-15") # Winter Solstice
  spring = as.Date("2012-3-15") # Spring Equinox
  summer = as.Date("2012-6-15") # Summer Solstice
  fall = as.Date("2012-9-15") # Fall Equinox

  d = as.Date(strftime(DATES, format="2012-%m-%d"))

  ifelse(winter <= d | d < spring, "Winter",
    ifelse(spring <= d & d < summer, "Spring",
      ifelse(summer <= d & d < fall, "Summer", "Fall")))
}

omitted_cols = c(
  "Declaration.Number",
  "Declaration.Type",
  "Disaster.Title",
  "Close.Date"
)

## Cleaning

data = read.csv("data.csv")
data = data[, !(names(data) %in% omitted_cols)]
data = drop_na(data)
data$Declaration.Date = as.Date(data$Declaration.Date, tryFormats="%m/%d/%Y")
data$Start.Date = as.Date(data$Start.Date, tryFormats="%m/%d/%Y")
data$End.Date = as.Date(data$End.Date, tryFormats="%m/%d/%Y")

## EDA Predictors

data$Start.Month = as.factor(months(data$Start.Date))
data$Start.Season = as.factor(Date.season(data$Start.Date))
data$Duration = as.numeric(data$End.Date - data$Start.Date)

## Training

set.seed(0)
train <- sample(nrow(data), 0.2 * nrow(data))

library(e1071)
svm.fit = svm(Disaster.Type ~ ., data=data[train,], kernel="polynomial", gamma=1)
svm.pred <- predict(svm.fit,newdata=data[-train,])
mean(svm.pred != data[-train,]$Disaster.Type) # misclassification error rate
