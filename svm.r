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

Date.decade = function(dates) {
  years = as.numeric(format(dates, "%Y"))
  years %/% 10
}

## Cleaning

data = read.csv("data.csv")
data$Declaration.Date = as.Date(data$Declaration.Date, tryFormats="%m/%d/%Y")
data$Start.Date = as.Date(data$Start.Date, tryFormats="%m/%d/%Y")
data$End.Date = as.Date(data$End.Date, tryFormats="%m/%d/%Y")

## EDA Predictors

data$Start.Month = as.factor(months(data$Start.Date))
data$Start.Season = as.factor(Date.season(data$Start.Date))
data$Disaster.Duration = as.numeric(data$End.Date - data$Start.Date)
data$State.Region = as.factor(state.region[sapply(data$State, match, table=state.abb)])
data$Start.Decade = as.numeric(Date.decade(data$Start.Date))

## Omitted Data

data = data[,!(names(data) %in% c(
  "Declaration.Date",
  "End.Date",
  "Declaration.Number",
  "Declaration.Type",
  "Disaster.Title",
  "Close.Date"
))]

data = drop_na(data)

## Training

set.seed(0)
train <- sample(nrow(data), 0.8 * nrow(data))

library(e1071)
svm.fit = svm(Disaster.Type ~ ., data=data[train,], kernel="polynomial", gamma=1, cost=1)
svm.pred <- predict(svm.fit,newdata=data[-train,])
mean(svm.pred != data[-train,]$Disaster.Type) # misclassification error rate

## Tuning

stune = sample(nrow(data), 1000)
svm.tune = tune.svm(Disaster.Type ~ ., data=data[stune,],
                    kernel="polynomial",
                    cost=c(0.1, 0.5, 1, 5, 10),
                    gamma=c(0.1, 0.5, 1, 5, 10))
