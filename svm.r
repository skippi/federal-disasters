rm(list=ls())

library(tidyr)

season.name = c("Spring", "Summer", "Fall", "Winter")

replace.date = function(df, pred_name) {
  names = c(sprintf("%s.Month", pred_name), sprintf("%s.Day", pred_name), sprintf("%s.Year", pred_name))
  splited = separate(df, pred_name, names, '/', convert=TRUE)
  return(splited)
}

Date.season = function(DATES) {
  winter = as.Date("2012-12-15") # Winter Solstice
  spring = as.Date("2012-3-15") # Spring Equinox
  summer = as.Date("2012-6-15") # Summer Solstice
  fall = as.Date("2012-9-15") # Fall Equinox

  d = as.Date(strftime(DATES, format="2012-%m-%d"))

  ordered(
    ifelse(winter <= d | d < spring, "Winter",
      ifelse(spring <= d & d < summer, "Spring",
        ifelse(summer <= d & d < fall, "Summer", "Fall"))),
    levels=season.name)
}

Date.decade = function(dates) {
  years = as.numeric(format(dates, "%Y"))
  years %/% 10
}

Date.year = function(dates) {
  as.numeric(format(dates, "%Y"))
}

## Cleaning

data = read.csv("data.csv")
data$Declaration.Date = as.Date(data$Declaration.Date, tryFormats="%m/%d/%Y")
data$Start.Date = as.Date(data$Start.Date, tryFormats="%m/%d/%Y")
data$End.Date = as.Date(data$End.Date, tryFormats="%m/%d/%Y")

## EDA Predictors

data$Start.Month = ordered(months(data$Start.Date), levels=month.name)
data$Start.Year = as.numeric(Date.year(data$Start.Date))
data$Start.Season = as.factor(Date.season(data$Start.Date))

## Unused EDA Predictors

# data$Disaster.Duration = as.numeric(data$End.Date - data$Start.Date)
# data$State.Region = as.factor(state.region[sapply(data$State, match, table=state.abb)])
# data$Start.Decade = as.numeric(Date.decade(data$Start.Date))

## Omitted Data

data = data[,!(names(data) %in% c(
  "Declaration.Date",
  "Declaration.Number",
  "Declaration.Type",
  "Disaster.Title",
  "County",
  "Start.Date",
  "End.Date",
  "Close.Date",
  "Individual.Assistance.Program",
  "Individuals...Households.Program",
  "Public.Assistance.Program",
  "Hazard.Mitigation.Program"
))]

data = drop_na(data)

## Training

set.seed(0)
train <- sample(nrow(data), 0.8 * nrow(data))

library(e1071)
svm.fit = svm(Disaster.Type ~ ., data=data[train,], kernel="polynomial", gamma=1, cost=1)
svm.pred <- predict(svm.fit,newdata=data[-train,])
mean(svm.pred != data[-train,]$Disaster.Type) # misclassification error rate

svm.fit

plot(svm.fit, data[train,], Disaster.Type ~ Start.Month + Start.Year)

## Part D Training

set.seed(0)
train <- sample(nrow(data), 0.8 * nrow(data))

library(e1071)
svm.fit = svm(Disaster.Type ~ ., data=data, kernel="polynomial", gamma=1, cost=1)
svm.pred <- predict(svm.fit, newdata=data)
mean(svm.pred != data$Disaster.Type) # misclassification error rate


## Tuning

stune = sample(nrow(data), 1000)
svm.tune = tune.svm(Disaster.Type ~ ., data=data[stune,],
                    kernel="polynomial",
                    cost=c(0.1, 0.5, 1, 5, 10),
                    gamma=c(0.1, 0.5, 1, 5, 10))
