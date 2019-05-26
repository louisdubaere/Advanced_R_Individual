factorize_columns <- function(data, columns) {
  copy <- data.frame(data)
  copy[columns] <- lapply(copy[columns], factor)
  return(copy)
}

numerical_columns <- function(data) {
  nums <- unlist(lapply(data, is.numeric))
  return(nums)
}

drop_columns <- function(data, columns) {
  copy <- data.frame(data)
  return( copy[ , -which(names(copy) %in% columns)])
}

split_train_test <- function(df, train=0.8, seed=2112) {
  set.seed(seed)
  
  nrows = nrow(df) #total number of rows in data
  ntrain = round(train * nrows) # number of rows for train,70%
  ntest = round((1-train)* nrows) # number of rows for test, 30%
  index = seq(1:nrows)
  trainIndex = sample(index, ntrain) #train data set
  testIndex = index[-trainIndex]
  
  train = df[trainIndex,]
  test = df[testIndex,]
  
  mylist = list(train, test)
  names(mylist) <- c("Train","Test")
  return(mylist)
}

mape<-function(real,predicted){
  return(mean(abs((real-predicted)/real)))
}

calculate_MAPE_lm <- function(data) {
  splitted <- split_train_test(data)
  lm <- lm(price~., data = splitted$Train)
  p <- predict(lm, splitted$Test)
  MAPE <- mape(predicted = p, real = splitted$Test$price)
  return(MAPE)
}

calculate_MAPE_dt <- function(data) {
  splitted <- split_train_test(data)
  fit <- rpart(price~., data = splitted$Train)
  p <- predict(fit, splitted$Test)
  MAPE <- mape(predicted = p, real = splitted$Test$price)
  return(MAPE)
}

calculate_MAPE_rf <- function(data) {
  splitted <- split_train_test(data)
  rf <- randomForest(price~., data = splitted$Train)
  p <- predict(rf, splitted$Test)
  MAPE <- mape(predicted = p, real = splitted$Test$price)
  return(MAPE)
}

get_train_data_MAPE_lm <- function(data, train_n, price) {
  train <- cbind(data[1:train_n,], price)
  
  train <-train[train$grade != "3",]
  train <-train[train$bathrooms != "5.75",]
  
  #colnames(train)[20] <- "price"
  calculate_MAPE_lm(train)
}

get_train_data_MAPE_rf <- function(data, train_n, price) {
  train <- cbind(data[1:train_n,], price)
  
  train <-train[train$grade != "3",]
  train <-train[train$bathrooms != "5.75",]
  
  #colnames(train)[20] <- "price"
  calculate_MAPE_rf(train)
}

predict_house_prices_lm <- function(data, train_n, price) {
  train <- cbind(data[1:train_n,], price)
  test <- data[(train_n+1):nrow(data),]
  
  lm <- lm(price~., data = train)
  
  test[test$bathrooms == "5.5",]$bathrooms <- "5.25"
  test[test$bathrooms == "6.75",]$bathrooms <- "6"
  test[test$grade == "13",]$grade <- "12"
  
  p <- predict(lm, test)
  
  return(p)
}
