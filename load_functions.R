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

