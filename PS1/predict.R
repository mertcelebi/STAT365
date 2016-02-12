# ====== Helpers ======

prep_program <- function () {
  library(FNN)
  library(glmnet)
  library(GENEAread)

  # Read data.
  nyc_train <- read.csv("~/Desktop/Yale/Yale Spring 2016/STAT 365/Assignments/PS1/nyc_train.csv")
  nyc_test <- read.csv("~/Desktop/Yale/Yale Spring 2016/STAT 365/Assignments/PS1/nyc_test.csv")
  return (list(nyc_train = nyc_train, nyc_test = nyc_test))
}

prep_linear <- function (nyc_train, nyc_test) {
  # Set the explanatory and response variables for the training set.
  train <- nyc_train[, c("pickup_datetime", "pickup_NTACode", "dropoff_BoroCode")]
  train[c(1)] <- lapply(train[c(1)], function(x) as.POSIXlt(x)$hour)
  train[c(3)] <- unlist(lapply(train[, 3], function(x) if (x != 1) 1 else 0))
  train[, 1] <- factor(train[, 1])
  train[, 2] <- factor(train[, 2])
  train[, 3] <- factor(train[, 3])

  # Set the explanatory variables for the test set.
  test <- nyc_test[, c("pickup_datetime", "pickup_NTACode")]
  test[c(1)] <- lapply(test[c(1)], function(x) as.POSIXlt(x)$hour)
  test[, 1] <- factor(test[, 1])
  test[, 2] <- factor(test[, 2])

  return (list(train = train, test = test))
}

prep_ridge <- function (nyc_train, nyc_test) {
  # Set the explanatory and response variables for the training set.
  pickup_datetime <- strptime(nyc_train$pickup_datetime, "%Y-%m-%d %H:%M:%S")[[3]]
  pickup_datetime <- model.matrix(~factor(pickup_datetime))
  neighborhood <- model.matrix(~(nyc_train$pickup_NTACode))
  train <- cbind(neighborhood, pickup_datetime)
  response <- nyc_train["dropoff_BoroCode"] != 1

  pickup_datetime <- strptime(nyc_test$pickup_datetime, "%Y-%m-%d %H:%M:%S")[[3]]
  pickup_datetime <- model.matrix(~factor(pickup_datetime))
  neighborhood <- model.matrix(~(nyc_test$pickup_NTACode))
  test <- cbind(neighborhood, pickup_datetime)
  return (list(train = train, test = test, response = response))
}

write_to_csv <- function (knn, linear, ridge, name = "~/Desktop/Yale/Yale Spring 2016/STAT 365/Assignments/PS1/pset01.csv") {
  x <- data.frame(knn, linear, ridge)
  write.table(x, file = name, row.names = FALSE, col.names = FALSE, sep = ",")
}

# ====== KNN ======

predict_knn <- function (nyc_train, nyc_test) {
  k <- 100
  # Extract columns of importance.
  train <- nyc_train[, c("pickup_longitude", "pickup_latitude")]
  test <- nyc_test[, c("pickup_longitude", "pickup_latitude")]

  # Set the labels for analysis.
  labels <- nyc_train[, c("dropoff_BoroCode")]
  labels <- factor(unlist(lapply(labels, function(x) if (x != 1) 1 else 0)))

  # Run the knn algorithm.
  predictions <- knn(train, test, labels, k, prob = TRUE)
  probabilities_knn <- attr(predictions, "prob")

  for (index in 1 : length(probabilities_knn)) {
    if (predictions[index]) {
      probabilities_knn[index] <- 1 - probabilities_knn[index]
    }
  }

  return (probabilities_knn)
}

# ====== Linear/Ridge Regressions ======

predict_linear <- function (nyc_train, nyc_test) {
  variables <- prep_linear(nyc_train, nyc_test)

  # Run the linear regression.
  model_linear <- lm(formula = dropoff_BoroCode ~ pickup_datetime + pickup_NTACode, data = variables$train)
  probabilities_linear <- predict(model_linear, variables$test, type = "response")

  for (index in 1 : length(probabilities_linear)) {
    probabilities_linear[index] <- probabilities_linear[index] - 1
  }
  return (probabilities_linear)
}

predict_ridge <- function (nyc_train, nyc_test) {
  variables <- prep_ridge(nyc_train, nyc_test)

  # Run the ridge regression.
  model_ridge <- cv.glmnet(x = variables$train, y = variables$response, family="gaussian", alpha = 0)
  probabilities_ridge <- predict(model_ridge, variables$test, type = "response")
  return (probabilities_ridge)
}

# ====== Main Program ======

data <- prep_program()
probabilities_linear <- predict_linear(data$nyc_train, data$nyc_test)
probabilities_ridge <- predict_ridge(data$nyc_train, data$nyc_test)
probabilities_knn <- predict_knn(data$nyc_train, data$nyc_test)
write_to_csv(probabilities_knn, probabilities_linear, probabilities_ridge)
