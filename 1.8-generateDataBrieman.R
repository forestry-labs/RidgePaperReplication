try(setwd("~/Dropbox/ridgeEvaluation/"), silent = TRUE)


library(MASS)
library(gclus)
library(forestry)

datasets_grid <- list()

data(ozone)
Boston
servo <- read.csv("replicationCode/servo.csv", header = TRUE)
abalone <- read.csv("replicationCode/abalone.csv")

set.seed(3984938)

# Simulated Datasets -----------------------------------------------------------

# Friedman 1
n <- 3000
x <- matrix(runif(10 * n), nrow = n, ncol = 10)
x <- as.data.frame(x)

y <- 10*sin(pi*x[,1]*x[,2]) + 20*(x[,3] - .5)^2 + 10*x[,4] + 5*x[,5] + rnorm(n, sd = 1)
friedman_1 <- cbind(x, y)

test_id <- 1001:3000
train_id <- 1:1000

datasets_grid[["Friedman_1"]] <- list(
  "train" = friedman_1[train_id, ],
  "test" = friedman_1[test_id, ])

# Friedman 2
# Error SD selected to roughly give signal to noise ratio of 3:1
x1 <- runif(n, min = 0, max = 100)
x2 <- runif(n, min = 40*pi, max = 560*pi)
x3 <- runif(n, min = 0.01, max = 1)
x4 <- runif(n, min = 1, max = 11)

x <- data.frame(x1, x2, x3, x4)

y <- x[, 1] ^ 2 + (x[, 2] * x[, 3] - (1 / (x[, 2] * x[, 3])) ^ 2) ^ (.5)

noise <- rnorm(n)

ratio <- sqrt(var(y)/(3*var(noise)))

y <- y + ratio * noise

friedman_2 <- cbind(x, y)
# summary(friedman_2)
# 
datasets_grid[["Friedman_2"]] <- list(
  "train" = friedman_2[train_id, ],
  "test" = friedman_2[test_id, ])

# Friedman 3
# Error SD selected to roughly give signal to noise ratio of 3:1
# 
y <- atan( (x[,2]*x[,3] - (1/(x[,2]*x[,4]) )) / x[,1])

noise <- rnorm(n)

ratio <- sqrt(var(y)/(3*var(noise)))

y <- y + ratio * noise

friedman_3 <- cbind(x, y)


datasets_grid[["Friedman_3"]] <- list(
  "train" = friedman_3[train_id, ],
  "test" = friedman_3[test_id, ])


# Real Datasets ----------------------------------------------------------------

# Boston Housing

colnames(Boston)[ncol(Boston)] <- "y"
n <- nrow(Boston)

b <- Boston[,-ncol(Boston)]

for (i in 1:10) {
  noise <- rnorm(nrow(Boston), mean = 0, sd = 2)
  b <- cbind(b, noise)
  colnames(b)[ncol(b)] <- paste0("noise",i)
}

b <- cbind(b, Boston$y)

flds <- caret::createFolds(Boston$y, k = 5, list = TRUE, returnTrain = FALSE)

for (i in 1:length(flds)) {
  # i = 1
  test_id <- flds[[i]]
  train_id <- (1:n)[!(1:n) %in% test_id]


  datasets_grid[[paste0("aBoston_Housing_fold", i)]] <- list(
    "train" = b[train_id, ],
    "test" = b[test_id, ])
}

for (i in 1:length(flds)) {
  # i = 1
  test_id <- flds[[i]]
  train_id <- (1:n)[!(1:n) %in% test_id]


  datasets_grid[[paste0("Boston_Housing_fold", i)]] <- list(
    "train" = Boston[train_id, ],
    "test" = Boston[test_id, ])
}

# Ozone
ozone <- cbind(ozone, Ozone = ozone$Ozone)
ozone <- ozone[,-1]
n <- nrow(ozone)

colnames(ozone)[ncol(ozone)] <- "y"
n <- nrow(ozone)

flds <- caret::createFolds(ozone$y, k = 5, list = TRUE, returnTrain = FALSE)

for (i in 1:length(flds)) {
  # i = 1
  test_id <- flds[[i]]
  train_id <- (1:n)[!(1:n) %in% test_id]


  datasets_grid[[paste0("Ozone_fold", i)]] <- list(
    "train" = ozone[train_id, ],
    "test" = ozone[test_id, ])
}

# Servo
n <- nrow(servo)
servo_onehot_translator <- onehot::onehot(servo)
servo <- as.data.frame(predict(servo_onehot_translator, servo))
test_id <- sort(sample(n, size = round(.1*n)))
train_id <- (1:n)[!(1:n) %in% test_id]


colnames(servo)[ncol(servo)] <- "y"
colnames(servo)[1:(ncol(servo) - 1)] <- paste0("x", 1:(ncol(servo) - 1))
n <- nrow(servo)

flds <- caret::createFolds(servo$y, k = 5, list = TRUE, returnTrain = FALSE)

for (i in 1:length(flds)) {
  # i = 1
  test_id <- flds[[i]]
  train_id <- (1:n)[!(1:n) %in% test_id]


  datasets_grid[[paste0("Servo_fold", i)]] <- list(
    "train" = servo[train_id, ],
    "test" = servo[test_id, ])
}


# Abalone
abalone_onehot_translator <- onehot::onehot(abalone)
abalone <- as.data.frame(predict(abalone_onehot_translator, abalone))
n <- nrow(abalone)

colnames(abalone)[ncol(abalone)] <- "y"
colnames(abalone)[1:(ncol(abalone) - 1)] <- paste0("x", 1:(ncol(abalone) - 1))

test_id <- sort(sample(n, size = round(.5*n))); length(test_id)
train_id <- (1:n)[!(1:n) %in% test_id]; length(train_id)


datasets_grid[["Abalone"]] <- list(
  "train" = abalone[train_id, ],
  "test" = abalone[test_id, ])

str(datasets_grid)
names(datasets_grid)
