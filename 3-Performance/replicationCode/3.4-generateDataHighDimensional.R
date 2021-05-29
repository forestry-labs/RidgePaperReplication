library(Rforestry)
library(MASS)

set.seed(3984938)

p <- 100

nonlinear_func <- function(x) {
  return(2/(1+exp(-12*(x-.5))))
}

# A high dimensional simulation inspired by Wager and Athey 2018 ===============
n_train <- 500
n_test <- 10000
n <- n_train + n_test

x <- data.frame(matrix(rnorm(n*p), ncol = p, nrow = n))
y <- sapply(x[,1], nonlinear_func)*sapply(x[,2], nonlinear_func) +
  sapply(x[,3], nonlinear_func)*sapply(x[,4], nonlinear_func)

data <- data.frame(x,y)

datasets_grid[["HighDimensionalSmall"]] <- list(
  "train" = data[1:n_train,] + rnorm(n_train, sd = 2),
  "test" = data[(n_train+1):(n_train + n_test),]
)

# A lrager version dimensional simulation inspired by Wager and Athey 2018 =====
n_train <- 1000
n_test <- 10000
n <- n_train + n_test

x <- data.frame(matrix(rnorm(n*p), ncol = p, nrow = n))
y <- sapply(x[,1], nonlinear_func)*sapply(x[,2], nonlinear_func) +
  sapply(x[,3], nonlinear_func)*sapply(x[,4], nonlinear_func)

data <- data.frame(x,y)

datasets_grid[["HighDimensionalLarge"]] <- list(
  "train" = data[1:n_train,] + rnorm(n_train, sd = 2),
  "test" = data[(n_train+1):(n_train + n_test),]
)


