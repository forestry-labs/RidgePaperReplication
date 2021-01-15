# try(setwd("~/Dropbox/ridgeEvaluation/"), silent = TRUE)

library(forestry)
library(MASS)


set.seed(776291)
n_train <- 2100
n_test <- 10000
n <- n_train + n_test
sd <- 1
p <- 10
x <- matrix(rnorm(p * n), nrow = n, ncol = p)

# Artificially created data sets -----------------------------------------------
# Dataset #1 Linear ------------------------------------------------------------
b <- rep(0, p)
b[2] <- -0.47 
b[3] <- -0.98 
b[4] <- 0.87 
b[8] <- 0.63 
b[10] <- 0.64

y <- x %*% b
x <- as.data.frame(x)
lm_artificial_ds <- cbind(x, y)

for (nobs in 128 * 2^(0:4)) {
  set.seed(776291)
  
  data_name <- paste0("artificial-LM-", nobs)
  
  datasets_grid[[data_name]] <- list(
    "test" = lm_artificial_ds[1:n_test,],
    "train" = lm_artificial_ds[(n_test + 1):(n_test + nobs),] %>%
      mutate(y = y +  3 * sd * rnorm(nobs)))
  
}

# Dataset #2 Step Function -----------------------------------------------------
set.seed(24332333)
num_levels <- 50
y_levels <- runif(num_levels, -10, 10) 
sample_idx <- sample(1:nrow(x), num_levels)

reg <- forestry(x = x[sample_idx,],
                y = y_levels,
                nodesizeSpl = 1,
                nodesizeStrictSpl = 1,
                nodesizeAvg = 1,
                nodesizeStrictAvg = 1)

y <- predict(reg, x)

simulated_step <- as.data.frame(cbind(x, y))

for (nobs in 128 * 2 ^ (0:4)) {
  set.seed(776291)
  
  
  data_name <- paste0("simulated-Step-Function-", nobs)
  
  datasets_grid[[data_name]] <- list(
    "test" = simulated_step[1:n_test,],
    "train" = simulated_step[(n_test + 1):(n_test + nobs),] %>%
      mutate(y = y +  sd * rnorm(nobs)))
  
}

# Dataset #3 half Step halfLinear Function -------------------------------------
set.seed(24332333)
# simulated y step ------------------
simulated_y_step <- predict(reg, x)
# simulated linear y ----------------
simulated_y_linear <- as.matrix(x) %*% b

y <- ifelse(x[,1] < .5, simulated_y_linear, simulated_y_step)

simulated_StepLinear <- as.data.frame(cbind(x, y))

for (nobs in 128 * 2 ^ (0:4)) {
  set.seed(776291)
  
  
  data_name <- paste0("simulated-StepLinear-Function-", nobs)
  
  datasets_grid[[data_name]] <- list(
    "test" = simulated_StepLinear[1:n_test,],
    "train" = simulated_StepLinear[(n_test + 1):(n_test + nobs),] %>%
      mutate(y = y +  sd * rnorm(nobs)))
  
}

# str(datasets_grid)
