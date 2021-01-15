# A Simple example illustrating the flexibility of Linear Random Forests
setwd("~/Dropbox/ridgeEvaluationCode")
set.seed(4335)
library(forestry)
library(grf)
library(ggplot2)
library(dplyr)
library(reshape)


n <- 100
x <- runif(n,0,2 )
noise_less_y <- ifelse(x > 1, -2*x+ 4, 2*x )
y <- noise_less_y + rnorm(n, sd = .3) 
df <- data.frame(x,y)

# Locally linear data set ------------------------------------------------------
#plot <- ggplot(data = df, aes(x = x, y = y)) + geom_point(shape = 1) + theme_classic()


# Standard CART tree -----------------------------------------------------------
rf <- forestry(x = x, 
               y = y,
               nodesizeStrictSpl = 5,
               ntree = 1)

rf_out <- predict(rf, x)
rf_df <- data.frame(x, rf_out)
#ggplot(data = rf_df, aes(x = x, y = rf_out)) + geom_point(shape = 1) + theme_classic()


# Linear CART tree -------------------------------------------------------------
lrf <- forestry(x = x, 
                y = y,
                minSplitGain = .1,
                nodesizeStrictSpl = 5,
                ntree = 1,
                linear = TRUE,
                overfitPenalty = .01)

lrf_out <- predict(lrf, x, aggregation = "coefs")
lrf_df <- data.frame(x, lrf_out$predictions)
#ggplot(data = lrf_df, aes(x = x, y = lrf_out.predictions)) + geom_point(shape = 1) + theme_classic()


# Local Linear Forest tree -----------------------------------------------------
llf <- ll_regression_forest(X = as.matrix(x), 
                            Y = y, 
                            num.trees = 1,
                            enable.ll.split = TRUE)

llf_out <- predict(llf, as.data.frame(x))$predictions
llf_df <- data.frame(x, llf_out)
#ggplot(data = llf_df, aes(x = x, y = llf_out)) + geom_point(shape = 1, color = "red") + geom_line() + theme_classic()

# Retrieve Split points and coefficients ---------------------------------------
plot <- data.frame(
  x = x, 
  Truth = noise_less_y, 
  CART = rf_out,
  LocalLinearForest = llf_out, 
  RidgeRF = lrf_out$predictions
) %>% melt(id = "x") %>%
  dplyr::rename(Estimator = variable, Truth = value) %>%
  mutate(Estimator = as.character(Estimator)) %>%
  ggplot(aes(x = x, y = Truth, color = Estimator, linetype = Estimator, 
             size = Estimator)) +
  geom_line() +
  scale_linetype_manual(values = c("Truth" = "dotted", 
                                   "LocalLinearForest" = "solid", 
                                   "CART" = "solid",
                                    "RidgeRF" = "solid")) +
  scale_size_manual(values = c("Truth" = .5, 
                               "LocalLinearForest" = .7, 
                               "CART" = .7,
                               "RidgeRF" = .7)) + 
  scale_color_manual(values = c("Truth" = "black", 
                                "LocalLinearForest" = "green", 
                                "CART" = "red",
                                "RidgeRF" = "blue")) +  
  geom_point(aes(x = x, y = y), data = data.frame(x = x, y = y), 
             inherit.aes = FALSE, size = 0.5, alpha = .8)
plot + theme_classic() +
  scale_x_continuous(breaks = seq(0, 2, length.out = 5) + 1) +
  coord_cartesian(ylim = c(-1, 3), xlim = c(0, 2.1)) + ggtitle(label = "CART and Linear Aggregation Comparison")

# We can compare the complexity of the trees -----------------------------------
plot(rf, tree.id = 1)
plot(lrf, tree.id = 1)


# A second example with added smoothness + differential in local noise levels due to axial noise
n <- 300
x <- runif(n,0,6)
noise_less_y <- ifelse(x > 2, 
                       -sin(.8*(x-1)) + sin(1), 
                       ifelse(x > 1, 
                              -2*x+ 4, 
                              2*x ))
plot(x, noise_less_y)
y <- noise_less_y + rnorm(n, sd = .15) 
plot(x, y)
df <- data.frame(x,y)

# Standard CART tree -----------------------------------------------------------
rf <- forestry(x = x, 
               y = y,
               nodesizeStrictSpl = 5,
               ntree = 1)

rf_out <- predict(rf, x)
rf_df <- data.frame(x, rf_out)

# Linear CART tree -------------------------------------------------------------
lrf <- forestry(x = x, 
                y = y,
                minSplitGain = .1,
                nodesizeStrictSpl = 10,
                ntree = 1,
                linear = TRUE,
                overfitPenalty = .01)

lrf_out <- predict(lrf, x, aggregation = "coefs")
lrf_df <- data.frame(x, lrf_out$predictions)

# Local Linear Forest tree -----------------------------------------------------
llf <- ll_regression_forest(X = as.matrix(x), 
                            Y = y, 
                            num.trees = 1,
                            enable.ll.split = TRUE)

llf_out <- predict(llf, as.data.frame(x))$predictions
llf_df <- data.frame(x, llf_out)

# Retrieve Split points and coefficients ---------------------------------------
plot <- data.frame(
  x = x, 
  Truth = noise_less_y, 
  CART = rf_out,
  LocalLinearForest = llf_out, 
  RidgeRF = lrf_out$predictions
) %>% melt(id = "x") %>%
  dplyr::rename(Estimator = variable, Truth = value) %>%
  mutate(Estimator = as.character(Estimator)) %>%
  ggplot(aes(x = x, y = Truth, color = Estimator, linetype = Estimator, 
             size = Estimator)) +
  geom_line() +
  scale_linetype_manual(values = c("Truth" = "dotted", 
                                   "LocalLinearForest" = "solid", 
                                   "CART" = "solid",
                                   "RidgeRF" = "solid")) +
  scale_size_manual(values = c("Truth" = .5, 
                               "LocalLinearForest" = .7, 
                               "CART" = .7,
                               "RidgeRF" = .7)) + 
  scale_color_manual(values = c("Truth" = "black", 
                                "LocalLinearForest" = "green", 
                                "CART" = "red",
                                "RidgeRF" = "blue")) +  
  geom_point(aes(x = x, y = y), data = data.frame(x = x, y = y), 
             inherit.aes = FALSE, size = 0.5, alpha = .8)
plot + theme_classic() +
  scale_x_continuous(breaks = seq(0, 6, length.out = 5) + 1) +
  coord_cartesian(ylim = c(-1, 3), xlim = c(0, 6.1)) + ggtitle(label = "CART and Linear Aggregation Comparison #2")

# We can compare the complexity of the trees -----------------------------------
plot(rf, tree.id = 1)
plot(lrf, tree.id = 1)


# Simple V example ----------------------------------------------------------------
set.seed(4335)
n <- 500
p <- 10
X <- matrix(rnorm(n*p), n, p)
Y <- ifelse(X[,1] > 0, 3 * X[,1],  - 3 * X[,1])


# GRF Local Linear Forest ------------------------------------------------------
grf_linear_forest <- ll_regression_forest(
  X, 
  Y,
  mtry = 10,
  num.trees = 1,
  enable.ll.split = TRUE,
  ci.group.size = 1,
  min.node.size = 100)

pred_grf <- predict(grf_linear_forest, X)$predictions

# Forestry Linear Random Forest ---------------------------------------------------------
forestry_linear_rf <- forestry(
  X,
  Y, 
  mtry = 10,
  ntree = 1,
  nodesizeStrictSpl = 100,
  linear = TRUE
)

pred_forestry <- predict(forestry_linear_rf, X)

# Plot data --------------------------------------------------------------------
data.frame(
  x = X[,1], 
  Signal = Y, 
  GRF = pred_grf,
  Linear_forestry = pred_forestry
) %>% melt(id = "x") %>%
  ggplot(aes(x = x, y = value, color = variable)) + 
  geom_line() +
  scale_color_manual(values = c("Signal" = "blue", 
                                "Linear_forestry" = "green", 
                                "GRF" = "red")) +
  theme_bw()

ggsave("~/Downloads/lrf_grf_comparison.pdf", width = 8, height = 8)



