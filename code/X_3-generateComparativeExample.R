# A Simple example illustrating the flexibility of Linear Random Forests
set.seed(4335)
library(Rforestry)
library(grf)
library(ggplot2)
library(dplyr)
library(reshape)

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
  num.trees = 11,
  enable.ll.split = TRUE,
  ci.group.size = 1,
  min.node.size = 100,
  seed = 4335)

# plot(tree <- get_tree(grf_linear_forest, 1))

pred_grf <- predict(grf_linear_forest, newdata = X)$predictions

# GRF predictions seem to be broken when using a single tree

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
)  %>% melt(id = "x") %>% rename(c(x = "x", variable = "Estimator", value = "y")) %>%
  ggplot(aes(x = x, y = y, color = Estimator)) +
  geom_line() +
  scale_color_manual(values = c("Signal" = "blue",
                                "Linear_forestry" = "green",
                                "GRF" = "red")) +
  theme_bw()

ggsave("figures/lrf_grf_comparison.pdf", width = 9, height = 5)



