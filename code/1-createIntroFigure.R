# devtools::install_github("soerenkuenzel/forestry")
set.seed(534325421)
library(tidyverse)
library(reshape)
library(Rforestry)
library(gridExtra)
library(grid)
library(ggplot2)
library(lattice)

reg_f <- function(x) {
  return(ifelse(x < pi,
                sin(x),
                -x + pi) + 1.5)
}

# Geberate data ----------------------------------------------------------------
n <- 500
d <- 1
X <- matrix(runif(n * d, min = -7, 7), n, d)
X <- X[X[, 1] < 3.3 | X[, 1] > 5,, drop = FALSE]
y <- reg_f(X[, 1]) +  .3 * rnorm(nrow(X))
plot(X[, 1], y)

n_to_predict <- 5000
X_to_predict <- matrix(runif(n = n_to_predict * d, min = -7, 10), n_to_predict, d)
mu_truth <-  reg_f(X_to_predict[, 1])
plot(X_to_predict[, 1], mu_truth)

# vanilla forest ---------------------------------------------------------------
forest_vanilla <- forestry(
  X,
  y,
  mtry = 1,
  ntree = 1,
  nodesizeSpl = 50)

pred_vanilla <- predict(forest_vanilla, newdata = X_to_predict)


# forestry ---------------------------------------------------------------------
forest_ridge <- forestry(
  X,
  y,
  mtry = 1,
  ntree = 1,
  nodesizeStrictSpl = 20,
  linear = TRUE,
  sample.fraction = 1,
  replace = FALSE,
  overfitPenalty = .01
)

pred_ridge <- predict(forest_ridge, newdata = X_to_predict)

# Plot data --------------------------------------------------------------------
# v1
p_fitted <- data.frame(
  x = X_to_predict[,1],
  truth = mu_truth,
  RF_vanilla = pred_vanilla,
  RF_ridge = pred_ridge
) %>% melt(id = "x") %>%
  dplyr::rename(predictor = variable, y = value) %>%
  mutate(predictor = as.character(predictor)) %>%
  mutate(predictor = ifelse(predictor == "RF_vanilla", "classical CART",
                     ifelse(predictor == "RF_ridge", "linear CART",predictor))) %>%
  ggplot(aes(x = x, y = y, color = predictor, linetype = predictor,
             size = predictor)) +
  geom_line() +
  scale_linetype_manual(values = c("truth" = "dotted",
                                   "linear CART" = "solid",
                                   "classical CART" = "solid")) +
  scale_size_manual(values = c("truth" = .5,
                                "linear CART" = .7,
                                "classical CART" = .7)) +
  scale_color_manual(values = c("truth" = "black",
                                "linear CART" = "green",
                                "classical CART" = "red")) +
  geom_point(aes(x = X, y = Y), data = data.frame(X = X[,1], Y = y),
             inherit.aes = FALSE, size = 0.3, alpha = .5) +
  theme_bw()
p_fitted
# ggsave(filename = "001_IntroFigure_v1.pdf", plot = p_fitted, width = 6.4,
#        height = 4)


# v2

p_hist <- data.frame(X = X[,1], Y = y) %>%
  ggplot(aes(x = X)) +
  geom_histogram(bins = 100) +
  theme_bw()



p_fitted +
  theme(
    axis.title.x = element_blank(),
    axis.text.x = element_blank(),
    axis.ticks.x = element_blank()
  ) +
  theme(legend.justification = c(0, 0),
        legend.position = c(.01, .01))



p_fitted + scale_x_continuous(breaks = seq(-6, 10, length.out = 5) + 2)

p_fittedGP <- ggplotGrob(
  p_fitted +
    theme(
      axis.title.x = element_blank(),
      axis.text.x = element_blank(),
      axis.ticks.x = element_blank()
    ) +
    theme(
      legend.justification = c(0, 0),
      legend.position = c(.01, .01),
      legend.title = element_blank()
    ) +
    scale_x_continuous(breaks = seq(-6, 10, length.out = 5) + 2) +
    coord_cartesian(ylim = c(-3, 3), xlim = c(-6, 9))
)
p_histGP <- ggplotGrob(
  p_hist +
    scale_x_continuous(breaks = seq(-6, 10, length.out = 5) + 2) +
    coord_cartesian(xlim = c(-6, 9)) +
    xlab("x") +
    scale_y_continuous(breaks = c(0, 10)))

maxWidth = grid::unit.pmax(p_fittedGP$widths[2:5],
                           p_histGP$widths[2:5])
p_fittedGP$widths[2:5] <- as.list(maxWidth)
p_histGP$widths[2:5] <- as.list(maxWidth)

plot <- arrangeGrob(p_fittedGP, p_histGP,
             ncol = 1, heights = c(3, 1.4))


ggsave(filename = "figures/001_IntroFigure_v2.pdf", plot = plot, width = 6.4, height = 4)

dev.off()
