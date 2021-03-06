library(Rforestry)
library(MASS)
library(dplyr)

if (!interactive()){
  library(argparse)
  pars <- ArgumentParser()

  pars$add_argument("--seed", type = "double", default = 1, help = "random seed")
  args <- pars$parse_args()

  seed <- args$seed
} else {
  seed <- 101
}

source("code/3.4-generateDataHighDimensional.R")

# datasets_grid

# true_derivative <- function(x){
#   return((24*exp(12*x + 6))/((exp(12*x) + exp(6))^2))
# }

ds <- datasets_grid[["HighDimensionalLarge"]]

es_trnd <- forestry(x = ds$train[,] %>% dplyr::select(-y),
                    y = ds$train[,] %>% dplyr::select(y) %>% .[,1],
                    linear = TRUE,
                    nodesizeStrictSpl = 20,
                    overfitPenalty = 150)

# save(es_trnd, file = "large_ridge.Rda")

preds <- predict(es_trnd,
                 newdata = ds$test %>% dplyr::select(-y),
                 aggregation = "coefs")

mean_coefs <- apply(preds$coef, 2, mean)

filename <- paste0("code/hresults/high_dim_coefs_",seed,".RDS")
saveRDS(mean_coefs, file = filename)

# val <- data.frame(Variable = 1:4,
#                   variable = c("X1", "X2", "X3", "X4"),
#                   value = mean_coefs[1:4])

# ggplot(data = data.frame(Variable = 1:100, value = mean_coefs[-101]),
#        aes(x = Variable, y = value))+
#   geom_point()+
#   theme_bw()+
#   geom_text_repel(
#     aes(label = variable),
#     data = val,
#     size = 3, force = 3,arrow = arrow(length = unit(0.01, "npc")),
#     direction = "both", nudge_x = 20, nudge_y = -0.001, point.padding = .8
#   )+
#   labs(x = "Variable", y = "Mean Ridge Coefficient")
# ggsave(filename = "figures/high_dim_coefficients.pdf", height = 4, width = 5)

