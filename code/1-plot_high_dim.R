library(Rforestry)
library(MASS)
library(dplyr)
library(ggplot2)


# Read in coefs ================================================================
all_data <- data.frame()

for (file in dir("code/new_results")) {
  coefs <- readRDS(file=paste0("code/new_results/",file))
  if (length(all_data) == 0) {
    all_data <- coefs
  } else {
    all_data <- rbind(all_data,
                      coefs)
  }
  print(head(coefs))
}

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

