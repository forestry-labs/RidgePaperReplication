library(dplyr)
library(ggplot2)

data <- read.csv(file = "results/vi_results.csv")

lrf.vi <- data$lrf.vi / mean(data$lrf.vi)
rf.vi <- data$rf.vi / mean(data$rf.vi)


# Make plots ===================================================================
library(ggplot2)
data.frame(Var = factor(paste0("X",1:10), levels = unique(paste0("X",1:10))), Importance = lrf.vi) %>%
  ggplot(aes(x=Var,y=Importance))+
  geom_col()+
  theme_classic()+
  labs(y = "Normalized Variable Importance", x = "Variable")
ggsave(filename = "figures/VI_lrf.pdf")

data.frame(Var = factor(paste0("X",1:10), levels = unique(paste0("X",1:10))), Importance = rf.vi) %>%
  ggplot(aes(x=Var,y=Importance))+
  geom_col()+
  theme_classic()+
  labs(y = "Normalized Variable Importance", x = "Variable")
ggsave(filename = "figures/VI_rf.pdf")

