if (dir.exists("~/Dropbox/ridgeEvaluation/")) {
  setwd("~/Dropbox/ridgeEvaluation/")
} else if (dir.exists("~/ridgeEvaluationCode/")) {
  setwd("~/ridgeEvaluationCode/")
} else if (dir.exists("~/ridgeEvaluation/")) {
  setwd("~/ridgeEvaluation/")
} else if (dir.exists("/accounts/projects/sekhon/theo_s/gdrive/ridgeEvaluation")) {
  setwd("/accounts/projects/sekhon/theo_s/gdrive/ridgeEvaluation")
} else {
  stop("wd was not set correctly")
}
library(tidyverse)
library(xtable)
library(reshape)
library(MASS)
library(ggplot2)
library(dplyr)
library(caret)
library(pdp)
library(iml)
library(dbarts)
library(TTR)
library(reshape)
library(viridis)

X <- read.csv("replicationCode/9-run_all_cluster_resultsEMSE.csv", stringsAsFactors = FALSE)
# X <- X[22:36,-1]
X$n <- as.numeric(gsub(pattern = "([^0123456789])", replacement = "", X$Dataset))
X$Dataset <- gsub(pattern = "([-0123456789])", replacement = "", X$Dataset)
X <- melt(data = X, id.vars = c("Dataset", "n"))
X$dsname <- NA
X$dsname[X$Dataset == "artificialLM"] <- "Experiment_1"
X$dsname[X$Dataset == "simulatedStepFunction"] <- "Experiment_2"
X$dsname[X$Dataset == "simulatedStepLinearFunction"] <- "Experiment_3"
X$variable <- plyr::revalue(X$variable, c("caretRidgeRF_nonstrict" = "LRF (forestry)",
                                          "BART" = "BART (dbarts)",
                                          "forestryRF" = "RF (forestry)",
                                          "ranger" = "RF (ranger)",
                                          "local_RF" = "LLF (grf)",
                                          "cubist" = "Cubist (Cubist)",
                                          "glmnet" = "RLM (glmnet)"))


X %>% filter(!is.na(value)) %>%
  # filter(variable %in% c("ranger", "glmnet", "local_RF")) %>%
  ggplot(aes(x = n, y = value, color = variable))  +
  geom_line() +
  facet_wrap(.~Dataset, scales = "free_y") +
  #geom_text(aes(label = variable)) +
  theme_bw() + 
  ylim(0, 2.5) +
  theme(legend.position = "none")

library(ggrepel)

# Plot experiment 1
for (expnm in c("Experiment_1")) {
  X %>% 
    filter(!is.na(value) & dsname == expnm & variable != "cubist" & variable != "caretRidgeTree") %>% 
    dplyr::select(c(4)) %>% 
    max() -> max
  
  X %>% 
    filter(!is.na(value) & dsname == expnm & variable != "cubist" & variable != "caretRidgeTree") %>% 
    dplyr::select(-Dataset, -dsname) %>% 
    filter(n == 2048) -> end_values
  
  X %>% filter(!is.na(value) & dsname == expnm & variable != "cubist" & variable != "caretRidgeTree") %>%
    ggplot(aes(x = n, y = value, color = variable))  +
    geom_line() +
    #geom_text(aes(label = variable)) +
    theme_bw() + 
    xlim(0, 2500) +
    ylim(-.2, max + .1)+
    theme(legend.position = "none") +
    geom_text_repel(
      aes(label = variable),
      data = end_values, color = c("#440154FF", "#443A83FF",
                                   "#31688EFF", "#21908CFF",
                                   "#35B779FF", "#8FD744FF", "#FDE725FF"),
      size = 3, force = 6,arrow = arrow(length = unit(0.01, "npc")),
      direction = "both", nudge_x = 80, nudge_y = .1, point.padding = .9
    )+
    # ggtitle(label = expnm) +
    ggtitle(label = "") +
    xlab("Sample Size") +
    ylab("EMSE") + 
    geom_point() + 
    scale_color_viridis_d()
  
  ggsave(file = paste0("figures/varyn_", expnm, ".pdf"), height = 3.3, width = 6)
  #ggsave(file = paste0("~/Dropbox/RidgeForestry_paper/figures/2-VaryNSim/varyn_", 
  #                     expnm, ".pdf"), height = 3.3, width = 6)
  
}


# Plot experiment 2
for (expnm in c("Experiment_2")) {
  X %>% 
    filter(!is.na(value) & dsname == expnm & variable != "cubist" & variable != "caretRidgeTree") %>% 
    dplyr::select(c(4)) %>% 
    max() -> max
  
  X %>% 
    filter(!is.na(value) & dsname == expnm & variable != "cubist" & variable != "caretRidgeTree") %>% 
    dplyr::select(-Dataset, -dsname) %>% 
    filter(n == 2048) -> end_values
  
  X %>% filter(!is.na(value) & dsname == expnm & variable != "cubist" & variable != "caretRidgeTree") %>%
    ggplot(aes(x = n, y = value, color = variable))  +
    geom_line() +
    #geom_text(aes(label = variable)) +
    theme_bw() + 
    xlim(0, 2500) +
    ylim(-.1, max + .1)+
    theme(legend.position = "none") +
    geom_text_repel(
      aes(label = variable),
      data = end_values, color = c("#440154FF", "#443A83FF",
                                   "#31688EFF", "#21908CFF",
                                   "#35B779FF", "#8FD744FF", "#FDE725FF"),
      size = 3, force = 3,arrow = arrow(length = unit(0.01, "npc")),
      direction = "both", nudge_x = 25, nudge_y = 0, point.padding = .8
    )+
    # ggtitle(label = expnm) +
    ggtitle(label = "") +
    xlab("Sample Size") +
    ylab("EMSE") + 
    geom_point() + 
    scale_color_viridis_d()
  
  ggsave(file = paste0("figures/varyn_", expnm, ".pdf"), height = 3.3, width = 6)
  #ggsave(file = paste0("~/Dropbox/RidgeForestry_paper/figures/2-VaryNSim/varyn_", 
  #                     expnm, ".pdf"), height = 3.3, width = 6)
  
}


# Plot experiment 3
for (expnm in c("Experiment_3")) {
  X %>% 
    filter(!is.na(value) & dsname == expnm & variable != "cubist" & variable != "caretRidgeTree") %>% 
    dplyr::select(c(4)) %>% 
    max() -> max
  
  X %>% 
    filter(!is.na(value) & dsname == expnm & variable != "cubist" & variable != "caretRidgeTree") %>% 
    dplyr::select(-Dataset, -dsname) %>% 
    filter(n == 2048) -> end_values
  
  X %>% filter(!is.na(value) & dsname == expnm & variable != "cubist" & variable != "caretRidgeTree") %>%
    ggplot(aes(x = n, y = value, color = variable))  +
    geom_line() +
    #geom_text(aes(label = variable)) +
    theme_bw() + 
    xlim(0, 2500) +
    ylim(-.1, max + .1)+
    theme(legend.position = "none") +
    geom_text_repel(
      aes(label = variable),
      data = end_values, color = c("#440154FF", "#443A83FF",
                                   "#31688EFF", "#21908CFF",
                                   "#35B779FF", "#8FD744FF", "#FDE725FF"),
      size = 3, force = 3,arrow = arrow(length = unit(0.01, "npc")),
      direction = "both", nudge_x = 25, nudge_y = 0, point.padding = .92
    )+
    # ggtitle(label = expnm) +
    ggtitle(label = "") +
    xlab("Sample Size") +
    ylab("EMSE") + 
    geom_point() + 
    scale_color_viridis_d()
  
  ggsave(file = paste0("figures/varyn_", expnm, ".pdf"), height = 3.3, width = 6)
  #ggsave(file = paste0("~/Dropbox/RidgeForestry_paper/figures/2-VaryNSim/varyn_", 
  #                     expnm, ".pdf"), height = 3.3, width = 6)
  
}
