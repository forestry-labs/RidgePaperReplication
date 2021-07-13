library(tidyverse)
library(reshape)
library(MASS)
library(ggplot2)
library(dplyr)
library(reshape)
library(viridis)

X <- read.csv("code/3-run_all_cluster_resultsEMSE.csv", stringsAsFactors = FALSE)
X <- X[1:15,-1]
X$n <- as.numeric(gsub(pattern = "([^0123456789])", replacement = "", X$Dataset))
X$Dataset <- gsub(pattern = "([-0123456789])", replacement = "", X$Dataset)
X <- melt(data = X, id.vars = c("Dataset", "n"))
X$dsname <- NA
X$dsname[X$Dataset == "artificialLM"] <- "Experiment_1"
X$dsname[X$Dataset == "simulatedStepFunction"] <- "Experiment_2"
X$dsname[X$Dataset == "simulatedStepLinearFunction"] <- "Experiment_3"
X$variable <- plyr::revalue(X$variable, c("caretRidgeRF_nonstrict" = "LRF (forestry)",
                                          "pre" = "RuleFit (pre)",
                                          "gbm" = "GBM (gbm)"))


# X %>% filter(!is.na(value)) %>%
#   # filter(variable %in% c("ranger", "glmnet", "local_RF")) %>%
#   ggplot(aes(x = n, y = value, color = variable))  +
#   geom_line() +
#   facet_wrap(.~Dataset, scales = "free_y") +
#   #geom_text(aes(label = variable)) +
#   theme_bw() +
#   ylim(0, 2.5) +
#   theme(legend.position = "none")

library(ggrepel)

colors <-c( "#440154FF", "#1E9B8AFF", "#E0E318FF")
mapping <- c("LRF (forestry)" ="#440154FF",
             "RuleFit (pre)" = "#E0E318FF",
             "GBM (gbm)" = "#1E9B8AFF")

# Plot experiment 1
for (expnm in c("Experiment_1")) {
  X %>%
    filter(!is.na(value) & dsname == expnm & variable %in% c("LRF (forestry)","RuleFit (pre)","GBM (gbm)")) %>%
    dplyr::select(c(4)) %>%
    max() -> max

  X %>%
    filter(!is.na(value) & dsname == expnm & variable %in% c("LRF (forestry)","RuleFit (pre)","GBM (gbm)")) %>%
    dplyr::select(-Dataset, -dsname) %>%
    filter(n == 2048) -> end_values

  X %>% filter(!is.na(value) & dsname == expnm & variable %in% c("LRF (forestry)","RuleFit (pre)","GBM (gbm)")) %>%
    ggplot(aes(x = n, y = value, color = variable))  +
    geom_line() +
    #geom_text(aes(label = variable)) +
    theme_bw() +
    xlim(0, 2500) +
    ylim(-.2, max + .1)+
    scale_color_manual(values = mapping)+
    theme(legend.position = "none") +
    geom_text_repel(
      aes(label = variable),
      data = end_values, color = colors,
      size = 3, force = 6,arrow = arrow(length = unit(0.01, "npc")),
      direction = "both", nudge_x = 300, nudge_y = .05, point.padding = .6
    )+
    # ggtitle(label = expnm) +
    ggtitle(label = "") +
    xlab("Sample Size") +
    ylab("EMSE") +
    geom_point()

  ggsave(file = paste0("figures/add_varyn_", expnm, ".pdf"), height = 3.3, width = 6)
  #ggsave(file = paste0("~/Dropbox/RidgeForestry_paper/figures/2-VaryNSim/varyn_",
  #                     expnm, ".pdf"), height = 3.3, width = 6)

}


# Plot experiment 2
for (expnm in c("Experiment_2")) {
  X %>%
    filter(!is.na(value) & dsname == expnm & variable %in% c("LRF (forestry)","RuleFit (pre)","GBM (gbm)")) %>%
    dplyr::select(c(4)) %>%
    max() -> max

  X %>%
    filter(!is.na(value) & dsname == expnm & variable %in% c("LRF (forestry)","RuleFit (pre)","GBM (gbm)")) %>%
    dplyr::select(-Dataset, -dsname) %>%
    filter(n == 2048) -> end_values

  X %>% filter(!is.na(value) & dsname == expnm & variable %in% c("LRF (forestry)","RuleFit (pre)","GBM (gbm)")) %>%
    ggplot(aes(x = n, y = value, color = variable))  +
    geom_line() +
    scale_color_manual(values = mapping)+
    #geom_text(aes(label = variable)) +
    theme_bw() +
    xlim(0, 2500) +
    ylim(-.1, max + .1)+
    theme(legend.position = "none") +
    geom_text_repel(
      aes(label = variable),
      data = end_values, color = colors,
      size = 3, force = 5,arrow = arrow(length = unit(0.01, "npc")),
      direction = "both", nudge_x = 470, nudge_y = 0.03, point.padding = 1.2
    )+
    # ggtitle(label = expnm) +
    ggtitle(label = "") +
    xlab("Sample Size") +
    ylab("EMSE") +
    geom_point()

  ggsave(file = paste0("figures/add_varyn_", expnm, ".pdf"), height = 3.3, width = 6)
  #ggsave(file = paste0("~/Dropbox/RidgeForestry_paper/figures/2-VaryNSim/varyn_",
  #                     expnm, ".pdf"), height = 3.3, width = 6)

}


# Plot experiment 3
for (expnm in c("Experiment_3")) {
  X %>%
    filter(!is.na(value) & dsname == expnm & variable %in% c("LRF (forestry)","RuleFit (pre)","GBM (gbm)")) %>%
    dplyr::select(c(4)) %>%
    max() -> max

  X %>%
    filter(!is.na(value) & dsname == expnm & variable %in% c("LRF (forestry)","RuleFit (pre)","GBM (gbm)")) %>%
    dplyr::select(-Dataset, -dsname) %>%
    filter(n == 2048) -> end_values

  X %>% filter(!is.na(value) & dsname == expnm & variable %in% c("LRF (forestry)","RuleFit (pre)","GBM (gbm)")) %>%
    ggplot(aes(x = n, y = value, color = variable))  +
    geom_line() +
    scale_color_manual(values = mapping)+
    #geom_text(aes(label = variable)) +
    theme_bw() +
    xlim(0, 2500) +
    ylim(-.1, max + .1)+
    theme(legend.position = "none") +
    geom_text_repel(
      aes(label = variable),
      data = end_values, color = colors,
      size = 3, force = 3,arrow = arrow(length = unit(0.01, "npc")),
      direction = "both", nudge_x = 350, nudge_y = 0, point.padding = .92
    )+
    # ggtitle(label = expnm) +
    ggtitle(label = "") +
    xlab("Sample Size") +
    ylab("EMSE") +
    geom_point()

  ggsave(file = paste0("figures/add_varyn_", expnm, ".pdf"), height = 3.3, width = 6)
  #ggsave(file = paste0("~/Dropbox/RidgeForestry_paper/figures/2-VaryNSim/varyn_",
  #                     expnm, ".pdf"), height = 3.3, width = 6)

}
