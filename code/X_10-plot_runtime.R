library(ggplot2)
library(reshape2)
library(dplyr)
library(viridis)

runtimes <- read.csv(file = "code/X_10-runtimeRuntime.csv")


p_range <- c(5,10,20,40,80,160)#,160)
colors <- viridis(3)

data.frame(P = p_range,
           LRF = runtimes$ridgeRF[1:length(p_range)],
           Naive = runtimes$RridgeRF[1:length(p_range)]) %>%
  melt(id.vars = c("P")) -> x

x$variable <- plyr::revalue(x$variable, c("Naive" = "Naive Algorithm",
                                        "LRF" = "LRF Algorithm"))

x %>%
  rename(c("Algorithm"="variable")) %>%
  ggplot(aes(x = P, y = value, color = Algorithm))+
  geom_line()+
  theme_bw() +
  xlab("Dimension") +
  ylab("Runtime (seconds)")+
  scale_color_manual(values=colors[1:2])

ggsave(filename = "figures/runtimeP.pdf")


n_range <- c(100,200,400,800,1600,3200)#,160)

data.frame(N = n_range,
           LRF = runtimes$ridgeRF[c(2,7:11)],
           Naive = runtimes$RridgeRF[c(2,7:11)]) %>%
  melt(id.vars = c("N")) -> x

x$variable <- plyr::revalue(x$variable, c("Naive" = "Naive Algorithm",
                                          "LRF" = "LRF Algorithm"))

x %>%
  rename(c("Algorithm"="variable")) %>%
  ggplot(aes(x = N, y = value, color = Algorithm))+
  geom_line()+
  theme_bw() +
  xlab("Sample Size") +
  ylab("Runtime (seconds)")+
  scale_color_manual(values=colors[1:2])

ggsave(filename = "figures/runtimeN.pdf")
