library(MASS)
library(dplyr)
library(ggplot2)
library(ggrepel)

all_data <- data.frame()#matrix(rep(0,100), ncol = 100, nrow=1)

for (file in dir("code/hresults")) {
  print(file)
  # if (nrow(all_data) == 100){
  #   next
  # }
  coefs <- readRDS(file = paste0("code/hresults/",file))
  all_data <- rbind(all_data, unname(coefs))
}
# remove any duplicates
all_data <- all_data[-which(duplicated(all_data[,1])),]

# rename columns
colnames(all_data) <- c(paste0("X",1:100),"Intercept")


mean_coefs <- apply(all_data, MARGIN = 2,mean)

sd_coefs <- apply(all_data, MARGIN = 2,sd)
mean_sd <- mean(sd_coefs[-101])


val <- data.frame(Variable = 1:4,
                  variable = c("X1", "X2", "X3", "X4"),
                  value = mean_coefs[1:4])

ggplot(data = data.frame(Variable = 1:100, value = mean_coefs[-101]),
       aes(x = Variable, y = value))+
  geom_point()+
  theme_bw()+
  geom_text_repel(
    aes(label = variable),
    data = val,
    size = 3.5, force = 1,arrow = arrow(length = unit(0.01, "npc")),
    direction = "both", nudge_x = 7, nudge_y =-.0019, point.padding = .8
  )+
  geom_hline(yintercept=1.96*mean_sd, linetype="dashed", color = "red")+
  ylim(c(0,.012))+
  labs(x = "Variable", y = "Mean Ridge Coefficient")
ggsave(filename = "figures/high_dim_coefficients.pdf", height = 4, width = 7)
