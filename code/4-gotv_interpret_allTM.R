library(visNetwork)
library(tidyverse)
library(Rforestry)

# Load + clean the GerberGreenLarimer data -------------------------------------
social <- read.csv(file = "data/GerberGreenLarimer_APSR.csv")
social <- tbl_df(social)[,-1] %>%
  dplyr::mutate(
    WC = ifelse(treatment == " Civic Duty", 1, 0),
    WH = ifelse(treatment == " Hawthorne", 1, 0),
    WS = ifelse(treatment == " Self", 1, 0),
    WN = ifelse(treatment == " Neighbors", 1, 0),
    age = 2008-yob,
    y = ifelse(voted == "Yes", 1, 0)
  ) %>%
  dplyr::select(y, WC, WH, WS, WN, g2000, g2002, p2000, p2002, p2004, age)

social$p2004 <- gsub("N","n", gsub("Y","y",social$p2004))
social$CVH <- apply(social[, c("g2000", "g2002", "p2000", "p2002", "p2004")] == "yes", 1, sum)
social$CGVH <- apply(social[, c("g2000", "g2002")] == "yes", 1, sum)
social$CPVH <- apply(social[, c("p2000", "p2002", "p2004")] == "yes", 1, sum)

social <- social %>%
  mutate_if(sapply(social, is.character), as.factor)

social <- social %>%
  dplyr::rename("Voted 2000 \n Primary" = p2000,
         "Voted 2002 \n Primary" = p2002,
         "Voted 2004 \n Primary" = p2004,
         "Voted 2000 \n General" = g2000,
         "Voted 2002 \n General" = g2002,
         "Civic Duty" = WC,
         "Hawthorne " = WH,
         "Household " = WS,
         "Neighbor  " = WN,
         "Total Elections" = CVH,
         "Total General \n Elections" = CGVH,
         "Total Primary \n  Elections" = CPVH,
         "\n        Age        \n" = age)

summary(social)

# Train the Slearner and we look at W: -----------------------------------------
rft <- forestry(x = as.data.frame(social %>% dplyr::select(-y)),
                y = social$y,
                mtry = ncol(social) - 1,
                sample.fraction = 1,
                nodesizeStrictSpl = 10000,
                ntree = 4,
                minSplitGain = .005,
                linear = TRUE,
                overfitPenalty = 1e-8,
                linFeats = 1:4,
                middleSplit = TRUE,
                verbose = FALSE,
                splitratio = .5)

# Source the custom plotting function for the GOTV data set --------------------
source(file = "code/4-gotv_S_plot_fkt.R")


# Plot the first four trees ----------------------------------------------------
plotit(x = rft, tree.id = 1)
# ggsave(filename = "figures/trees_new/tree1.pdf")
plotit(x = rft, tree.id = 2)
# ggsave(filename = "figures/trees_new/tree2.pdf")
plotit(x = rft, tree.id = 3)
# ggsave(filename = "figures/trees_new/tree3.pdf")
plotit(x = rft, tree.id = 4)
# ggsave(filename = "figures/trees_new/tree4.pdf")


