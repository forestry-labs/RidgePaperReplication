# devtools::install_github("soerenkuenzel/causalToolbox")
# devtools::install_github("soerenkuenzel/forestry", ref = "splitting-features")
# devtools::load_all("~/Dropbox/Rforestry/")
# library(forestry)
library(visNetwork)
library(tidyverse)

# load("GerberGreenLarimer_r1b.RData", verbose = TRUE)
social <- read.csv(file = "data/GerberGreenLarimer_APSR.csv")
social <- tbl_df(social)[,-1] %>%
  dplyr::mutate(
    WC = ifelse(treatment == " Civic Duty", 1, 0),
    WH = ifelse(treatment == " Hawthorne", 1, 0),
    WS = ifelse(treatment == " Self", 1, 0),
    WN = ifelse(treatment == " Neighbors", 1, 0),
    # g2000 = ifelse(g2000 == "yes", 1, 0),
    # g2002 = ifelse(g2002 == "yes", 1, 0),
    # p2000 = ifelse(p2000 == "yes", 1, 0),
    # p2002 = ifelse(p2002 == "yes", 1, 0),
    # p2004 = ifelse(p2004 == "yes", 1, 0),
    age = 2008-yob,
    y = ifelse(voted == "Yes", 1, 0)
  ) %>%
  select(y, WC, WH, WS, WN, g2000, g2002, p2000, p2002, p2004, age)

social$p2004 <- gsub("N","n", gsub("Y","y",social$p2004))
social$CVH <- apply(social[, c("g2000", "g2002", "p2000", "p2002", "p2004")] == "yes", 1, sum)
social$CGVH <- apply(social[, c("g2000", "g2002")] == "yes", 1, sum)
social$CPVH <- apply(social[, c("p2000", "p2002", "p2004")] == "yes", 1, sum)

social <- social %>%
  mutate_if(sapply(social, is.character), as.factor)


social <- social %>%
  rename("Voted 2000 \n Primary" = p2000,
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
         "Total Primary \n  Elections" = CPVH)

summary(social)

# define custom plot fnkt ------------------------------------------------------
# custom_plot <- function(pdta) {
#   node_info <- pdta[[1]]
#   nodes <- pdta[[2]]
#   edges <- pdta[[3]]
#   coefs <- pdta[[4]]
#   tree.id <- pdta[[5]]
#
#   # min_TE <- min(do.call("rbind", coefs)[,2]) * 100
#   # max_TE <- max(do.call("rbind", coefs)[,2]) * 100
#   min_TE <- 3
#   max_TE <- 11.5
#   color_code <- data.frame(value = seq(min_TE, max_TE, length.out = 100))
#   color_code$color <- as.character(colorRampPalette(c("red", "yellow", "green"))(100))
#
#   for (i in node_info$nodes) {
#     # i = which(node_info$is_leaf)[1]
#     coefs_this <- coefs[[as.character(i)]]
#     ni <- node_info[i,]
#
#
#     convert_num <- function(x) {
#       x_rnd <- round(x * 100, 1)
#       add_space_pre <- ifelse(x_rnd < 10, "  ", "")
#       add_0 <- ifelse(x_rnd == round(x * 100, 0), ".0", "")
#       return(paste0(add_space_pre,
#                     as.character(round(x * 100, 1)),
#                     add_0))
#     }
#     nodes$label[i] <- paste0("nObs = ", ni$num_averaging,
#                              "\n===========\n",
#                              "untr BL = ", convert_num(coefs_this[1]), "%\n",
#                              "TE(CD) = ", convert_num(coefs_this[2]), "%\n",
#                              "TE(HT) = ", convert_num(coefs_this[3]), "%\n",
#                              "TE(SE) = ", convert_num(coefs_this[4]), "%\n",
#                              "TE(NE) = ", convert_num(coefs_this[5]), "%"
#     )
#     nodes$color[i]  <- as.character(color_code$color[
#       which.min((color_code$value - round(coefs_this[5] * 100, 1))^2)])
#   }
#
#   (p1 <-
#       visNetwork(
#         nodes,
#         edges,
#         width = "60%",
#         height = "500px",
#         main = paste("Tree", tree.id)
#       ) %>%
#       visEdges(arrows = "to") %>%
#       visHierarchicalLayout() %>% visExport(type = "png", name = "ridge_tree"))
#
# }

# Tree 2: Slearner and we look at W: -------------------------------------------
rft <- forestry(x = as.data.frame(social %>% select(-y)),
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

source(file = "code/4-gotv_S_plot_fkt.R")

# pdta <- plot(rft, tree.id = 1, return.plot.dta = TRUE)

plotit(x = rft, tree.id = 1)
ggsave(filename = "figures/trees_new/tree1.pdf")
plotit(x = rft, tree.id = 2)
ggsave(filename = "figures/trees_new/tree2.pdf")
plotit(x = rft, tree.id = 3)
ggsave(filename = "figures/trees_new/tree3.pdf")
plotit(x = rft, tree.id = 4)
ggsave(filename = "figures/trees_new/tree4.pdf")


# 3. Create 6 nice looking trees with the same message
# 4. Write that it is important to create many trees + honesty
