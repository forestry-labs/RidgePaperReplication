# devtools::install_github("soerenkuenzel/causalToolbox")
# devtools::install_github("soerenkuenzel/forestry", ref = "splitting-features")
devtools::load_all("~/Dropbox/forestry/")
# library(forestry)
library(visNetwork)
library(tidyverse)

load("GerberGreenLarimer_r1b.RData", verbose = TRUE)
social <- tbl_df(social)[,-1] %>% 
  dplyr::mutate(
    WC = ifelse(treatment == " Civic Duty", 1, 0),
    WH = ifelse(treatment == " Hawthorne", 1, 0),
    WS = ifelse(treatment == " Self", 1, 0),
    WN = ifelse(treatment == " Neighbors", 1, 0),
    y = b.voted
  ) %>% 
  select(y, WC, WH, WS, WN, g2000, g2002, p2000, p2002, p2004, age)

social$p2004 <- factor(gsub("Y", "y", gsub("N", "n", social$p2004)))
social$CVH <- apply(social[, c("g2000", "g2002", "p2000", "p2002", "p2004")] == "yes", 1, sum)
social$CGVH <- apply(social[, c("g2000", "g2002")] == "yes", 1, sum)
social$CPVH <- apply(social[, c("p2000", "p2002", "p2004")] == "yes", 1, sum)

summary(social)

# define custom plot fnkt ------------------------------------------------------
custom_plot <- function(pdta) {
  node_info <- pdta[[1]]
  nodes <- pdta[[2]]
  edges <- pdta[[3]]
  coefs <- pdta[[4]]
  tree.id <- pdta[[5]] 
  
  # min_TE <- min(do.call("rbind", coefs)[,2]) * 100
  # max_TE <- max(do.call("rbind", coefs)[,2]) * 100
  min_TE <- 3
  max_TE <- 11.5
  color_code <- data.frame(value = seq(min_TE, max_TE, length.out = 100))
  color_code$color <- as.character(colorRampPalette(c("red", "yellow", "green"))(100))
  
  for (i in which(node_info$is_leaf)) {
    # i = which(node_info$is_leaf)[1]
    coefs_this <- coefs[[as.character(i)]]
    ni <- node_info[i,]
    

    convert_num <- function(x) {
      x_rnd <- round(x * 100, 1)
      add_space_pre <- ifelse(x_rnd < 10, "  ", "")
      add_0 <- ifelse(x_rnd == round(x * 100, 0), ".0", "")
      return(paste0(add_space_pre,
                    as.character(round(x * 100, 1)),
                    add_0))
    }
    nodes$label[i] <- paste0("nObs = ", ni$num_averaging,
                             "\n===========\n",
                             "untr BL = ", convert_num(coefs_this[1]), "%\n",
                             "TE(CD) = ", convert_num(coefs_this[2]), "%\n",
                             "TE(HT) = ", convert_num(coefs_this[3]), "%\n",
                             "TE(SE) = ", convert_num(coefs_this[4]), "%\n",
                             "TE(NE) = ", convert_num(coefs_this[5]), "%"
    )
    nodes$color[i]  <- as.character(color_code$color[
      which.min((color_code$value - round(coefs_this[5] * 100, 1))^2)])
  }
  
  (p1 <-
      visNetwork(
        nodes,
        edges,
        width = "60%",
        height = "500px",
        main = paste("Tree", tree.id)
      ) %>%
      visEdges(arrows = "to") %>%
      visHierarchicalLayout() %>% visExport(type = "png", name = "ridge_tree"))
  
}

# Tree 2: Slearner and we look at W: -------------------------------------------
rft <- forestry(x = as.data.frame(social %>% select(-y)),
                y = social$y, 
                mtry = ncol(social) - 1,
                sample.fraction = 1,
                nodesizeStrictSpl = 5000,
                ntree = 8,
                minSplitGain = .005,
                ridgeRF = TRUE,
                overfitPenalty = 0.00000001,
                # linFeats = 1:4,
                middleSplit = TRUE,
                verbose = TRUE,
                splitratio = .5)

# pdta <- plot(rft, tree.id = 1, return.plot.dta = TRUE)
custom_plot(plot.forestry(x = rft, tree.id = 1, return.plot.dta = TRUE))
custom_plot(plot(rft, tree.id = 2, return.plot.dta = TRUE))
custom_plot(plot(rft, tree.id = 3, return.plot.dta = TRUE))
custom_plot(plot(rft, tree.id = 4, return.plot.dta = TRUE))
custom_plot(plot(rft, tree.id = 5, return.plot.dta = TRUE))
custom_plot(plot(rft, tree.id = 6, return.plot.dta = TRUE))
custom_plot(plot(rft, tree.id = 7, return.plot.dta = TRUE))
custom_plot(plot(rft, tree.id = 8, return.plot.dta = TRUE))


# 3. Create 6 nice looking trees with the same message
# 4. Write that it is important to create many trees + honesty 