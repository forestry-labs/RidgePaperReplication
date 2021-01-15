# devtools::install_github("soerenkuenzel/causalToolbox")
# devtools::install_github("soerenkuenzel/forestry", ref = "splitting-features")
devtools::load_all("~/Dropbox/forestry/")
library(forestry)
library(visNetwork)
library(tidyverse)

load("GerberGreenLarimer_r1b.RData", verbose = TRUE)
social <- tbl_df(social)[,-1] %>% 
  mutate(
    treatment = as.character(treatment)
  ) %>% 
  filter(treatment %in% c(" Control", " Neighbors")) %>%
  mutate(
    treatment = ifelse(treatment == " Control", 0, 1)
  )

mean(social$b.voted[social$treatment == 1]) - 
  mean(social$b.voted[social$treatment == 0])

(y = social$b.voted)
(w = social$treatment)

(feat <- social[ , c(1, 3:4, 6:8, 18)])
feat$p2004 <- factor(gsub("Y", "y", gsub("N", "n", feat$p2004)))
# feat$age2 <- feat$age^2
feat$CVH <- apply(feat[, c("g2000", "g2002", "p2000", "p2002", "p2004")] == "yes", 1, sum)
feat$CGVH <- apply(feat[, c("g2000", "g2002")] == "yes", 1, sum)
feat$CPVH <- apply(feat[, c("p2000", "p2002", "p2004")] == "yes", 1, sum)
feat$agegroup <- as.factor(ifelse(feat$age > 65, "retired", "not retired"))



# define custom plot fnkt ------------------------------------------------------
custom_plot <- function(pdta) {
  node_info <- pdta[[1]]
  nodes <- pdta[[2]]
  edges <- pdta[[3]]
  coefs <- pdta[[4]]
  tree.id <- pdta[[5]] 
  
  min_TE <- min(do.call("rbind", coefs)[,2]) * 100
  max_TE <- max(do.call("rbind", coefs)[,2]) * 100
  min_TE <- 3
  max_TE <- 11.5
  color_code <- data.frame(value = seq(min_TE, max_TE, length.out = 100))
  color_code$color <- as.character(colorRampPalette(c("red", "yellow", "cornflowerblue"))(100))
  
  for (i in which(node_info$is_leaf)) {
    # i = which(node_info$is_leaf)[1]
    coefs_this <- coefs[[as.character(i)]]
    ni <- node_info[i,]
    
    add0_1 <- add0_2 <- ""
    if(round(coefs_this[1] * 100, 1) - round(coefs_this[1] * 100, 0) == 0){
      add0_1 <- ".0"
    } 
    if(round(coefs_this[2] * 100, 1) - round(coefs_this[2] * 100, 0) == 0){
      add0_2 <- ".0"
    } 
    spacing_1 <- ifelse(round(coefs_this[1] * 100, 1) < 10, "  ", "")
    spacing_2 <- ifelse(round(coefs_this[2] * 100, 1) < 10, "  ", "")
    nodes$label[i] <- paste0("nObs = ", ni$num_averaging,
                             "\n===========\n",
                             "BL = ", spacing_1, round(coefs_this[1] * 100,1), add0_1, "%\n",
                             "TE = ", spacing_2, round(coefs_this[2] * 100, 1), add0_2, "%"
    )
    nodes$color[i]  <- as.character(color_code$color[
      which.min((color_code$value - round(coefs_this[2] * 100, 1))^2)])
  }
  
  (p1 <-
      visNetwork(
        nodes,
        edges,
        width = "100%",
        height = "800px",
        main = paste("Tree", tree.id)
      ) %>%
      visEdges(arrows = "to") %>%
      visHierarchicalLayout() %>% visExport(type = "png", name = "ridge_tree"))
  
}

# Tree 2: Slearner and we look at W: -------------------------------------------
rft <- forestry(x = data.frame(w, feat),
                y = y, 
                mtry = ncol(feat),
                sample.fraction = 1,
                nodesizeStrictSpl = 10000,
                ntree = 8,
                minSplitGain = .0001,
                ridgeRF = TRUE,
                overfitPenalty = 0.0000000000001,
                linFeats = 1,
                middleSplit = TRUE,
                verbose = TRUE,
                splitratio = .5)

custom_plot(plot(rft, tree.id = 1, return.plot.dta = TRUE))
custom_plot(plot(rft, tree.id = 2, return.plot.dta = TRUE))
custom_plot(plot(rft, tree.id = 3, return.plot.dta = TRUE))
custom_plot(plot(rft, tree.id = 4, return.plot.dta = TRUE))
custom_plot(plot(rft, tree.id = 5, return.plot.dta = TRUE))
custom_plot(plot(rft, tree.id = 6, return.plot.dta = TRUE))
custom_plot(plot(rft, tree.id = 7, return.plot.dta = TRUE))
custom_plot(plot(rft, tree.id = 8, return.plot.dta = TRUE))


# 3. Create 6 nice looking trees with the same message
# 4. Write that it is important to create many trees + honesty 