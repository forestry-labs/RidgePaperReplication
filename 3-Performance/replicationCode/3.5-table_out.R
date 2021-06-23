# Output information about the performance -------------------------------------
# Run from here to get superheat table -----------------------------------------
library(tidyverse)
# library(xtable)
# library(ztable)
library(magrittr)
library(RColorBrewer)
library(superheat)

options(ztable.type="latex")


X <- read.csv("replicationCode/3-run_all_cluster_resultsEMSE.csv", stringsAsFactors = FALSE)
X$Dataset <- gsub(pattern = "_fold[12345]", replacement = "", x = X$Dataset)

X %>%
  group_by(Dataset) %>%
  summarize(forestryRF = mean(forestryRF),
            caretRidgeRF = mean(caretRidgeRF_nonstrict),
            caretRidgeTree = mean(caretRidgeTree),
            ranger = mean(ranger),
            glmnet = mean(glmnet),
            cubist = mean(cubist),
            local_RF = mean(local_RF),
            BART = mean(BART),
            GBM = mean(gbm),
            RuleFit = mean(pre)) %>%
  dplyr::rename(RF_forestry = forestryRF,
                Ridge_RF = caretRidgeRF,
                Ridge_Tree = caretRidgeTree,
                RF_ranger = ranger) %>%
  dplyr::select(Dataset, RF_forestry, RF_ranger, glmnet, BART, cubist,
                Ridge_Tree, local_RF, Ridge_RF, everything()) ->
  X_toprint

# Save the table ------------------------------------------------
bold <- function(x) {paste('{\\textbf{',x,'}}', sep = '')}

# print(
#   xtable(X_toprint, align = rep('r', ncol(X_toprint) + 1)),
#   sanitize.rownames.function = bold,
#   sanitize.colnames.function = identity,
#   sanitize.text.function = identity,
#   latex.environments = "flushleft",
#   file = "replicationCode/performanceTables.tex"
# )
X_toprint[,-1] <- sqrt(X_toprint[,-1])
X_toprint <- as.data.frame(X_toprint)

minimizer_pos <- apply(X_toprint[,-1], 1, which.min) + 1

X_toprint_char <- X_toprint
for (i in 2:ncol(X_toprint_char)) {
  X_toprint_char[ , i] <- as.character(round(X_toprint_char[ , i], 2))
}
X_toprint_char$Dataset <- gsub("_", " ", X_toprint_char$Dataset)
colnames(X_toprint_char) <- gsub("_", " ", colnames(X_toprint_char))
#X_toprint_char$Dataset[4] <- "Boston" # THIS MUST HAVE BEEN LEFT FROM EARLIER

X_toprint_char
X_toprint_char <- rbind(c("", "forestry", "ranger", "glmnet", "dbarts", "Cubist",
                          "forestry", "grf", "forestry","gbm","pre"),
                        X_toprint_char)
colnames(X_toprint_char) <- c("", "RF \n (Rforestry)",
                              "RF \n (ranger)",
                              "RLM \n (glmnet)",
                              "BART \n (dbarts)",
                              "Cubist \n (Cubist)",
                              "LCART \n (Rforestry)",
                              "Local Linear Forest \n (grf)",
                              "Linear RF \n (Rforestry)",
                              "GBM \n (gbm)",
                              "RuleFit \n (pre)")


# SuperHeat Table ==============================================================
library(matrixStats)
z <- X_toprint_char[c(2,8:15),]
z_raw <- X_toprint[c(1,7:14),-1]

row_ranks <- rowRanks(as.matrix(z_raw))
mean_ranks <- t(as.matrix(colMeans(as.data.frame(row_ranks))))
mean_ranks <- round(mean_ranks, digits = 2)
mean_rank_data <- cbind("Mean Rank", as.data.frame(mean_ranks))
colnames(mean_rank_data) <- names(z)
z <- rbind(z, mean_rank_data[1,])
z_raw <- rbind(z_raw, mean_ranks[1,])
# Create mean rank =============================================================

names(z)



z <- z[,c(1,4,3,2,5:11)]
z_raw <- z_raw[,c(3,2,1,4:10)]


colnames(z) <- c("", "RLM \n (glmnet)",
                 "RF \n (ranger)",
                 "RF \n (Rforestry)",
                 "BART \n (dbarts)",
                 "Cubist \n (Cubist)",
                 "LCART \n (Rforestry)",
                 "LLF \n (grf)",
                 "LRF \n (Rforestry)",
                 "GBM \n (gbm)",
                 "RuleFit \n (pre)")

names <- z[,1]
z <- z[,-1]
row.names(z) <- names

#colnames(z) <- names
# mat <- data.matrix(z)
#
#
# rows <- row.names(z)
#mat[,c(3,8)] = NA

a <- data.matrix(t(scale(t(z_raw))))

attr(a,"scaled:center")<-NULL
attr(a,"scaled:scale")<-NULL

a[,c(1)] = NA

rownames(a) <- rownames(z)
colnames(a) <- colnames(z)

a <- a[,c(1:7,9:10,8)]
z <- z[,c(1:7,9:10,8)]

png("performance.png", height = 900, width = 1800)

superheat(data.matrix(a),
          X.text = as.matrix(z),
          X.text.size = 10,
          heat.na.col = "grey",
          title.size = 18,
          legend = FALSE,
          order.rows = nrow(z):1,
          heat.pal = c("green", "yellow", "red"),
          bottom.label.text.angle = 90,
          grid.hline = FALSE,
          grid.vline = FALSE,
          left.label.text.size = 11,
          bottom.label.text.size = 8)
dev.off()

