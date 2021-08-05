# Investigate how linear aggregation affects the random forest variable importance
library(Rforestry)
library(dplyr)


# Runs one rep of the varible importance experiment from Appendix 9
run_sim <- function(seed) {
  set.seed(seed)

  p <- 10
  sd <- 1

  nonlinear_func <- function(x) {
    return(2/(1+exp(-12*(x-.5))))
  }

  # Generate nonlinear DGP with 10 covariates ==================================
  n <- 1000
  x <- data.frame(matrix(rnorm(n*p), ncol = p, nrow = n))
  y <- 2*sapply(x[,1], nonlinear_func)*sapply(x[,2], nonlinear_func) +
    2*sapply(x[,3], nonlinear_func) + 3*sapply(x[,4], nonlinear_func)
  data <- data.frame(x,y)

  # Train a LRF ================================================================
  es_trnd <- forestry(x = data %>% dplyr::select(-y),
                      y = data %>% dplyr::select(y) %>% .[,1],
                      linear = TRUE,
                      nodesizeStrictSpl = 20,
                      overfitPenalty = 1)

  linear.vi <- getVI(es_trnd)

  # Train a standard RF ========================================================
  es_rf <- forestry(x = data %>% dplyr::select(-y),
                    y = data %>% dplyr::select(y) %>% .[,1],
                    linear = FALSE,
                    nodesizeStrictSpl = 10,
                    overfitPenalty = 1)

  rf.vi <- getVI(es_rf)

  return(list("RF" = rf.vi, "LRF" = linear.vi))
}

# Run for 100 monte carlo reps =================================================
results <- data.frame(rep = 1:100)
results$lrf <- NA
results$rf <- NA

for (rep_i in 1:100) {
  expr <- run_sim(rep_i)
  print(expr)
  results$lrf[rep_i] <- expr[["LRF"]]
  results$rf[rep_i] <- expr[["RF"]]
  print(paste0("Rep ", rep_i))

}

# Save results =================================================================
lrf.vi <- rowMeans(sapply(results$lrf, unlist))
rf.vi <- rowMeans(sapply(results$rf, unlist))

write.csv(cbind(lrf.vi,rf.vi), file = "results/vi_results.csv")

lrf.vi <- lrf.vi / mean(lrf.vi)
rf.vi <- rf.vi / mean(rf.vi)


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



