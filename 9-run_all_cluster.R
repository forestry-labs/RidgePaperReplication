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

# install most up to date version of forestry
# devtools::install_github("soerenkuenzel/forestry", ref = "master")

library(forestry)
library(ranger)
library(glmnet)
library(grf)
library(tidyverse)
library(reshape)
library(Cubist)
library(caret)
library(clustermq)
# set up cluster to run on the high partition

# options(clustermq.scheduler = "slurm", 
#         clustermq.template = "~/clustermq_low.tmpl") 

options(clustermq.scheduler = "slurm",
        clustermq.template = "~/clustermq_high.tmpl")
#~/clustermq_low.tmpl ~/clustermq_high.tmpl

dir.create("replicationCode/9-results/", showWarnings = FALSE)

data_folder_name <- "replicationCode/estimates/"
dir.create(data_folder_name, showWarnings = FALSE)

#set.seed(634801) Previous seed with bad LM Medium Trend
set.seed(5387479) 
#source("replicationCode/1.5-generateDataBigtest.R")
source("replicationCode/2-generateEstimators.R")
# source("replicationCode/2.1-generateEstimators_RRF.R")
source("replicationCode/1.8-generateDataBrieman.R")
source("replicationCode/1.9-DS_autos_bike_soe.R")
source("replicationCode/1.7-generateDataVaryingN.R")
# generate all the different jobs and save it ----------------------------------
(ds_names <- names(datasets_grid))
(etm_names <- names(estimator_grid))

(all_jobs <- expand.grid(ds_names, etm_names) %>% 
  dplyr::rename(Dataset = Var1, Estimator = Var2)) %>% 
  dplyr::arrange(Dataset)

all_jobs$EMSE <- NA
all_jobs$runtime <- NA


# update EMSE table ------------------------------------------------------------
update_tables <- function(){
  # Reads in all the predicitons in results and computes the EMSE and saves it 
  # in 9-run_all_cluster_results.csv
  
  # updates all_jobs
  for (file in dir("replicationCode/9-results/")) {
    results <- read.csv(paste0("replicationCode/9-results/", file))
    this_row <- 
      as.character(all_jobs$Dataset) == as.character(results$Dataset) &  
      as.character(all_jobs$Estimator) == as.character(results$Estimator)
    all_jobs[this_row, ] <- results
  }
  
  # save
  EMSE_table <- reshape2::dcast(data = all_jobs,
                                formula = Dataset ~ Estimator,
                                value.var = "EMSE") 
  write.csv(x = EMSE_table, 
            file = "replicationCode/9-run_all_cluster_resultsEMSE.csv")
  Runtime_table <- reshape2::dcast(data = all_jobs,
                                   formula = Dataset ~ Estimator,
                                   value.var = "runtime") 
  write.csv(x = Runtime_table, 
            file = "replicationCode/9-run_all_cluster_resultsRuntime.csv")
}

# run the jobs -----------------------------------------------------------------
batch_func <- function(i, force = FALSE){
  library(dplyr)
  # i <- 32
  set.seed(6264175)
  (this_job <- all_jobs[i, ])

  (filename <- paste0("replicationCode/9-results/job_", 
                     this_job$Dataset, "_", 
                     this_job$Estimator,".csv"))
  
  if ((!substr(filename, 27, 1000) %in% dir("replicationCode/9-results")) | 
      force) {
    
    ds <- datasets_grid[[as.character(this_job$Dataset)]]
    es <- estimator_grid[[as.character(this_job$Estimator)]]
    pd <- predictor_grid[[as.character(this_job$Estimator)]]
    # run the current job this will save the results in 9-results/
    tm <- microbenchmark::microbenchmark({
      es_trnd <- es(Xobs = ds$train %>% dplyr::select(-y),
                    Yobs = ds$train %>% dplyr::select(y) %>% .[,1], 
                    note = as.character(this_job$Dataset))
      pdctns <- pd(estimator = es_trnd,
                   feat = ds$test %>% dplyr::select(-y))
      EMSE <- mean((pdctns - ds$test %>% dplyr::select(y) %>% .[,1])^2)
    }, times = 1, unit = "s")
    this_job$EMSE <- EMSE
    this_job$runtime <- summary(tm)$mean
    # save the job
    
    # Rerun the semilinear ridge RF  
    if (filename == "replicationCode/9-results/job_simulated-StepLinear-Function-2048_ridgeRFStepLinear.csv") {
      # Rename the results before we save
      filename <- "replicationCode/9-results/job_simulated-StepLinear-Function-2048_caretRidgeRF_nonstrict.csv"
      this_job$Estimator <- "caretRidgeRF_nonstrict"
    }
    
    write.csv(x = this_job, 
              file = filename, 
              row.names = FALSE) 
    
    # Update the EMSE table 
    update_tables()
    
  }
  return(filename)
}
# which(all_jobs$Estimator == "caretRidgeRF")
# all_jobs[all_jobs$Estimator == "caretRidgeRF", ]
# for (i in which(all_jobs$Estimator == "BART")) {
#   batch_func(i = i, force = FALSE)
# }
# 
# print("running things in parallel")
# library(foreach)
# library(doParallel)
# cl <- makeCluster(8)
# registerDoParallel(cl)
# foreach(i = c(130:132, 135:137, 140:142, 166:180, 238:240, 243:245, 248:250)) %dopar% {
#   source("replicationCode/2-generateEstimators.R")
#   batch_func(i = i, force = TRUE)
#   i
# }
# all_jobs[c(130:132, 135:137, 140:142, 166:180, 238:240, 243:245, 248:250), ]
# 
# stop("done")
# all_jobs
# which(all_jobs$Dataset == "Friedman_1" & 
#         all_jobs$Estimator == "caretRidgeRF")
# 
# for (i in c(166:180, 130:144)) {
#   print(paste("##################### DONE WITH", i, "###################"))
#   batch_func(i = i, force = TRUE)
# }
# stop("done")
# 
# batch_func(i = 256, force = TRUE)
# which(all_jobs$Estimator == "caretRidgeRF")
# all_jobs[all_jobs$Estimator == "caretRidgeRF", ]
# batch_func(i = 37, force = FALSE)
# 
# 
# print("running things in parallel")
# library(foreach)
# library(doParallel)
# parallel::detectCores(all.tests = FALSE, logical = TRUE)
# cl <- makeCluster(8)
# registerDoParallel(cl)
# foreach(i = which(all_jobs$Estimator ==  "caretRidgeRF")) %dopar% {
#   batch_func(i = i, force = FALSE)
#   i
# }
# stop("done")
# 
# print("running things in parallel")
# library(foreach)
library(doParallel)
parallel::detectCores(all.tests = FALSE, logical = TRUE)
cl <- makePSOCKcluster(20)
registerDoParallel(cl)

for (i in which(all_jobs$Estimator %in% c("caretRidgeRF_nonstrict"))) {
  print(paste("RUNNING", all_jobs[i, 1], "----", all_jobs[i, 2]))
  batch_func(i = i, force = FALSE)
  print(paste("Done with", all_jobs[i,1], "----", all_jobs[i,2]))
}


stop("done")

# which(all_jobs$Dataset == "simulated-StepLinear-Function-2048" &
# stop("DONE!")

Q(fun = batch_func,
  n_jobs = nrow(all_jobs),
  i = 1:nrow(all_jobs),
  export = list(
    datasets_grid = datasets_grid,
    estimator_grid = estimator_grid,
    predictor_grid = predictor_grid,
    all_jobs = all_jobs,
    update_tables = update_tables,
    create_random_node_sizes = create_random_node_sizes
  ))

update_tables()
read.csv("replicationCode/9-run_all_cluster_resultsEMSE.csv")
tt <- read.csv("replicationCode/9-run_all_cluster_resultsEMSE.csv")[, c(2, 9, 4, 11:13)]
tt[,-1] <- tt[,-1] / tt$local_RF
tt
read.csv("replicationCode/9-run_all_cluster_resultsRuntime.csv")


X <- read.csv("replicationCode/9-run_all_cluster_resultsEMSE.csv", stringsAsFactors = FALSE)
X$Dataset <- gsub(pattern = "_fold[12345]", replacement = "", x = X$Dataset)

X %>%
  group_by(Dataset) %>%
  summarize(forestryRF = mean(forestryRF),
            caretRidgeRF = mean(caretRidgeRF),
            caretRidgeTree = mean(caretRidgeTree),
            ranger = mean(ranger),
            glmnet = mean(glmnet),
            cubist = mean(cubist),
            local_RF = mean(local_RF),
            BART = mean(BART),
            caretRidgeRF_BT = mean(caretRidgeRF_BT),
            caretRidgeRF_noMinSplitGain = mean(caretRidgeRF_noMinSplitGain),
            caretRidgeTree_moreSplit = mean(caretRidgeTree_moreSplit)) %>% 
  dplyr::rename(RF_forestry = forestryRF,
                Ridge_RF = caretRidgeRF,
                Ridge_Tree = caretRidgeTree,
                RF_ranger = ranger) %>%
  dplyr::select(Dataset, RF_forestry, RF_ranger, glmnet, BART, cubist,
                Ridge_Tree, local_RF, Ridge_RF, everything()) -> X
X[,-1] <- sqrt(X[,-1])
X <- X %>% as.data.frame()
# X[,-1] <- round(X[,-1] / apply(X[,-1], 1, function(x) min(x, na.rm = TRUE)), 2)
X[,-1] <- round(X[,-1] / X$Ridge_RF, 3)
X
tt <- readRDS("replicationCode/tuningParam/RidgeForestServo_fold5.RDS")
str(tt)
g <- tt[[1]]
b <- g$results
b %>% arrange(-.B)
b %>% arrange(-RMSE)
b %>% arrange(-.B, RMSE) %>% head(10)
b %>% arrange(-.B, RMSE) %>% tail(10)
# 
# readRDS("replicationCode/tuningParam/local_rfFriedman_2.RDS")
# # readRDS("replicationCode/tuningParam/RidgeForestBoston_Housing_fold1.RDS")
# # 
