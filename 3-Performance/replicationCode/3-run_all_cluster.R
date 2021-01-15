library(forestry)
library(ranger)
library(glmnet)
library(grf)
library(tidyverse)
library(reshape)
library(Cubist)
library(caret)
library(clustermq)
library(foreach)
library(doParallel)
# set options if running on the cluster

# options(clustermq.scheduler = "slurm", 
#         clustermq.template = "~/clustermq_low.tmpl") 
# 
# options(clustermq.scheduler = "slurm",
#         clustermq.template = "~/clustermq_high.tmpl")
#~/clustermq_low.tmpl ~/clustermq_high.tmpl

dir.create("replicationCode/3-results/", showWarnings = FALSE)

data_folder_name <- "replicationCode/estimates/"
dir.create(data_folder_name, showWarnings = FALSE)


set.seed(5387479) 

source("replicationCode/3.1-generateDataVaryingN.R")
source("replicationCode/3.2-generateDataBrieman.R")
source("replicationCode/3.3-DS_autos_bike_soe.R")
source("replicationCode/3.4-generateEstimators.R")
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
  # in 3-run_all_cluster_results.csv
  
  # updates all_jobs
  for (file in dir("replicationCode/3-results/")) {
    results <- read.csv(paste0("replicationCode/3-results/", file))
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
            file = "replicationCode/3-run_all_cluster_resultsEMSE.csv")
  Runtime_table <- reshape2::dcast(data = all_jobs,
                                   formula = Dataset ~ Estimator,
                                   value.var = "runtime") 
  write.csv(x = Runtime_table, 
            file = "replicationCode/3-run_all_cluster_resultsRuntime.csv")
}

# run the jobs -----------------------------------------------------------------
batch_func <- function(i, force = FALSE){
  library(dplyr)
  set.seed(6264175)
  (this_job <- all_jobs[i, ])

  (filename <- paste0("replicationCode/3-results/job_", 
                     this_job$Dataset, "_", 
                     this_job$Estimator,".csv"))
  
  if ((!substr(filename, 27, 1000) %in% dir("replicationCode/3-results")) | 
      force) {
    
    ds <- datasets_grid[[as.character(this_job$Dataset)]]
    es <- estimator_grid[[as.character(this_job$Estimator)]]
    pd <- predictor_grid[[as.character(this_job$Estimator)]]
    # run the current job this will save the results in 3-results/
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
    write.csv(x = this_job, 
              file = filename, 
              row.names = FALSE) 
    
    # Update the EMSE table 
    update_tables()
    
  }
  return(filename)
}

# Set tables before running simulations
update_tables()

num_cores <- parallel::detectCores(all.tests = FALSE, logical = TRUE)
cl <- makePSOCKcluster(num_cores-1)
registerDoParallel(cl)

print("running things in parallel")

foreach(i = c(1:nrow(all_jobs))) %dopar% {
  print(paste("RUNNING", all_jobs[i, 1], "----", all_jobs[i, 2]))
  batch_func(i = i, force = FALSE)
  print(paste("Done with", all_jobs[i,1], "----", all_jobs[i,2]))
}

update_tables()
