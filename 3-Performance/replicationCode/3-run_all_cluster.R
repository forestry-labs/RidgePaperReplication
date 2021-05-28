library(Rforestry)
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
batch_func <- function(i, force = FALSE, run_saved = FALSE){
  library(dplyr)
  set.seed(6264175)
  (this_job <- all_jobs[i, ])

  (filename <- paste0("replicationCode/3-results/job_",
                     this_job$Dataset, "_",
                     this_job$Estimator,".csv"))

  es_names <- list("forestryRF" = "ForestryForest",
                   "glmnet" = "glmnet",
                   "cubist" = "cubist",
                   "caretRidgeRF_nonstrict" = "caretRidgeRF_nonstrict",
                   "local_RF" = "local_rf",
                   "ranger" = "rangerForest",
                   "caretRidgeTree" = "RidgeTree",
                   "pre" = "pre",
                   "gbm" = "gbm")
  if (run_saved && this_job$Estimator == "BART") {
    #BART ran without hyperparameter tuning
    run_saved = FALSE
  }

  if ((!substr(filename, 27, 1000) %in% dir("replicationCode/3-results")) |
      force) {

    ds <- datasets_grid[[as.character(this_job$Dataset)]]
    pd <- predictor_grid[[as.character(this_job$Estimator)]]
    # run the current job this will save the results in 3-results/
    tm <- microbenchmark::microbenchmark({
      # If we don't want to run with saved hyperparameters
      if (!run_saved) {
        es <- estimator_grid[[as.character(this_job$Estimator)]]
        es_trnd <- es(Xobs = ds$train %>% dplyr::select(-y),
                      Yobs = ds$train %>% dplyr::select(y) %>% .[,1],
                      note = as.character(this_job$Dataset))
      # Else check that the hyperparameters exist
      } else if (run_saved) {
        es_name <- es_names[[as.character(this_job$Estimator)]]
        loaded_model <- readRDS(paste0("replicationCode/tuningParam/",es_name,this_job$Dataset,".RDS"))

        # If we have reloaded a forestry object, relink the C++ ptr will not work
        # as this is an old version of forestry, so we pull the hyperparameters
        # and pass them as a list to the model
        if (es_name %in% c("ForestryForest", "caretRidgeRF_nonstrict", "RidgeTree")) {
          es <- loaded_model[[1]]$finalModel
          parameters <- list("mtry" = es@mtry,
                             "nodesizeStrictSpl" = es@nodesizeStrictSpl,
                             "ntree" = es@ntree,
                             "sample.fraction" = round(es@sampsize/es@processed_dta$nObservations),
                             "minSplitGain" = es@minSplitGain,
                             "overfitPenalty" = es@overfitPenalty)

          es <- estimator_grid[[as.character(this_job$Estimator)]]

          es_trnd <- es(Xobs = ds$train %>% dplyr::select(-y),
                        Yobs = ds$train %>% dplyr::select(y) %>% .[,1],
                        note = as.character(this_job$Dataset),
                        paramList = parameters)
        } else if (es_name %in% c("cubist")) {
          es <- loaded_model[[1]]$finalModel
          parameters <- list("committees" = es$committees,
                             "extrapolation" = es$control$extrapolation,
                             "neighbors" = es$tuneValues$neighbors)

          es <- estimator_grid[[as.character(this_job$Estimator)]]

          es_trnd <- es(Xobs = ds$train %>% dplyr::select(-y),
                        Yobs = ds$train %>% dplyr::select(y) %>% .[,1],
                        note = as.character(this_job$Dataset),
                        paramList = parameters)

        } else if (es_name %in% c("local_rf")) {
          es <- loaded_model[[1]]$finalModel
          parameters <- list("num.trees" = es$num.trees,
                             "sample.fraction" = unname(es$tunable.params["sample.fraction"]),
                             "min.node.size" = es$min.node.size,
                             "mtry" = unname(es$tunable.params["mtry"]))

          es <- estimator_grid[[as.character(this_job$Estimator)]]

          es_trnd <- es(Xobs = ds$train %>% dplyr::select(-y),
                        Yobs = ds$train %>% dplyr::select(y) %>% .[,1],
                        note = as.character(this_job$Dataset),
                        paramList = parameters)

        } else if (es_name %in% c("glmnet")) {
          es <- loaded_model[[1]]$finalModel
          parameters <- list("lambda" = es$lambda,
                             "alpha" = es$tuneValue$alpha)

          es <- estimator_grid[[as.character(this_job$Estimator)]]

          es_trnd <- es(Xobs = ds$train %>% dplyr::select(-y),
                        Yobs = ds$train %>% dplyr::select(y) %>% .[,1],
                        note = as.character(this_job$Dataset),
                        paramList = parameters)

        } else if (es_name %in% c("rangerForest")) {
          es <- loaded_model[[1]]$finalModel
          parameters <- list("ntree" = es$num.trees,
                             "sample.fraction" = es$tuneValue$sample.fraction,
                             "min.node.size" = es$min.node.size,
                             "mtry" = es$mtry)

          es <- estimator_grid[[as.character(this_job$Estimator)]]

          es_trnd <- es(Xobs = ds$train %>% dplyr::select(-y),
                        Yobs = ds$train %>% dplyr::select(y) %>% .[,1],
                        note = as.character(this_job$Dataset),
                        paramList = parameters)
        } else if (es_name %in% c("pre")) {
          es <- loaded_model[[1]]$bestTune

          parameters <- list("ntrees" = es$ntrees,
                             "maxdepth" = es$maxdepth,
                             "learnrate" = es$learnrate,
                             "mtry" = es$mtry)

          es <- estimator_grid[[as.character(this_job$Estimator)]]

          es_trnd <- es(Xobs = ds$train %>% dplyr::select(-y),
                        Yobs = ds$train %>% dplyr::select(y) %>% .[,1],
                        note = as.character(this_job$Dataset),
                        paramList = parameters)
        } else if (es_name %in% c("gbm")) {
          es <- loaded_model[[1]]$bestTune

          parameters <- list("n.trees" = es$n.trees,
                             "interaction.depth" = es$interaction.depth,
                             "shrinkage" = es$shrinkage,
                             "n.minobsinnode" = es$n.minobsinnode)

          es <- estimator_grid[[as.character(this_job$Estimator)]]

          es_trnd <- es(Xobs = ds$train %>% dplyr::select(-y),
                        Yobs = ds$train %>% dplyr::select(y) %>% .[,1],
                        note = as.character(this_job$Dataset),
                        paramList = parameters)
        }

        # Clean up environment so it doesn't get messy
        rm(loaded_model)
      }

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
    print(this_job)
    print(filename)

    # Update the EMSE table
    update_tables()
  }
  return(filename)
}

# Test the run_saved functionality for each estimator
# print(paste("RUNNING", all_jobs[1, 1], "----", all_jobs[1, 2]))
# batch_func(i = 1, force = TRUE, run_saved = TRUE)
#
# print(paste("RUNNING", all_jobs[45, 1], "----", all_jobs[45, 2]))
# batch_func(i = 45, force = TRUE, run_saved = TRUE)
#
# print(paste("RUNNING", all_jobs[80, 1], "----", all_jobs[80, 2]))
# batch_func(i = 80, force = TRUE, run_saved = TRUE)
#
# print(paste("RUNNING", all_jobs[211, 1], "----", all_jobs[211, 2]))
# batch_func(i = 211, force = TRUE, run_saved = TRUE)
#
# print(paste("RUNNING", all_jobs[217, 1], "----", all_jobs[217, 2]))
# batch_func(i = 217, force = TRUE, run_saved = TRUE)
#
# print(paste("RUNNING", all_jobs[176, 1], "----", all_jobs[176, 2]))
# batch_func(i = 176, force = TRUE, run_saved = TRUE)
#
# print(paste("RUNNING", all_jobs[109, 1], "----", all_jobs[109, 2]))
# batch_func(i = 109, force = TRUE, run_saved = TRUE)



# Set tables before running simulations
update_tables()

num_cores <- parallel::detectCores(all.tests = FALSE, logical = TRUE)
cl <- makePSOCKcluster(num_cores-1)
registerDoParallel(cl)

print("running things in parallel")

# The following code will run all the simulations from Section 3
# in order to only run a subset, one can call the batch_func
# with the desired job_id's only

foreach(i = which(all_jobs$Estimator %in% c("gbm", "pre"))) %dopar% {
  print(paste("RUNNING", all_jobs[i, 1], "----", all_jobs[i, 2]))

  # In order to run the simulations with hyperparameter tuning, change the
  # run_saved flag to FALSE. This may take a very long time however

  batch_func(i = i, force = TRUE, run_saved = TRUE)
  print(paste("Done with", all_jobs[i,1], "----", all_jobs[i,2]))
}

update_tables()
