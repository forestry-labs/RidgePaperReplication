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

# Generate Data for runtime comparison =========================================

# You need the R only version of the Rforestry package in order to run the
# LRF trees with the naive implementation. This can be installed using
# devtools::install_github("theo-s/Rforestry_R")
require("forestryR")
source("code/3.4-generateDataTiming.R")
source("code/3.4-generateEstimators.R")

# generate all the different jobs and save it ----------------------------------
(ds_names <- names(datasets_grid))
(etm_names <- names(estimator_grid))

(all_jobs <- expand.grid(ds_names, etm_names) %>%
    dplyr::rename(Dataset = Var1, Estimator = Var2)) %>%
  dplyr::arrange(Dataset)

all_jobs$EMSE <- NA
all_jobs$runtime <- NA

# Only get RidgeRF jobs
all_jobs <- all_jobs[which(all_jobs$Estimator %in% c("ridgeRF","RridgeRF")),]

# update EMSE table ------------------------------------------------------------
update_tables <- function(){
  # Reads in all the predicitons in results and computes the EMSE and saves it
  # in 3-run_all_cluster_results.csv

  # updates all_jobs
  for (file in dir("code/timing_results/")) {
    results <- read.csv(paste0("code/timing_results/", file))
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
            file = "code/X_10-runtimeEMSE.csv")
  Runtime_table <- reshape2::dcast(data = all_jobs,
                                   formula = Dataset ~ Estimator,
                                   value.var = "runtime")
  write.csv(x = Runtime_table,
            file = "code/X_10-runtimeRuntime.csv")
}

# run the jobs -----------------------------------------------------------------
batch_func <- function(i, force = FALSE, run_saved = FALSE){
  library(dplyr)
  set.seed(6264175)
  (this_job <- all_jobs[i, ])

  (filename <- paste0("timing_results/job_",
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

  if ((!substr(filename, 16, 1000) %in% dir("code/timing_results")) ||
      force) {

    ds <- datasets_grid[[as.character(this_job$Dataset)]]
    pd <- predictor_grid[[as.character(this_job$Estimator)]]
    # run the current job this will save the results in esults/
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
        loaded_model <- readRDS(paste0("tuningParam/",es_name,this_job$Dataset,".RDS"))

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
    }, times = 100, unit = "s")
    this_job$EMSE <- EMSE
    this_job$runtime <- summary(tm)$mean

    # save the job
    write.csv(x = this_job,
              file = paste0("code/",filename),
              row.names = FALSE)
    print(this_job)
    print(filename)

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

# The following code will run all the simulations from Section 3
# in order to only run a subset, one can call the batch_func
# with the desired job_id's only
foreach(i = which(all_jobs$Estimator %in% c("ridgeRF","RridgeRF"))) %dopar% {
  print(paste("RUNNING", all_jobs[i, 1], "----", all_jobs[i, 2]))

  # In order to run the simulations with hyperparameter tuning, change the
  # run_saved flag to FALSE. This may take a very long time however

  batch_func(i = i, force = FALSE, run_saved = FALSE)
  print(paste("Done with", all_jobs[i,1], "----", all_jobs[i,2]))
}

# Once we have finished running the sims, save results again in EMSE and Runtime files
update_tables()
