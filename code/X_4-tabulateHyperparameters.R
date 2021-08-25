library(forestry)
library(ranger)
library(glmnet)
library(grf)
library(tidyverse)
library(reshape)
library(Cubist)
library(caret)
library(xtable)

data_folder_name <- "tuningParam/"

source("code/3.1-generateDataVaryingN.R")
source("code/3.2-generateDataBrieman.R")
source("code/3.3-DS_autos_bike_soe.R")


set.seed(634801)

# Loop through data sets -------------------------------------------------------

# Validate Lambda selection
# Cycle through matrix of estimators/datasets_grid
# Output results as .csv

# Set fraction of data set aside for training + several sample sizes used

total_params <- setNames(data.frame(matrix(ncol = 6, nrow = 0)),
                         c("Dataset",
                           "mtry",
                           "nodesizeSpl",
                           "overfitPenalty",
                           "LOGminSplitGain",
                           "sample.fraction"))

# Loop through all datset, estimator combination
for (dataset_i in 1:length(datasets_grid)) {
    # sampsize = 64; dataset_i = 1
    data_name <- names(datasets_grid)[dataset_i]

    print(paste("Dataset =", data_name))

    if (data_name == "simulated-Step-Function-2048" ) {
      print("Skipping")
      next;
    }

    if (data_name == "simulated-StepLinear-Function-2048" ) {
      print("Skipping")
      next;
    }

    params <-
      readRDS(paste0(data_folder_name, "caretRidgeRF_nonstrict", data_name,".RDS"))

    cur_params <- params[[1]]$bestTune

    total_params <- rbind(total_params,c(data_name,
                                         cur_params$mtry[1],
                                         cur_params$nodesizeSpl[1],
                                         cur_params$overfitPenalty[1],
                                         cur_params$minSplitGain[1],
                                         cur_params$sample.fraction[1]))
    colnames(total_params) <-c("Dataset",
                               "mtry",
                               "nodesizeSpl",
                               "overfitPenalty",
                               "LOGminSplitGain",
                               "sample.fraction")


}

total_params$LOGminSplitGain <- log(total_params$LOGminSplitGain %>% as.numeric())

tp_char <- total_params
for (i in 2:ncol(tp_char)) {
  tp_char[ , i] <- as.character(round(tp_char[ , i] %>% as.numeric(), 2))
}

tp_char$Dataset <- gsub("_", " ", tp_char$Dataset)
tp_char$Dataset <- gsub("-", " ", tp_char$Dataset)
tp_char$Dataset <- gsub("simulated", "", tp_char$Dataset)
tp_char$Dataset <- gsub("Function", "", tp_char$Dataset)

write.csv(tp_char, file = "code/hyperparams.csv")
