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

library(forestry)
library(ranger)
library(glmnet)
library(grf)
library(tidyverse)
library(reshape)
library(Cubist)
library(caret)
library(xtable)

data_folder_name <- "replicationCode/tuningParam/"

source("replicationCode/1.8-generateDataBrieman.R")
source("replicationCode/1.9-DS_autos_bike_soe.R")
source("replicationCode/1.7-generateDataVaryingN.R")

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
    
    total_params <- total_params %>% add_row(Dataset = data_name,
                                             mtry = cur_params$mtry[1],
                                             nodesizeSpl = cur_params$nodesizeSpl[1],
                                             overfitPenalty = cur_params$overfitPenalty[1],
                                             LOGminSplitGain = cur_params$minSplitGain[1],
                                             sample.fraction = cur_params$sample.fraction[1])
    

}

total_params$LOGminSplitGain <- log(total_params$LOGminSplitGain)

tp_char <- total_params
for (i in 2:ncol(tp_char)) {
  tp_char[ , i] <- as.character(round(tp_char[ , i], 2))
}

tp_char$Dataset <- gsub("_", " ", tp_char$Dataset)
tp_char$Dataset <- gsub("-", " ", tp_char$Dataset)
tp_char$Dataset <- gsub("simulated", "", tp_char$Dataset)
tp_char$Dataset <- gsub("Function", "", tp_char$Dataset)

print(
  xtable(tp_char, align = rep('r', ncol(tp_char) + 1), 
         caption = "The table summarizes the selected hyperparameters.", label = "tbl:hyperparameters"),
  include.rownames = FALSE,
  # include.colnames = FALSE, 
  sanitize.colnames.function = identity,
  sanitize.text.function = identity,
  latex.environments = "flushleft",
  file = "~/Dropbox/hyperparameters.tex"
)
