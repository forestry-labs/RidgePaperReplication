
# define helper functions:------------------------------------------------------
create_random_node_sizes <- function(nobs, len) {
  # Function creates random node sizes
  potential_sample_space <- c(1:20, round(exp((1:20) / 4) / exp(5) * nobs / 4))
  potential_sample_space <- unique(potential_sample_space)

  potential_sample_space[potential_sample_space < 2] <- 2

  potential_sample_space <- potential_sample_space[
    potential_sample_space != 0 & potential_sample_space < nobs]

  if (length(potential_sample_space) < 1) {
    stop(paste0("It seems like there are not enough samples to generate ",
                "nodesizes. It looks like the samples size is ", nobs, "."))
  }
  return(sample(potential_sample_space, size = len, replace = TRUE))
}


# Define all estimators:========================================================

estimator_grid <- list()


#Tuning forestry RF ------------------------------------------------------------
estimator_grid[["forestryRF"]] <- function(Xobs,
                                           Yobs,
                                           tune_length = 20,
                                           cv_fold = 8,
                                           note = NA,
                                           paramList = NA) {
  library(Rforestry)
  library(caret)

  if (is.na(paramList[[1]])) {

    forestryRF <- list(
      type = "Regression",
      library = "forestry",
      loop = NULL,
      parameters = data.frame(
        parameter = c(
          "mtry",
          "nodesizeStrictSpl",
          "ntree",
          "sample.fraction"
        ),
        class = rep("numeric", 4),
        label = c(
          "mtry",
          "nodesizeStrictSpl",
          "ntree",
          "sample.fraction"
        )
      ),
      grid = function(x, y, len = NULL, search = "random") {
        ## Define ranges for the parameters and
        ## generate random values for them

        paramGrid <-
          data.frame(
            mtry = sample(1:ncol(x), size = len, replace = TRUE),
            nodesizeStrictSpl = create_random_node_sizes(nobs = nrow(x),
                                                         len = len),
            ntree = 500,
            sample.fraction = runif(len, 0.5, 1))
        return(paramGrid)
      },
      fit = function(x,
                     y,
                     wts,
                     param,
                     lev = NULL,
                     last,
                     weights,
                     classProbs) {
        print(param)

        forestry(
          x = x,
          y = y,
          ntree = param$ntree,
          sample.fraction = param$sample.fraction,
          nodesizeSpl = 1,
          nodesizeAvg = 1,
          nodesizeStrictAvg = 1,
          nthread = 1,
          nodesizeStrictSpl = param$nodesizeStrictSpl,
          mtry = param$mtry,
          saveable = FALSE
        )
      },
      predict = function(modelFit,
                         newdata,
                         preProc = NULL,
                         submodels = NULL) {
        predict(modelFit, newdata)
      },
      prob = NULL
    )

    fitControl <- trainControl(
      method = "adaptive_cv",
      ## 8-fold CV
      number = cv_fold,
      ## repeated 5 times
      repeats = 4,
      adaptive = list(
        min = 3,
        alpha = 0.01,
        method = "gls",
        complete = FALSE
      )
    )

    random_rf <- train(
      y = Yobs,
      x = Xobs,
      method = forestryRF,
      metric = "RMSE",
      tuneLength = tune_length,
      trControl = fitControl
    )

    # Save Tuning parameters ---------------------------------------------------
    dir.create("replicationCode/tuningParam/", showWarnings = FALSE)
    saveRDS(
      object = list(random_rf),
      file = paste0("replicationCode/tuningParam/ForestryForest", note, ".RDS")
    )

    return(list("random_rf" = random_rf$finalModel))

  } else {
    fit <- forestry(x = Xobs,
                    y = Yobs,
                    ntree = paramList$ntree,
                    sample.fraction = paramList$sample.fraction,
                    nodesizeSpl = 1,
                    nodesizeAvg = 1,
                    nodesizeStrictAvg = 1,
                    nthread = 1,
                    nodesizeStrictSpl = paramList$nodesizeStrictSpl,
                    mtry = paramList$mtry,
                    saveable = FALSE)

    return(list("random_rf" = fit))
  }

}

# #Tuning Ridge RF --------------------------------------------------------------
estimator_grid[["caretRidgeRF_nonstrict"]] <- function(Xobs,
                                                       Yobs,
                                                       tune_length = 200,
                                                       cv_fold = 8,
                                                       note = NA,
                                                       paramList = NA) {
  library(Rforestry)
  library(caret)

  if (is.na(paramList[[1]])) {
    ridgeRF <- list(
      type = "Regression",
      library = "forestry",
      loop = NULL,
      parameters = data.frame(
        parameter = c(
          "mtry",
          "nodesizeStrictSpl",
          "overfitPenalty",
          "minSplitGain",
          "ntree",
          "sample.fraction"
        ),
        class = rep("numeric", 6),
        label = c(
          "mtry",
          "nodesizeStrictSpl",
          "overfitPenalty",
          "minSplitGain",
          "ntree",
          "sample.fraction"
        )
      ),
      grid = function(x, y, len = NULL, search = "random") {
        ## Define ranges for the parameters and
        ## generate random values for them

        paramGrid <-
          data.frame(
            mtry = sample(1:ncol(x), size = len, replace = TRUE),
            nodesizeStrictSpl = create_random_node_sizes(nobs = nrow(x),
                                                         len = len),
            # Might want to pass specific range/distribution for lambdas
            overfitPenalty = exp(runif(
              len,
              min = log(.1),
              max = log(10)
            )),
            minSplitGain = runif(len, 0, .5) ^
              4,
            ntree = 500,
            sample.fraction = runif(len, 0.5, 1))
        return(paramGrid)
      },
      fit = function(x,
                     y,
                     wts,
                     param,
                     lev = NULL,
                     last,
                     weights,
                     classProbs) {
        print(param)

        forestry(
          x = x,
          y = y,
          linear = TRUE,
          ntree = param$ntree,
          sample.fraction = param$sample.fraction,
          nodesizeSpl = 1,
          nodesizeAvg = 1,
          nodesizeStrictAvg = 1,
          nthread = 1,
          nodesizeStrictSpl = param$nodesizeStrictSpl,
          mtry = param$mtry,
          overfitPenalty = param$overfitPenalty,
          saveable = FALSE,
          minSplitGain = param$minSplitGain
        )
      },
      predict = function(modelFit,
                         newdata,
                         preProc = NULL,
                         submodels = NULL) {
        predict(modelFit, newdata)
      },
      prob = NULL
    )

    fitControl <- trainControl(
      method = "adaptive_cv",
      ## 8-fold CV
      number = cv_fold,
      ## repeated 5 times
      repeats = 4,
      adaptive = list(
        min = 3,
        alpha = 0.01,
        method = "gls",
        complete = FALSE
      )
    )

    random_rf <- train(
      y = Yobs,
      x = Xobs,
      method = ridgeRF,
      metric = "RMSE",
      tuneLength = tune_length,
      trControl = fitControl
    )

    # Save Tuning parameters ---------------------------------------------------
    dir.create("replicationCode/tuningParam/", showWarnings = FALSE)
    saveRDS(
      object = list(random_rf),
      file = paste0("replicationCode/tuningParam/RidgeForest", note, ".RDS")
    )

    return(list("random_rf" = random_rf$finalModel))
  } else {
    fit <- forestry(x = Xobs,
                    y = Yobs,
                    linear = TRUE,
                    ntree = paramList$ntree,
                    sample.fraction = paramList$sample.fraction,
                    nodesizeSpl = 1,
                    nodesizeAvg = 1,
                    nodesizeStrictAvg = 1,
                    nthread = 1,
                    nodesizeStrictSpl = paramList$nodesizeStrictSpl,
                    mtry = paramList$mtry,
                    overfitPenalty = paramList$overfitPenalty,
                    saveable = FALSE,
                    minSplitGain = paramList$minSplitGain)

    return(list("random_rf" = fit))
  }

}

# Tune Ridge Tree --------------------------------------------------------------
estimator_grid[["caretRidgeTree"]] <- function(Xobs,
                                               Yobs,
                                               tune_length = 200,
                                               cv_fold = 8,
                                               note = NA,
                                               paramList = NA) {
  library(Rforestry)
  library(caret)

  if (is.na(paramList[[1]])) {
    ridgeRF <- list(
      type = "Regression",
      library = "forestry",
      loop = NULL,
      parameters = data.frame(
        parameter = c(
          "mtry",
          "nodesizeStrictSpl",
          "overfitPenalty",
          "minSplitGain",
          "ntree"
        ),
        class = rep("numeric", 5),
        label = c(
          "mtry",
          "nodesizeStrictSpl",
          "overfitPenalty",
          "minSplitGain",
          "ntree"
        )
      ),
      grid = function(x, y, len = NULL, search = "random") {
        ## Define ranges for the parameters and
        ## generate random values for them

        paramGrid <-
          data.frame(
            mtry = sample(1:ncol(x), size = len, replace = TRUE),
            nodesizeStrictSpl = create_random_node_sizes(nobs = nrow(x),
                                                         len = len),
            # Might want to pass specific range/distribution for lambdas
            overfitPenalty = exp(runif(
              len,
              min = log(.1),
              max = log(10)
            )),
            minSplitGain = runif(len, 0, .5) ^
              4,
            ntree = 1
          )
        return(paramGrid)
      },
      fit = function(x,
                     y,
                     wts,
                     param,
                     lev = NULL,
                     last,
                     weights,
                     classProbs) {
        print(param)

        forestry(
          x = x,
          y = y,
          replace = TRUE,
          sample.fraction = 1,
          linear = TRUE,
          ntree = param$ntree,
          nodesizeSpl = 1,
          nodesizeAvg = 1,
          nodesizeStrictAvg = 1,
          nthread = 1,
          nodesizeStrictSpl = param$nodesizeStrictSpl,
          mtry = param$mtry,
          overfitPenalty = param$overfitPenalty,
          saveable = FALSE,
          minSplitGain = param$minSplitGain
        )
      },
      predict = function(modelFit,
                         newdata,
                         preProc = NULL,
                         submodels = NULL) {
        predict(modelFit, newdata)
      },
      prob = NULL
    )

    fitControl <- trainControl(
      method = "adaptive_cv",
      ## 8-fold CV
      number = cv_fold,
      ## repeated 5 times
      repeats = 4,
      adaptive = list(
        min = 3,
        alpha = 0.01,
        method = "gls",
        complete = FALSE
      )
    )

    random_rf <- train(
      y = Yobs,
      x = Xobs,
      method = ridgeRF,
      metric = "RMSE",
      tuneLength = tune_length,
      trControl = fitControl
    )

    # Save Tuning parameters ---------------------------------------------------
    dir.create("replicationCode/tuningParam/", showWarnings = FALSE)
    saveRDS(
      object = list(random_rf),
      file = paste0("replicationCode/tuningParam/RidgeTree", note, ".RDS")
    )


    return(list("random_rf" = random_rf$finalModel))
  } else {
    fit <- forestry(x = Xobs,
                    y = Yobs,
                    replace = TRUE,
                    sample.fraction = 1,
                    linear = TRUE,
                    ntree = paramList$ntree,
                    nodesizeSpl = 1,
                    nodesizeAvg = 1,
                    nodesizeStrictAvg = 1,
                    nthread = 1,
                    nodesizeStrictSpl = paramList$nodesizeStrictSpl,
                    mtry = paramList$mtry,
                    overfitPenalty = paramList$overfitPenalty,
                    saveable = FALSE,
                    minSplitGain = paramList$minSplitGain)

    return(list("random_rf" = fit))
  }
}

# Tune ranger --------------------------------------------------------------
estimator_grid[["ranger"]] <- function(Xobs,
                                       Yobs,
                                       tune_length = 200,
                                       cv_fold = 8,
                                       note = NA,
                                       paramList = NA) {
  library(ranger)
  library(caret)

  if (is.na(paramList[[1]])) {
    rangerRF <- list(
      type = "Regression",
      library = "ranger",
      loop = NULL,
      parameters = data.frame(
        parameter = c(
          "mtry",
          "nodesizeStrictSpl",
          "ntree",
          "sample.fraction"
        ),
        class = rep("numeric", 4),
        label = c(
          "mtry",
          "nodesizeStrictSpl",
          "ntree",
          "sample.fraction"
        )
      ),
      grid = function(x, y, len = NULL, search = "random") {
        ## Define ranges for the parameters and
        ## generate random values for them

        paramGrid <-
          data.frame(
            mtry = sample(1:ncol(x), size = len, replace = TRUE),
            nodesizeStrictSpl = create_random_node_sizes(nobs = nrow(x),
                                                         len = len),
            ntree = 500,
            sample.fraction = runif(len, 0.5, 1))
        return(paramGrid)
      },
      fit = function(x,
                     y,
                     wts,
                     param,
                     lev = NULL,
                     last,
                     weights,
                     classProbs) {

        print(param)

        ranger(y ~ .,
               data = data.frame(x, y),
               num.trees = param$ntree,
               sample.fraction = param$sample.fraction,
               min.node.size = param$nodesizeStrictSpl,
               mtry = param$mtry,
               num.threads = 1
        )
      },
      predict = function(modelFit,
                         newdata,
                         preProc = NULL,
                         submodels = NULL) {

        predict(modelFit, newdata)$predictions
      },
      prob = NULL
    )

    fitControl <- trainControl(
      method = "adaptive_cv",
      ## 8-fold CV
      number = cv_fold,
      ## repeated 5 times
      repeats = 4,
      adaptive = list(
        min = 3,
        alpha = 0.01,
        method = "gls",
        complete = FALSE
      )
    )

    random_rf <- train(
      y = Yobs,
      x = Xobs,
      method = rangerRF,
      metric = "RMSE",
      tuneLength = tune_length,
      trControl = fitControl
    )

    # Save Tuning parameters ---------------------------------------------------
    dir.create("replicationCode/tuningParam/", showWarnings = FALSE)
    saveRDS(
      object = list(random_rf),
      file = paste0("replicationCode/tuningParam/rangerForest", note, ".RDS")
    )

    return(list("random_rf" = random_rf$finalModel))

  } else {
    fit <- ranger(Yobs ~ .,
                  data = data.frame(Xobs, Yobs),
                  num.trees = paramList$ntree,
                  sample.fraction = paramList$sample.fraction,
                  min.node.size = paramList$nodesizeStrictSpl,
                  mtry = paramList$mtry,
                  num.threads = 1)

    return(list("random_rf" = fit))
  }

}

# Tuning glmnet ----------------------------------------------------------------
estimator_grid[["glmnet"]] <- function(Xobs,
                                       Yobs,
                                       tune_length = 200,
                                       cv_fold = 8,
                                       note = NA,
                                       paramList = NA) {
  library(glmnet)
  library(caret)

  encoder <- onehot::onehot(Xobs)
  Xobs <- predict(encoder, Xobs)

  if (is.na(paramList[[1]])) {

    glmnet_fit <- list(
      type = "Regression",
      library = "glmnet",
      loop = NULL,
      parameters = data.frame(
        parameter = c(
          "alpha",
          "lambda"
        ),
        class = rep("numeric", 2),
        label = c(
          "alpha",
          "lambda"
        )
      ),
      grid = function(x, y, len = NULL, search = "random") {
        ## Define ranges for the parameters and
        ## generate random values for them

        paramGrid <-
          data.frame(
            alpha = runif(len, 0, 1),
            lambda = exp(runif(len, min = log(.1), max = log(10)))
          )
        return(paramGrid)
      },
      fit = function(x,
                     y,
                     wts,
                     param,
                     lev = NULL,
                     last,
                     weights,
                     classProbs) {
        print(param)

        glmnet(x = Xobs,
               y = Yobs,
               lambda = param$lambda,
               alpha = param$alpha)

      },
      predict = function(modelFit,
                         newdata,
                         preProc = NULL,
                         submodels = NULL) {
        predict(modelFit, newdata)
      },
      prob = NULL
    )

    fitControl <- trainControl(
      method = "adaptive_cv",
      ## 5-fold CV
      number = cv_fold,
      ## repeated 5 times
      repeats = 4,
      adaptive = list(
        min = 3,
        alpha = 0.01,
        method = "gls",
        complete = FALSE
      ),
      search = "random"
    )

    tuned_glmnet <- train(
      y = Yobs,
      x = Xobs,
      method = glmnet_fit,
      metric = "RMSE",
      tuneLength = tune_length,
      trControl = fitControl
    )

    # Save Tuning parameters -----------------------------------------------------
    dir.create("replicationCode/tuningParam/", showWarnings = FALSE)
    saveRDS(
      object = list(tuned_glmnet),
      file = paste0("replicationCode/tuningParam/glmnet", note, ".RDS")
    )

    return(list(model = tuned_glmnet$finalModel, encoder = encoder))

  } else {

    fit <- glmnet(x = Xobs,
                  y = Yobs,
                  lambda = paramList$lambda,
                  alpha = paramList$alpha)

    return(list(model = fit, encoder = encoder))
  }

}



# Tuning gbm -------------------------------------------------------------------
estimator_grid[["gbm"]] <- function(Xobs,
                                    Yobs,
                                    tune_length = 200,
                                    cv_fold = 8,
                                    note = NA,
                                    paramList = NA) {
  library(caret)
  library(gbm)


  if (is.na(paramList[[1]])) {
    gbm_fit <- list(
      type = "Regression",
      library = "gbm",
      loop = NULL,
      parameters = data.frame(
        parameter = c(
          "n.trees",
          "interaction.depth",
          "shrinkage",
          "n.minobsinnode"
        ),
        class = rep("numeric", 4),
        label = c(
          "n.trees",
          "interaction.depth",
          "shrinkage",
          "n.minobsinnode"
        )
      ),
      grid = function(x, y, len = NULL, search = "random") {
        ## Define ranges for the parameters and
        ## generate random values for them

        paramGrid <-
          data.frame(
            n.trees = sample(1:100, size = len, replace = TRUE),
            interaction.depth = sample(90:100, size = len, replace = TRUE),
            shrinkage = sample(0:9, size = len, replace = TRUE),
            n.minobsinnode = sample(5:10, size = len, replace = TRUE))
        return(paramGrid)
      },
      fit = function(x,
                     y,
                     wts,
                     param,
                     lev = NULL,
                     last,
                     weights,
                     classProbs) {
        print(param)

        e <- gbm(y ~ .,
                 data = data.frame(x, y),
                 distribution = "gaussian",
                 n.trees = param$n.trees,
                 interaction.depth = param$interaction.depth,
                 shrinkage = param$shrinkage,
                 n.minobsinnode = param$n.minobsinnode
                 )
        e
      },
      predict = function(modelFit,
                         newdata,
                         preProc = NULL,
                         submodels = NULL) {
        #browser()
        predict(modelFit, newdata)
      },
      prob = NULL
    )

    fitControl <- trainControl(
      method = "adaptive_cv",
      ## 5-fold CV
      number = cv_fold,
      ## repeated 5 times
      repeats = 4,
      adaptive = list(
        min = 3,
        alpha = 0.01,
        method = "gls",
        complete = FALSE
      ),
      search = "random"
    )

    tuned_gbm <- train(
      y = Yobs,
      x = Xobs,
      method = gbm_fit,
      metric = "RMSE",
      tuneLength = tune_length,
      trControl = fitControl
    )

    # Save Tuning parameters ---------------------------------------------------
    dir.create("replicationCode/tuningParam/", showWarnings = FALSE)
    saveRDS(
      object = list(tuned_cubist),
      file = paste0("replicationCode/tuningParam/gbm", note, ".RDS")
    )

    return(list(model = tuned_gbm$finalModel))

  } else {
    # If we have saved hyperparameters, use those instead
    e_fit <- gbm(y ~ .,
                 data = data.frame(x, y),
                 distribution = "gaussian",
                 n.trees = paramList$n.trees,
                 interaction.depth = paramList$interaction.depth,
                 shrinkage = paramList$shrinkage,
                 n.minobsinnode = paramList$n.minobsinnode)

    return(list(model = e_fit))
  }
}

# Tuning cubist ----------------------------------------------------------------
estimator_grid[["cubist"]] <- function(Xobs,
                                       Yobs,
                                       tune_length = 200,
                                       cv_fold = 8,
                                       note = NA,
                                       paramList = NA) {
  library(Cubist)
  library(caret)

  encoder <- onehot::onehot(Xobs)
  Xobs <- predict(encoder, Xobs)

  if (is.na(paramList[[1]])) {
    cubist_fit <- list(
      type = "Regression",
      library = "Cubist",
      loop = NULL,
      parameters = data.frame(
        parameter = c(
          "committees",
          "extrapolation",
          "neighbors"
        ),
        class = rep("numeric", 3),
        label = c(
          "committees",
          "extrapolation",
          "neighbors"
        )
      ),
      grid = function(x, y, len = NULL, search = "random") {
        ## Define ranges for the parameters and
        ## generate random values for them

        paramGrid <-
          data.frame(
            committees = sample(1:100, size = len, replace = TRUE),
            extrapolation = sample(90:100, size = len, replace = TRUE),
            neighbors = sample(0:9, size = len, replace = TRUE))
        return(paramGrid)
      },
      fit = function(x,
                     y,
                     wts,
                     param,
                     lev = NULL,
                     last,
                     weights,
                     classProbs) {
        print(param)

        c <- cubist(x = Xobs,
                    y = Yobs,
                    committees = param$committees,
                    control = cubistControl(extrapolation = param$extrapolation))
        c$tuneValues$neighbors <- param$neighbors
        c
      },
      predict = function(modelFit,
                         newdata,
                         preProc = NULL,
                         submodels = NULL) {
        #browser()
        predict(modelFit, newdata, neighbors = modelFit$tuneValues$neighbors)
      },
      prob = NULL
    )

    fitControl <- trainControl(
      method = "adaptive_cv",
      ## 5-fold CV
      number = cv_fold,
      ## repeated 5 times
      repeats = 4,
      adaptive = list(
        min = 3,
        alpha = 0.01,
        method = "gls",
        complete = FALSE
      ),
      search = "random"
    )

    tuned_cubist <- train(
      y = Yobs,
      x = Xobs,
      method = cubist_fit,
      metric = "RMSE",
      tuneLength = tune_length,
      trControl = fitControl
    )

    # Save Tuning parameters ---------------------------------------------------
    dir.create("replicationCode/tuningParam/", showWarnings = FALSE)
    saveRDS(
      object = list(tuned_cubist),
      file = paste0("replicationCode/tuningParam/cubist", note, ".RDS")
    )

    return(list(model = tuned_cubist$finalModel, encoder = encoder))

  } else {
    c_fit <- cubist(x = Xobs,
                    y = Yobs,
                    committees = paramList$committees,
                    control = cubistControl(extrapolation = paramList$extrapolation))
    c_fit$tuneValues$neighbors <- paramList$neighbors
    return(list(model = c_fit, encoder = encoder))
  }
}

# Tuning localRF ---------------------------------------------------------------
estimator_grid[["local_RF"]] <- function(Xobs,
                                         Yobs,
                                         tune_length = 200,
                                         cv_fold = 8,
                                         note = NA,
                                         paramList = NA) {
  library(grf)
  library(caret)
  library(onehot)
  encoder <- onehot(Xobs)
  Xobs <- predict(encoder, Xobs)
  minYobs <- min(Yobs)

  if (is.na(paramList[[1]])) {
    # llf <- local_linear_forest(
    #   X = predict(encoder, Xobs),
    #   Y = Yobs,
    #   num.trees = 500,
    #   num.threads = 1
    # )
    # return(list(encoder, llf))
    local_RF <- list(
      type = "Regression",
      library = "grf",
      loop = NULL,
      parameters = data.frame(
        parameter = c(
          "mtry",
          "min.node.size",
          "num.trees",
          "sample.fraction"
        ),
        class = rep("numeric", 4),
        label = c(
          "mtry",
          "min.node.size	",
          "num.trees",
          "sample.fraction"
        )
      ),
      grid = function(x, y, len = NULL, search = "random") {
        ## Define ranges for the parameters and
        ## generate random values for them

        paramGrid <-
          data.frame(
            mtry = sample(1:ncol(x), size = len, replace = TRUE),
            min.node.size	 = create_random_node_sizes(nobs = nrow(x),
                                                      len = len),
            num.trees = 500,
            sample.fraction = runif(len, 0.5, 1))
        print(paramGrid)
        return(paramGrid)
      },
      fit = function(x,
                     y,
                     wts,
                     param,
                     lev = NULL,
                     last,
                     weights,
                     classProbs) {
        print(param)
        mod <- NULL
        tryCatch({
          mod <- ll_regression_forest(
            X = x,
            Y = y,
            enable.ll.split = TRUE,
            num.trees = param$num.trees,
            sample.fraction = param$sample.fraction,
            num.threads = 1,
            min.node.size	 = param$min.node.size	,
            mtry = param$mtry)
        })
        return(mod)
      },
      predict = function(modelFit,
                         newdata,
                         preProc = NULL,
                         submodels = NULL) {
        predict(modelFit, newdata)$predictions

      },
      prob = NULL
    )


    fitControl <- trainControl(
      method = "adaptive_cv",
      ## 8-fold CV
      number = cv_fold,
      ## repeated 5 times
      repeats = 4,
      adaptive = list(
        min = 3,
        alpha = 0.01,
        method = "gls",
        complete = FALSE
      )
    )

    local_rf <- train(
      y = Yobs,
      x = Xobs,
      method = local_RF,
      metric = "RMSE",
      tuneLength = tune_length,
      trControl = fitControl
    )

    # Save Tuning parameters ---------------------------------------------------
    dir.create("replicationCode/tuningParam/", showWarnings = FALSE)
    saveRDS(
      object = list(local_rf),
      file = paste0("replicationCode/tuningParam/local_rf", note, ".RDS")
    )

    return(list("random_rf" = local_rf$finalModel, encoder))
  } else {

    mod <- ll_regression_forest(
      X = Xobs,
      Y = Yobs,
      enable.ll.split = TRUE,
      num.trees = paramList$num.trees,
      sample.fraction = paramList$sample.fraction,
      num.threads = 1,
      ci.group.size = 1,
      min.node.size	 = paramList$min.node.size	,
      mtry = paramList$mtry)

    return(list("random_rf" = mod, encoder))
  }

}


# Tuning BART ----------------------------------------------------------------
estimator_grid[["BART"]] <- function(Xobs,
                                     Yobs,
                                     tune_length = 4,
                                     cv_fold = 8,
                                     note = NA) {

  library(dbarts)
  library(caret)


  bartFit <- list(
    type = "Regression",
    library = "dbarts",
    loop = NULL,
    parameters = data.frame(
      parameter = c(
        "base",
        "power",
        "ntree",
        "sigdf"
      ),
      class = rep("numeric", 4),
      label = c(
        "base",
        "power",
        "ntree",
        "sigdf"
      )
    ),
    grid = function(x, y, len = NULL, search = "random") {
      ## Define ranges for the parameters and
      ## generate random values for them

      paramGrid <-
        data.frame(
          base = runif(len, min = .75, max = .98),
          power = runif(len, min = 3.0, max = 8.0),
          ntree = sample(100:400, size = len, replace = TRUE),
          sigdf = runif(len, min = 2.0, max = 5.0))
      return(paramGrid)
    },
    fit = function(x,
                   y,
                   wts,
                   param,
                   lev = NULL,
                   last,
                   weights,
                   classProbs) {
      print(param)
      #browser()

      e <- bart(x.train = Xobs,
                y.train = Yobs,
                keeptrees = TRUE,
                ntree = param$ntree,
                base = param$base,
                power = param$power,
                sigdf = param$sigdf,
                verbose = FALSE)

      e

    },
    predict = function(modelFit,
                       newdata,
                       preProc = NULL,
                       submodels = NULL) {
      apply(predict(modelFit, newdata), 2, mean)
    },
    prob = NULL
  )

  fitControl <- trainControl(
    method = "adaptive_cv",
    ## 8-fold CV
    number = cv_fold,
    ## repeated 5 times
    repeats = 4,
    adaptive = list(
      min = 3,
      alpha = 0.01,
      method = "gls",
      complete = FALSE
    )
  )

  tuned_bart <- train(
    y = Yobs,
    x = Xobs,
    method = bartFit,
    metric = "RMSE",
    tuneLength = tune_length,
    trControl = fitControl
  )

  # Save Tuning parameters ---------------------------------------------------
  dir.create("replicationCode/tuningParam/", showWarnings = FALSE)
  #saveRDS(
  #  object = list(tuned_bart),
  #  file = paste0("replicationCode/tuningParam/bart", note, ".RDS")
  #)

  return(list("tuned_bart" = tuned_bart$finalModel))
}






predictor_grid <- list(
  "forestryRF" = function(estimator, feat) {
    return(predict(estimator, feat)$random_rf)
  },

  "caretRidgeRF" = function(estimator, feat) {
    return(predict(estimator, feat)$random_rf)
  },

  "caretRidgeRF_nonstrict" = function(estimator, feat) {
    return(predict(estimator, feat)$random_rf)
  },

  "caretRidgeTree" = function(estimator, feat) {
    return(predict(estimator, feat)$random_rf)
  },

  "forestry" = function(estimator, feat) {
    return(predict(estimator, feat))
  },


  "ranger" = function(estimator, feat) {
    return(predict(estimator[[1]], feat)$predictions)
  },


  "glmnet" = function(estimator, feat) {
    return(predict(estimator[[1]],
                   newx = predict(estimator[[2]], feat)) %>% as.numeric)
  },


  "local_RF" = function(estimator, feat) {
    return(predict(estimator[[1]],
                   newdata = predict(estimator[[2]], feat))[, 1])
  },


  "cubist" = function(estimator, feat) {
    return(predict(estimator[[1]],
                   newdata = predict(estimator[[2]], feat)) %>% as.numeric)
  },

  "gbm" = function(estimator, feat) {
    return(predict(estimator[[1]],
                   newdata = feat)
  },

  "BART" = function(estimator, feat) {
    s <- predict(estimator, feat)
    return(apply(s$tuned_bart, 2, mean))
  },

  "ridgeRFStepLinear"= function(estimator, feat) {
    return(predict(estimator, feat)$random_rf)
  }
)

