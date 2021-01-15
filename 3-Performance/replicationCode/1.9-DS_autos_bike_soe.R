try(setwd("~/Dropbox/ridgeEvaluation/"), silent = TRUE)

library(tidyverse)
library(ggplot2)
library(readr)

set.seed(6318154)

# Import autos -----------------------------------------------------------------
autos <- read_csv("replicationCode/autos.csv")
autos$seller <- as.factor(autos$seller)
autos$abtest <- as.factor(autos$abtest)
autos$gearbox <- as.factor(autos$gearbox)
autos$model <- as.factor(autos$model)
autos$fuelType <- as.factor(autos$fuelType)
autos$notRepairedDamage <- as.factor(autos$notRepairedDamage)

autos %>%
  filter(offerType == "Angebot",
         !is.na(model),
         model != "andere",
         !is.na(vehicleType),
         vehicleType != "andere") %>%
  mutate(postalCode = as.numeric(postalCode),
         Damage = ifelse(is.na(notRepairedDamage),
                         "unknown", notRepairedDamage),
         gearbox = factor(ifelse(is.na(gearbox),
                                 "unknown", as.character(gearbox))),
         fuelType = factor(ifelse(is.na(fuelType),
                                  "unknown", as.character(fuelType))))  %>%
  filter(price < 35000, price > 1000, powerPS < 500, powerPS > 50,
         Damage != "unknown", brand == "volkswagen") %>%
  dplyr::select(-dateCrawled, -name, -offerType, -dateCreated, -lastSeen,
                -notRepairedDamage, -seller, -nrOfPictures, -brand) ->
  cars

carmodels_tbl <- sort(table(cars$model), decreasing = TRUE)
carmodels <- names(carmodels_tbl[carmodels_tbl > 300]); length(carmodels)


cars <- cars %>%
  filter(model %in% carmodels) %>%
  mutate(model = factor(model),
         abtest = factor(abtest),
         vehicleType = factor(vehicleType),
         gearbox = factor(gearbox),
         Damage = factor(Damage))

apply(cars, 2, function(x) sum(is.na(x)))
summary(cars)

dim(cars)

n <- nrow(cars)

test_id <- sort(sample(n, size = round(.97*n))); length(test_id)
train_id <- (1:n)[!(1:n) %in% test_id]; length(train_id)

cars <- as.data.frame(cars)
cars <- cbind(cars[,-1], cars[,1])
colnames(cars)[ncol(cars)] <- "y"
head(cars)

datasets_grid[["autos"]] <- list(
  "train" = cars[train_id, ],
  "test" = cars[test_id, ])

# str(datasets_grid)
# names(datasets_grid)


# Import bike ------------------------------------------------------------------

bike <- read_csv("replicationCode/bike.csv")



bike <- bike %>% 
  dplyr::select(-instant,-dteday,-casual, -registered, -weekday, -season) %>%
  mutate(
    holiday = factor(holiday),
    workingday = factor(workingday),
    weathersit = factor(weathersit)
  )
dim(bike)
summary(bike)


n <- nrow(bike)

test_id <- sort(sample(n, size = round(.95*n))); length(test_id)
train_id <- (1:n)[!(1:n) %in% test_id]; length(train_id)

bike_onehot_translator <- onehot::onehot(bike)
bike <- as.data.frame(predict(bike_onehot_translator, bike))
bike <- as.data.frame(bike)
colnames(bike)[ncol(bike)] <- "y"
head(bike)
colnames(bike) <- gsub("=", "", colnames(bike))
datasets_grid[["bike"]] <- list(
  "train" = bike[train_id, ],
  "test" = bike[test_id, ])

# str(datasets_grid)
# print(names(datasets_grid))
