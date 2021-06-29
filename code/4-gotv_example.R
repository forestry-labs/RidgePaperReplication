# devtools::install_github("soerenkuenzel/causalToolbox")
# devtools::install_github("forestry-labs/Rforestry", ref = "splitting-features")
library(causalToolbox)
library(Rforestry)
# load("datasets/GOTV/GerberGreenLarimer_r1b.RData", verbose = TRUE)
# load("datasets/GOTV/Green_et_al_polanalysis_2009_BW5.RData", verbose = TRUE)
# dim(social)
# head(social)
# summary(social)

dim(gotv)
head(gotv)
# train_u <- 1:100000
train_u <- 1:nrow(gotv)

feat <- gotv[train_u, -(8:9)]
# feat$age2 <- feat$age^2
feat$CVH <- apply(feat[, c("g2000", "g2002", "p2000", "p2002", "p2004")], 1, sum)
feat$CGVH <- apply(feat[, c("g2000", "g2002")], 1, sum)
feat$CPVH <- apply(feat[, c("p2000", "p2002", "p2004")], 1, sum)
feat$agegroup <- as.factor(ifelse(feat$age > 65, "retired", "not retired"))

# feat[,7:8] <- scale(feat[,7:8])

w <- gotv[train_u, 8]
y <- gotv[train_u, 9]

mu0 <- forestry(x = feat[w == 0, ], 
                y = y[w == 0])

D1 <- y[w == 1] - predict(mu0, feature.new = feat[w == 1,])

rft <- forestry(x = feat[w == 1, ],
                y = D1, 
                mtry = ncol(feat),
                # replace = FALSE,
                sample.fraction = 1,
                nodesizeStrictSpl = 3000,
                ntree = 8,
                minSplitGain = .0001,
                ridgeRF = TRUE,
                overfitPenalty = 0.0001,
                linFeats = 9:10,
                middleSplit = TRUE,
                verbose = TRUE)
plot(rft, tree.id = 1)
plot(rft, tree.id = 2)
plot(rft, tree.id = 3)
plot(rft, tree.id = 4)
plot(rft, tree.id = 5)
plot(rft, tree.id = 6)
plot(rft, tree.id = 7)
plot(rft, tree.id = 8)

# Predict only voting propensity -----------------------------------------------
train <- sample(c(TRUE, FALSE), replace = TRUE, nrow(feat))
test <- !train

rft <- forestry(x = feat[train, 1:7],
                y = y[train], 
                mtry = 1,
                # replace = FALSE,
                sample.fraction = 1,
                nodesizeStrictSpl = 3000,
                ntree = 8,
                minSplitGain = .00001,
                ridgeRF = TRUE,
                overfitPenalty = 0.0001,
                linFeats = 2:6,
                middleSplit = TRUE,
                verbose = FALSE, 
                splitFeats = 7)
plot(rft, tree.id = 1)
plot(rft, tree.id = 2)
plot(rft, tree.id = 3)
plot(rft, tree.id = 4)
plot(rft, tree.id = 5)
plot(rft, tree.id = 6)
plot(rft, tree.id = 7)
plot(rft, tree.id = 8)


# Run first X-learner and use the tree to interpret the results ----------------
train_u <- 1:10000
train_t <- 10001:50000

feat <- gotv[, -(8:9)]
feat$age2 <- feat$age^2
feat$CVH <- apply(feat[, c("g2000", "g2002", "p2000", "p2002", "p2004")], 1, sum)
feat$CGVH <- apply(feat[, c("g2000", "g2002")], 1, sum)
feat$CPVH <- apply(feat[, c("p2000", "p2002", "p2004")], 1, sum)
feat[,7:8] <- scale(feat[,7:8])

w <- gotv[, 8]
y <- gotv[, 9]

xRF <- X_RF(feat = feat[train_u, ],
            tr = w[train_u], 
            yobs = y[train_u])

cates <- EstimateCate(xRF, feature_new = feat[train_t, ])

rfts <- forestry(x = feat[train_t, ],
                 y = cates,
                 replace = FALSE,
                 sample.fraction = 1,
                 nodesizeStrictSpl = 1000,
                 ntree = 5,
                 minSplitGain = .02,
                 ridgeRF = TRUE,
                 overfitPenalty = 0.001,
                 linFeats = 2:3,
                 middleSplit = TRUE, 
                 maxDepth = 3)

plot(rfts)
plot(rfts, tree.id = 2)
plot(rfts, tree.id = 3)
plot(rfts, tree.id = 4)
plot(rfts, tree.id = 5)

