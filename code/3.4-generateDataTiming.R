library(Rforestry)
library(MASS)

set.seed(3984938)


linear_func <- function(x) {
  return(.4*x[,1] + 2*x[,2] - .9*x[,3] + .25 * x[,4])
}

data_gen <- function(n,p) {
  x <- data.frame(matrix(rnorm(n*p), ncol = p, nrow = n))
  y <- linear_func(x) + rnorm(n)
  data <- data.frame(x,y)
  return(data)
}

# First define a simulation for a range of N values and fixed p ================

datasets_grid <- list()

ranges <- expand.grid(c(5,10,20,40,80,160), c(100))
ranges <- rbind(ranges, expand.grid(c(10), c(200,400,800,1600,3200)))
colnames(ranges) <- c("p","n")

for (i in 1:nrow(ranges)) {

  name <- paste0("p",ranges$p[i],"n",ranges$n[i])

  data <- data_gen(ranges$n[i], ranges$p[i])

  datasets_grid[[name]] <- list(
    "train" = data,
    "test" = data
  )
}


