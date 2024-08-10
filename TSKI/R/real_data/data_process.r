# rm(list = ls())
# data downloaded from https://research.stlouisfed.org/econ/mccracken/fred-databases/
library(here)



DIM <- function(x) {
  if( is.null( dim(x) ) )
    return( length(x) )
  dim(x)
}

file <- here("R", "real_data/current.csv")

data_ <- read.csv(file)
# DIM(data_)
# the first row is the category index.



# record the categories of each column
categories <- data_[1,-1]

# use data from 2013:1 to 2023:1
# in the meantime the first row, which records the cotegories, is not included
# data_[650, 1] # 1/1/2013
data_ <- data_[650:(DIM(data_)[1] - 1),]


# the first column is the date column. Delete it.
date <- data_[, 1]
data_ <- data_[, -1]



# fill up the missing values (NAs)
# Each NA is assigned the same value as the value from the last month.
# it is known that there are no NAs in 2015:1

for (t in 2:DIM(data_)[1]) {
  if (length(which(is.na(data_[t,])))) {
    data_[t, which(is.na(data_[t,]))] <- data_[t-1, which(is.na(data_[t,]))]
  }
}

# which(is.na(data_)) # no NAs
# DIM(data_) # (97, 127)
# length(categories) # 127


cpi.index <- which(colnames(data_) == 'CPIAUCSL') # CPIAUCSL is the CPI for all items
# data_[, cpi.index]
# cpi.index # 106


# ---- data transformation ----
data_organized <- array(0, c(dim(data_)[1] - 2, dim(data_)[2]))

for (j in 1:length(categories)) {
  if (j == cpi.index) {
    # perchange changes of CPI: ((x_{t} - x_{t-1}) / x_{t-1}) * 100

    x <- data_[, j]
    x <- ((x[2:length(x)] / x[1:(length(x) - 1)]) - 1) * 100
    data_organized[, j] <- x[-1]

  } else {
    if (categories[j] == 7) {
      x <- data_[, j]
      x <- (x[2:length(x)] / x[1:(length(x) - 1)]) - 1
      x <- x[2:length(x)] - x[1:(length(x) - 1)]
      data_organized[, j] <- x
    }
    if (categories[j] == 6) {
      x <- log(data_[, j])
      x <- x[2:length(x)] - x[1:(length(x) - 1)]
      x <- x[2:length(x)] - x[1:(length(x) - 1)]
      data_organized[, j] <- x
    }
    if (categories[j] == 2) {
      x <- data_[, j]
      x <- x[2:length(x)]
      x <- x[2:length(x)] - x[1:(length(x) - 1)]
      data_organized[, j] <- x
    }
    if (categories[j] == 3) {
      x <- data_[, j]
      x <- x[2:length(x)] - x[1:(length(x) - 1)]
      x <- x[2:length(x)] - x[1:(length(x) - 1)]
      data_organized[, j] <- x
    }
    if (categories[j] == 4) {
      x <- log(data_[, j])
      data_organized[, j] <- x[c(-1, -2)]
    }
    if (categories[j] == 5) {
      x <- log(data_[, j])
      x <- x[2:length(x)] - x[1:(length(x) - 1)]
      data_organized[, j] <- x[-1]
    }
    if (categories[j] == 1) {
      x <- data_[, j]
      data_organized[, j] <- x[3:length(x)]
    }
  }
}


# record the dates of responses; from 3/1/2015 to 1/1/2023
# removing two period of data due to difference operation in preprocessing the time series
date <- date[c(-1, -2)]

# record the response; from 3/1/2015 to 1/1/2023
response <- data_organized[, cpi.index]



# construct the design
X <- array(0, c(length(response) - 2, DIM(data_organized)[2] * 2))
for (t in 1:2) {
  X[, ((t-1) * DIM(data_organized)[2] + 1):(DIM(data_organized)[2] * t)] <-
    data_organized[(3-t):(length(response) - t),]
}

response <- response[3:length(response)]
# record the dates of responses; from 5/1/2015 to 1/1/2023
date <- date[3:length(date)]


# y, X are ready
# X:  1 response t=1 lag + 126 predictors t = 1 lags  (from index 1 to 127)
#   + 1 response t=2 lag + 126 predictors t = 2 lags  (from index 128 to 254)
# ---- the end of data process ----




# ---- diagnosis ----
# plot(response, type = 'l')
#
# low.point.index <- which(response == min(response))
# response[(low.point.index-5):(low.point.index+5)]
# date[low.point.index]
#
#
# response
# X[, cpi.index]
# X[, cpi.index + DIM(X)[2] / 2]
#
#
# var(response)
# length(response)
#
