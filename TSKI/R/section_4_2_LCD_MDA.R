# rm(list = ls())
library(knockoff)
library(randomForest)
library(MASS)
library(here)

source(here("R", "data_generator.R"))
source(here("R", "isee_all.R"))
source(here("R", "LCD_MDA.R"))

# Run the above files
# Run the below file for example based on ARX model

# Three models as stated in the paper
# SETARX2 has more nonlinear covariates than SETARX
# See the paper for details
model.dgp <- 'SETARX' # 'ARXARCH', 'SETARX', 'SETARX2', 'ARX'

n <- 300 # 200, 300, 500, 1000, 2000
base <- 0.7 # 0.7 # beta
alpha <- 0.6 # 0.6 # the coefficient for the exogenous variables
serial.rho <- 0.2 # 0.2, 0.95
q <- 1 # 1, 2, 3 # q here is the (q+1) in the paper
R <- 5 # Repeat the experiments R times. Default is R = 100


# ALCD use adpative Lasso and TSKI
# It's power is often less than LCD
# The other can be found in the paper
e.values.results.saving <- list()
e.values.results.saving[['LCD']] <- list()
e.values.results.saving[['ALCD']] <- list()
e.values.results.saving[['MDA']] <- list()

for (r in 1:R) {
  e.values.results.saving[['LCD']][[r]] <- rep(0, 270)
  e.values.results.saving[['ALCD']][[r]] <- rep(0, 270)
  e.values.results.saving[['MDA']][[r]] <- rep(0, 270)
}


for (r in 1:R) {
  data_ <- data_process(n, serial.rho = serial.rho, base = base, alpha = alpha, DGP = model.dgp)
  X <- data_[[1]]
  y <- data_[[2]]
  p <- DIM(X)[2]

  obj <- isee(X, "log", 5, 0, 0)
  Omega <- obj$Omega.is
  X_1 = gknockoffX(X, Omega)


  subsample.list <- list()
  for (sub in 1:q) {
    indices <- seq(from = sub, to=n-20, by=q)
    subsample.list[[sub]] <- list(X_1[indices,], y[indices])
  }


  e.values.results.LCD <- e.value.calculator(subsample.list, 0.2 / q, p, method = 'LCD')

  e.values.results.saving[['LCD']][[r]] <- e.values.results.LCD

  e.values.results.ALCD <- e.value.calculator(subsample.list, 0.2 / q, p, method = 'ALCD')

  e.values.results.saving[['ALCD']][[r]] <- e.values.results.ALCD

  e.values.results.MDA <- e.value.calculator(subsample.list, 0.2 / q, p, method = 'MDA')

  e.values.results.saving[['MDA']][[r]] <- e.values.results.MDA

  print(r)
}
# system("uname -m")
###
###
### ---- Evaluation ----
# true coefficient
true.beta.index <- c(1, 2, 21:35)  # the  set S_{0} specified in the paper

FDP.total.LCD <- 0
FDP.LCD <- 0
FDP.total.ALCD <- 0
FDP.ALCD <- 0
FDP.total.MDA <- 0
FDP.MDA <- 0
PP.LCD <- 0
PP.ALCD <- 0
PP.MDA <- 0
for (r in 1:R) {
  eBH.threshold <- eBH(e.values.results.saving[['LCD']][[r]], 0.2)
  selected <- which(e.values.results.saving[['LCD']][[r]] >= eBH.threshold)
  false.positive <- setdiff(selected, true.beta.index)
  FDP.LCD <- length(false.positive) / max(length(selected), 1)
  FDP.total.LCD <- FDP.total.LCD + FDP.LCD
  overlap <- intersect(selected, true.beta.index)
  PP.LCD <- PP.LCD + length(overlap) / length(true.beta.index)

  eBH.threshold <- eBH(e.values.results.saving[['ALCD']][[r]], 0.2)
  selected <- which(e.values.results.saving[['ALCD']][[r]] >= eBH.threshold)
  false.positive <- setdiff(selected, true.beta.index)
  FDP.ALCD <- length(false.positive) / max(length(selected), 1)
  FDP.total.ALCD <- FDP.total.ALCD + FDP.ALCD
  overlap <- intersect(selected, true.beta.index)
  PP.ALCD <- PP.ALCD + length(overlap) / length(true.beta.index)

  eBH.threshold <- eBH(e.values.results.saving[['MDA']][[r]], 0.2)
  selected <- which(e.values.results.saving[['MDA']][[r]] >= eBH.threshold)
  false.positive <- setdiff(selected, true.beta.index)
  FDP.MDA <- length(false.positive) / max(length(selected), 1)
  FDP.total.MDA <- FDP.total.MDA + FDP.MDA
  overlap <- intersect(selected, true.beta.index)
  PP.MDA <- PP.MDA + length(overlap) / length(true.beta.index)
}
# Print empirical FDR and selection power for TSKI based approaches
FDR.LCD <- FDP.total.LCD / R
PP.LCD <- PP.LCD / R
print(q)
# FDR for TSKI-LCD
print(FDR.LCD)
# selection power for TSKI-LCD
print(PP.LCD)

FDR.ALCD <- FDP.total.ALCD / R
PP.ALCD <- PP.ALCD / R
print(q)
# FDR for TSKI-ALCD
print(FDR.ALCD)
# selection power for TSKI-ALCD
print(PP.ALCD)

FDR.MDA <- FDP.total.MDA / R
PP.MDA <- PP.MDA / R
# FDR for TSKI-MDA
print(FDR.MDA)
# section power for TSKI-MDA
print(PP.MDA)

###
###
### --- saving data ----
file_ <- "/Users/xbb/Dropbox/Desktop/paper_tex/TSKI_2023/data_simulation_codes/coor_simulation_results/"
file_ <- paste0(file_, 'n', n, 'beta', base, 'q', q, 'serialrho', serial.rho, model.dgp, '.rds')
file_
# saveRDS(e.values.results.saving, file = file_)

# mid_ <- readRDS(file_)
# e.values.results.saving <- mid_

