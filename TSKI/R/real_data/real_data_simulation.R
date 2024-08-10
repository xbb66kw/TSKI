rm(list = ls())
library(knockoff)
library(randomForest)
library(MASS)
library(here)

source(here("R", "real_data/data_process.R"))
source(here("R", "isee_all.R"))
source(here("R", "LCD_MDA.R"))


# --- Knockoffs generating ----
# X is the time series covariate matrix


# ---- rolling window ----
# date
# response: 5/1/2013 to 1/1/2023

# Tuning parameter:
q <- 2 # q is the q + 1 in the paper

# Fixed parameters:
T <- 60 # five-years rolling windows
R <- 5 # Default R = 100
tau <- 0.2
p.total <- 127 * 2 # 127 predictors (at current time predicting the next time inflation)
# and their one-period lags
counter <- array(0, c(p.total, (length(response) - T + 1)))
frequency <- rep(0, length(response) - T + 1)
counter.R.covariate <- array(0, c(p.total, R))

# --- MDA ---
counter.MDA <- array(0, c(p.total, (length(response) - T + 1)))
frequency.MDA <- rep(0, length(response) - T + 1)
counter.R.covariate.MDA <- array(0, c(p.total, R))
for (r in 1:R) {


  obj = isee(X, "log", 5, 0, 0)
  Omega = obj$Omega.isee
  X_aug <- gknockoffX(X, Omega)

  for (t in 1:(length(response) - T + 1)) {
    y_roll <- response[t:(t+T-1)]
    X_roll <- X_aug[t:(t+T-1), ]
    n <- DIM(X_roll)[1]
    p <- DIM(X_roll)[2]


    # print(t)

    e.value.vector.final <- rep(0, p.total)
    e.value.vector.final.MDA <- rep(0, p.total)
    for (k in 1:q) {
      index.set <- seq(from = k, to = n, by = q)
      # average the evalues to derandommize the randomness from random forests and knockoffs


      X_temp <- X_roll[index.set, ]
      y_temp <- y_roll[index.set]


      w.vector <- LCD.calculator(X_temp, y_temp)
      #######
      # omit the results of MDA
      w.vector.MDA <- rep(0, p.total)
      # w.vector.MDA <- MDA.calculator(X_temp, y_temp, p.total)



      T.1 <- threshold.finding(w.vector, tau / q)


      e.value.vector <- array(0, length(w.vector))
      for (j in 1:length(w.vector)) {
        if (w.vector[j] >= T.1) {
          e.value.vector[j] <- length(w.vector) / (1 + sum(w.vector <= -T.1))
        }
      }
      e.value.vector.final <- e.value.vector.final + e.value.vector

      # --- TSKI-MDA ----
      T.1 <- threshold.finding(w.vector.MDA, tau / q)


      e.value.vector.MDA <- array(0, length(w.vector.MDA))
      for (j in 1:length(w.vector.MDA)) {
        if (w.vector.MDA[j] >= T.1) {
          e.value.vector.MDA[j] <- length(w.vector.MDA) / (1 + sum(w.vector.MDA <= -T.1))
        }
      }
      e.value.vector.final.MDA <- e.value.vector.final.MDA + e.value.vector.MDA
    }


    # ---- TSKI-LCD ----
    e.value.vector.final <- e.value.vector.final / q
    selected.list <- which(e.value.vector.final >= eBH(e.value.vector.final, tau))
    if (length(selected.list)) {frequency[t] <- frequency[t] + 1}

    # counter ++
    counter[selected.list, t] <- counter[selected.list, t] + 1
    counter.R.covariate[selected.list, r] <- 1


    # --- TSKI-MDA ---
    e.value.vector.final.MDA <- e.value.vector.final.MDA / q
    selected.list <- which(e.value.vector.final.MDA >= eBH(e.value.vector.final.MDA, tau))
    if (length(selected.list)) {frequency.MDA[t] <- frequency.MDA[t] + 1}

    # counter ++
    counter.MDA[selected.list, t] <- counter.MDA[selected.list, t] + 1
    counter.R.covariate.MDA[selected.list, r] <- 1


  }
  print(paste0('The ', r, 'th run. ', ' In total ', R, ' rounds'))
}

rowSums(counter.R.covariate) / R # average selection frequency over all rolling windows of each covaraite
frequency
colSums(counter)
order(rowSums(counter))

rowSums(counter.R.covariate.MDA) / R # average selection frequency over all rolling windows of each covaraite
frequency.MDA
colSums(counter.MDA)
order(rowSums(counter.MDA))




###
###
### ---- Save files (commented out to avoid mistakes) ----
# Need obj to use plot.r to plot the figures displayed in the paper
obj <- list()
obj[[1]] <- frequency
obj[[2]] <- counter
obj[[3]] <- counter.R.covariate
obj[[4]] <- frequency.MDA
obj[[5]] <- counter.MDA
obj[[6]] <- counter.R.covariate.MDA


file <- '/Users/xbb/Desktop/paper_tex/TSKI_2023/data_simulation_codes/real_data/results_freq_'
file <- paste0(file, 'LCD_v2_', 'q', q, '.rds')
file
# saveRDS(obj, file = file)
# obj.mid <- readRDS(file)
# obj <- obj.mid

# frequency <- obj.mid[[1]]
# counter <- obj.mid[[2]]
# counter.R.covariate <- obj.mid[[3]]


