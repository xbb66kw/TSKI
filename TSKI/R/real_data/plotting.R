# rm(list = ls())
library(here)


###
###
###
# --- Run the following chunk of codes if you have saved file ---
# q is the q + 1 in the paper.
# q = 1 or q = 2 (set the value of q by yourself)
# q <- 2
# T <- 60 # five-years rolling windows
# R <- 100

# --- read files ---
# file <- here("R", "real_data/results_freq_")
# file <- paste0(file, 'LCD_v2_', 'q', q, '.rds')
# file
# obj.mid <- readRDS(file)
# obj <- obj.mid
#
# frequency <- obj.mid[[1]]
# counter <- obj.mid[[2]]
# counter.R.covariate <- obj.mid[[3]]


###
###
###
# ---- Getting started ---
# 1. Require: counter, frequency, counter.R.covariate
# from running real_data_simulation.R or loading the saved file.

# 2. Economic time series data data downloaded from https://research.stlouisfed.org/econ/mccracken/fred-databases/
# source("/Users/xbb/Dropbox/Desktop/paper_tex/TSKI_2023/data_simulation_codes/real_data/data_process.r")
# response (inflation): 5/1/2013 to 1/1/2023

# 3. Read the needed functions
# ---- Auxiliary functions ----
#' Standardizing
#'
#' Standardize the data
#'
#' @param x input vector
#'
#' @return a standardized vector
#' @export
#'
#' @examples
standardized <- function(x) {
  x <- x - mean(x)
  x / sqrt(sum(x^2))
}

#' Standardizing
#'
#' Standardize the data
#'
#' @param x input vector
#'
#' @return a standardized vector
#' @export
#'
#' @examples
standardized.unem <- function(x, normalizer) {
  x <- x - mean(x)
  x / sqrt(sum(x^2)) * normalizer
}


# index.lag <- function(x) {
#   v <- x
#   v[x>127] <- x[x>127] - 127
#   v[x==127] <- 1
#   return(unique(v))
# }

# 4. Run each chunk of code below for producing the pictures of interest.
###
###
### ---- Response: inflation (Figure 1 in the paper) ----
plot(response, type='l', lwd=1, ylab = NA, xlab = NA, xaxt = 'n', yaxt='n')
abline(h=0, lty=3)
axis(2, label = NA, at = c(-0.5), lwd = 3, tck = -0.02)
axis(2, label = -0.5, at = -0.5, cex.axis = 2.5,  col = NA)

axis(2, label = NA, at = c(0), lwd = 3, tck = -0.02)
axis(2, label = 0, at = -0, cex.axis = 2.5,  col = NA)

axis(2, label = NA, at = c(1), lwd = 3, tck = -0.02)
axis(2, label = 1, at = 1, cex.axis = 2.5,  col = NA)

axis(1, label = NA, at = 1, lwd = 3, tck = -0.04)
axis(1, label = '5/1/2013', at = 1, cex.axis = 2.5, pos = -0.7, col = NA)

axis(1, label = NA, at = 21, lwd = 3, tck = -0.04)
axis(1, label = '1/1/2015', at = 20, cex.axis = 2.5, pos = -0.95, col = NA)

axis(1, label = NA, at = 84, lwd = 3, tck = -0.04)
axis(1, label = '4/1/2020', at = 80, cex.axis = 2.5, pos = -0.95, col = NA)

axis(1, label = NA, at = 117, lwd = 3, tck = -0.04)
axis(1, label = '1/1/2023', at = 113, cex.axis = 2.5, pos = -0.95, col = NA)


# axis(1, label = 'A', at = 19, cex.axis = 2.5, pos = -0.7, col = NA)
# axis(1, label = 'B', at = 82, cex.axis = 2.5, pos = -0.7, col = NA)
# axis(1, label = 'C', at = 106, cex.axis = 2.5, pos = -0.6, col = NA)








###
### ---- Figure 3 (q=1) and Figure 4 (q=2) ---- # note that q here is the q+1 in the paper
### ---- Selection frequency (panel a) ----
plot(frequency / R, type = 'h', ylim = c(0, 1), ylab = NA, xlab = NA, yaxt = 'n', xaxt = 'n', lwd = 3)
# X-axis
ticks.points <- c(1, 21, 40, (length(response) - T + 1))
axis(1, label = NA, at = ticks.points, cex.axis = 2, tck = -0.04)
axis(1, label = date[60 - 1 + ticks.points], at = ticks.points, cex.axis = 2.5, pos = -0.1, col = NA)
axis(2, at = c(0,1), cex.axis = 2.5)

abline(h=0.1, lty=3)
axis(2, label=NA, at = 0.1, cex.axis = 2.5)
axis(2, label=0.1, at = 0.1, cex.axis = 2.5, col = NA)

# ---- Frequency of important covariates (panel b)----
plot(rowSums(counter.R.covariate) / R, type = 'h', ylim = c(0, 1), ylab = NA, xlab = NA, yaxt = 'n', xaxt = 'n', lwd = 1)
# X-axis
abline(v = 127 + 1, lty=3)
abline(h = 0.5, lty=3)

ticks.points <- c(1, 128, 254)
axis(1, label = NA, at = ticks.points, cex.axis = 2, tck = -0.04)
axis(1, label = c(1, 128, 254), at = ticks.points, cex.axis = 2.5, pos = -0.1, col = NA)
axis(2, at = c(0, 1), cex.axis = 2.5)
axis(2, at = c(1/2), cex.axis = 2.5)







###
###
### --- Temporarily related plotting (Figure 5)----
# ---- ACOGNO panel (a) ----
plot(standardized(X[, 58]), type='l', ylim = c(-0.5, 0.9), lwd=3, ylab = NA, xlab = NA, xaxt = 'n', yaxt = 'n', col = 'red')
points(standardized(response), type = 'l', lwd=5)


abline(v= 84, lty=3)
abline(v= 84 + 1, lty=3)
abline(v= 84 + 2, lty=3)
abline(v= 84 + 3, lty=3)
abline(v= 84 + 4, lty=3)

# date[84] # 4/12020
# date[40] # 8/1/2016
axis(1, label = NA, at = 1, lwd = 6, tck = 0.04)
axis(1, label = '5/1/2013 (Black line)', at = 6, cex.axis = 2, pos = -0.3, col = NA)
axis(1, label = NA, at = 117, lwd = 6, tck = 0.04)
axis(1, label = '1/1/2023 (Black line)', at = 117 - 9, cex.axis = 2, pos = -0.55, col = NA)
axis(1, label = NA, at = 84, lwd = 6, tck = -0.04)
axis(1, label = '4/1/2020 (Black line)', at = 57, cex.axis = 2.5, pos = -0.6, col = NA)
axis(1, label = NA, at = 84 + 1, lwd = 6, tck = 0.04, col = 'red')
axis(1, label = '4/1/2020 (Red line)', at = 70, cex.axis = 2.5, pos = -0.35, col = NA, col.axis ='red')




# ---- EXCAUSx panel (b) ----
plot(standardized(X[, 99]), type='l', ylim = c(-0.5, 0.9), lwd=3, ylab = NA, xlab = NA, xaxt = 'n', yaxt = 'n', col = 'red')
points(standardized(response), type = 'l', lwd=5)


abline(v= 84, lty=3)
abline(v= 84 + 1, lty=3)
abline(v= 84 + 2, lty=3)
abline(v= 84 + 3, lty=3)
abline(v= 84 + 4, lty=3)

# date[84] # 4/12020
# date[40] # 8/1/2016
axis(1, label = NA, at = 1, lwd = 6, tck = 0.04)
axis(1, label = '5/1/2013 (Black line)', at = 6, cex.axis = 2, pos = -0.3, col = NA)
axis(1, label = NA, at = 117, lwd = 6, tck = 0.04)
axis(1, label = '1/1/2023 (Black line)', at = 117 - 9, cex.axis = 2, pos = -0.55, col = NA)
axis(1, label = NA, at = 84, lwd = 6, tck = -0.04)
axis(1, label = '4/1/2020 (Black line)', at = 57, cex.axis = 2.5, pos = -0.6, col = NA)
axis(1, label = NA, at = 84 + 1, lwd = 6, tck = 0.04, col = 'red')
axis(1, label = '4/1/2020 (Red line)', at = 70, cex.axis = 2.5, pos = -0.35, col = NA, col.axis ='red')



# ---- CLAIMx panel (c) ----
plot(standardized(response), type='l', ylim = c(-0.5, 0.9), lwd=5, ylab = NA, xlab = NA, xaxt = 'n', yaxt = 'n')
points(standardized(X[, 31]), type = 'l', lwd=3, col = 'red')

abline(v= 84, lty=3)
abline(v= 84 + 1, lty=3)
abline(v= 84 + 2, lty=3)
abline(v= 84 + 3, lty=3)
abline(v= 84 + 4, lty=3)

# date[84] # 4/12020
# date[40] # 8/1/2016
axis(1, label = NA, at = 1, lwd = 6, tck = 0.04)
axis(1, label = '5/1/2013 (Black line)', at = 6, cex.axis = 2, pos = -0.3, col = NA)
axis(1, label = NA, at = 117, lwd = 6, tck = 0.04)
axis(1, label = '1/1/2023 (Black line)', at = 117 - 9, cex.axis = 2, pos = -0.55, col = NA)
axis(1, label = NA, at = 84, lwd = 6, tck = -0.04)
axis(1, label = '4/1/2020 (Black line)', at = 57, cex.axis = 2.5, pos = -0.6, col = NA)
axis(1, label = NA, at = 84 + 1, lwd = 6, tck = 0.04, col = 'red')
axis(1, label = '4/1/2020 (Red line)', at = 70, cex.axis = 2.5, pos = -0.35, col = NA, col.axis ='red')

