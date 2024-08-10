# rm(list = ls())
# library(aTSA)
library(here)

source(here("R", "real_data/data_process.R"))

###
###
###
# Produce the unit-root test in Figure 6 of Section A.5

unit.results <- c()
time_ <- c()
for (t in 1:(length (response) - 60 + 1)) {
  T <- t:(t + 60 - 1)
  x.temp <- response[T]
  h <- adf.test(x.temp, nlag = NULL, output = TRUE)
  # When nlag = 1, adf.test output the Dickey-Fuller test.
  unit.results <- c(unit.results, h$type3[2,3]) # the Dickey-Fuller test
  # unit.results <- c(unit.results, h$type3[3,3])
  time_ <- c(time_, date[t + 60 -1])

}



plot(unit.results, pch = 19, cex.axis = 1.5, cex.lab = 1.5, cex = 1, ylim = c(0,0.2), xlab = 'End date of each 5-year window', ylab = '', xaxt = 'n')
axis(1, at = c(1,25,58), labels = time_[c(1,25,58)],  ,cex.axis = 1.5)

# Overall test without rolling windows.
adf.test(response, nlag = NULL, output = TRUE)


