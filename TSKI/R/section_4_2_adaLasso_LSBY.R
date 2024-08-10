# rm(list = ls())
# library('glmnet')
library(here)

source(here("R", "data_generator.R"))
true.beta.index <- c(1,2,21:35) # the  set S_{0} specified in the paper

###
###
### ---- Least Square + Benjamini–Yekutieli (BY) ----
# Three models as stated in the paper
# SETARX2 has more nonlinear covariates than SETAE
# See the paper for details
model.dgp <- 'ARX' # 'ARXARCH', 'SETARX', 'SETARX2'

R <- 100
FDR.BY <- rep(0, R)
PP.BY <- rep(0, R)

n <- 500 # 300, 500
base <- 0.7 # 0.7
alpha <- 0.6 # 0.6
serial.rho <- 0.95 # 0.2, 0.95
rho2 <- 0.2 # 0.2
for (r in 1:R) {
  # Generate the simulated data
  # and fit a least square model
  data_ <- data_process(n, serial.rho = serial.rho, rho2 = rho2, base = base, alpha = alpha, DGP = model.dgp)
  lm.X <- data_[[1]]
  lm.y <- data_[[2]]
  lm.model <- lm(lm.y~., data = data.frame(lm.y, lm.X))
  summary <- summary(lm.model)
  p.vector <- summary$coefficients[2:(length(summary$coefficients[, 1])), 4]

  # ---- evalueation ----
  selected <- which(p.adjust(p.vector, method = 'BY') <= 0.2)
  false.positive <- setdiff(selected, true.beta.index)
  FDR.BY[r] <- length(false.positive) / max(length(selected), 1)

  overlap <- intersect(selected, true.beta.index)
  PP.BY[r] <- length(overlap) / length(true.beta.index)
}
# Print empirical FDR and selection power for LS + BY
mean(FDR.BY)
mean(PP.BY)

###
###
###



###
###
### ---- Adaptive Lasso ----
# three models as stated in the paper
# SETARX2 has more nonlinear covariates
model.dgp <- 'ARX' # 'ARXARCH', 'SETARX', 'SETARX2'
n <- 500 # 200, 300, 500
base <- 0.7 # 0.7
alpha <- 0.6 # 0.6
serial.rho <- 0.2 # 0.2, 0.95
rho2 <- 0.2 # 0.2

R <- 100
FDR.ada <- rep(0, R)
PP.ada <- rep(0, R)
for (r in 1:R) {
  # Generate the simulated data and fit the lasso model
  data_ <- data_process(n, serial.rho = serial.rho, rho2 = rho2, base = base, alpha = alpha, DGP = model.dgp)
  X <- data_[[1]]
  y <- data_[[2]]
  alpha <- 1
  lambda_seq <- 10^seq(2, -2, by = -.2)
  cv_output <- cv.glmnet(X, y,
                         alpha = alpha, lambda = lambda_seq)
  best_lam <- cv_output$lambda.min
  lasso_best_model <- glmnet(X, y, alpha = alpha, lambda = best_lam, intercept = TRUE)

  # calculate weights
  w  <- abs(as.numeric(lasso_best_model$beta))
  if (sum(w)!=0) {
    x2 <- scale(X, center=FALSE, scale=1/w)

    # fit adaptive lasso
    lasso.fit <- cv.glmnet(x2, y, alpha = 1, standardize = FALSE, nfolds = 10)
    beta <- stats::predict(lasso.fit, x2, type="coefficients", s="lambda.min")[-1]
    selected <- which(beta!=0) # weighted estimated coefficients
  } else {
    # select nothing if there are no features selected in the first round
    selected <- c()
  }

  # ---- evalueation ----
  false.positive <- setdiff(selected, true.beta.index)
  FDR.ada[r] <- length(false.positive) / max(length(selected), 1)
  overlap <- intersect(selected, true.beta.index)
  PP.ada[r] <- length(overlap) / length(true.beta.index)
}
# Print empirical FDR and selection power for adaLasso
mean(FDR.ada)
mean(PP.ada)
