# rm(list = ls())
library(randomForest)
library(MASS)
library(glmnet)

# ---- LCD ----
#' LCD.calculator
#'
#' @description
#' Calculate and return a numeric vector p knockoff statistics for
#' each input variable based on the Lasso Coefficient Distance.
#'
#' @param X a n by p design matrix
#' @param y a response vector of length n
#' @param lambda.input the lasso lambda parameter
#'
#' @return a numeric vector p feature statistics
#' @export
#'
#' @examples
#' library(here)
#' source(here("R", "isee_all.R"))
#' n <- 100
#' p <- 50
#' X <- array(rnorm(n * p), c(n, p))
#' y <- X[, 1] + rnorm(n)
#' obj <- isee(X, "log", 5, 0, 0)
#' Omega <- obj$Omega.is
#' X_1 <- gknockoffX(X, Omega)
#' LCD.calculator(X_1, y)
LCD.calculator <- function(X, y, lambda.input = -2){

  lambda_seq <- 10^seq(2, lambda.input, by = -.2)

  cv_output <- cv.glmnet(X, y,
                         alpha = 1, lambda = lambda_seq)
  best_lam <- cv_output$lambda.min

  lasso_best_model <- glmnet(X, y, alpha = 1, lambda = best_lam, intercept = TRUE)
  lasso_best <- lasso_best_model$beta

  p <- DIM(X)[2] / 2

  feature_statistics <- c()
  for (i in 1:p) {
    feature_statistics <- c(feature_statistics, abs(lasso_best[i]) - abs(lasso_best[i + p]))
  }
  feature_statistics
}


# ---- ALCD ----
#' ALCD.calculator
#'
#' @description
#' Calculate and return a numeric vector p knockoff statistics for
#' each input variable based on the Adaptive Lasso Coefficient Distance.
#'
#' @param X a n by p design matrix
#' @param y a response vector of length n
#' @param lambda.input the lasso lambda parameter
#'
#' @return
#' @export p feature statistics (not used in the paper)
#'
#' @examples
#' library(here)
#' source(here("R", "isee_all.R"))
#' n <- 100
#' p <- 50
#' X <- array(rnorm(n * p), c(n, p))
#' y <- X[, 1] + rnorm(n)
#' obj <- isee(X, "log", 5, 0, 0)
#' Omega <- obj$Omega.is
#' X_1 <- gknockoffX(X, Omega)
#' ALCD.calculator(X_1, y)
ALCD.calculator <- function(X, y, lambda.input = -2){

  lambda_seq <- 10^seq(2, lambda.input, by = -.2)

  cv_output <- cv.glmnet(X, y,
                         alpha = 1, lambda = lambda_seq)
  best_lam <- cv_output$lambda.min

  lasso_best_model <- glmnet(X, y, alpha = 1, lambda = best_lam, intercept = TRUE)


  w  <- abs(as.numeric(lasso_best_model$beta))
  if (sum(w)!=0) {
    x2 <- scale(X, center=FALSE, scale=1/w)

    # fit adaptive lasso
    lasso.fit <- cv.glmnet(x2, y, alpha = 1, standardize = FALSE, nfolds = 10)
    adalasso_best <- stats::predict(lasso.fit, x2, type="coefficients", s="lambda.min")[-1]
  } else {
    # adalasso coefficients are all zero if lasso coefficients are all zero
    adalasso_best <- lasso_best_model$beta
  }


  p <- DIM(X)[2] / 2

  feature_statistics <- c()
  for (i in 1:p) {
    feature_statistics <- c(feature_statistics, abs(adalasso_best[i]) - abs(adalasso_best[i + p]))
  }
  feature_statistics
}



# ---- MDA ----
#' MDA.calculator
#'
#' @description
#' Calculate and return a numeric vector p knockoff statistics for
#' each input variable based on the Random Forests MDA.
#'
#' @param XF a n by 2p augmented design matrix with knokcoffs
#' @param y a response vector of length n
#' @param len_ p
#'
#' @return p feature statistics
#' @export
#'
#' @examples
#' library(here)
#' source(here("R", "isee_all.R"))
#' n <- 100
#' p <- 50
#' X <- array(rnorm(n * p), c(n, p))
#' y <- X[, 1] + rnorm(n)
#' obj <- isee(X, "log", 5, 0, 0)
#' Omega <- obj$Omega.is
#' X_1 <- gknockoffX(X, Omega)
#' MDA.calculator(X_1, y, len_ = p)
MDA.calculator <- function(XF, y, len_) {
  statistics <- array(0, len_)
  # RF.model <- randomForest(XF, y, nodesize = 20, mtry = floor(len_ * 0.7), importance = TRUE)
  RF.model <- randomForest(XF, y, importance = TRUE)

  for (j in 1:len_) {
    Xf1 <- XF
    Xf1[, j] <- XF[, j + len_]
    Xf2 <- XF
    Xf2[, j + len_] <- XF[, j]
    statistics[j] <- sum((stats::predict(RF.model, newdata = Xf1) - y)^2) -
      sum((stats::predict(RF.model, newdata = Xf2) - y)^2)

  }
  statistics

}


# ---- Knockoff filter threshold ----
#' threshold.finding
#'
#' @description
#' Calculate the knockoff+ threshold based on the input knockoffs statistics.
#'
#' @param w.vector p knockoffs statistics
#' @param tau Target FDR level
#'
#' @return return a knockoff filter threshold
#' @export
#'
#' @examples
threshold.finding <- function(w.vector, tau) {
  ordered.w.vector <- sort(abs(w.vector), decreasing = TRUE)
  t.vector <- c()
  for (t in ordered.w.vector) {
    num1 <- sum(w.vector >= t)
    num2 <- sum(w.vector <= -t)

    if (((1 + num2) / num1) <= tau) {t.vector <- c(t.vector, t)} # knockoff+

  }
  threshold <- min(t.vector)
  return(threshold)
}



#' e.value.calculator
#'
#' @description
#' Calculate the e-values based on the splitted samples, following
#' the method given in the paper.
#'
#' @param subsamples q subsample (X, y) for making the infernece
#' @param tau1 Target FDR level divided by the number of subsamples (q)
#' @param len_ p
#' @param method 'LCD' or 'MDA'
#'
#' @return a numeric p e-values
#' @export
#'
#' @examples
e.value.calculator <- function(subsamples, tau1, len_, method = 'LCD') {
  e.value.final.vector <- array(0, len_)
  for (S in subsamples) {
    X_temp <- S[[1]]
    y_temp <- S[[2]]
    if (method == 'LCD') {
      w.vector <- LCD.calculator(X_temp, y_temp)
    } else if (method == 'ALCD') {
      w.vector <- ALCD.calculator(X_temp, y_temp)
    } else if (method == 'MDA') {
      w.vector <- MDA.calculator(X_temp, y_temp, len_)
    } else {print('Wrong method'); return(NULL)}



    T <- threshold.finding(w.vector, tau1)


    e.value.vector <- array(0, len_)
    for (j in 1:len_) {
      if (w.vector[j] >= T) {
        e.value.vector[j] <- len_ / (1 + sum(w.vector <= -T))
      }
    }
    e.value.final.vector <- e.value.final.vector + e.value.vector

  }
  return(e.value.final.vector / length(subsamples))
}




#' eBH
#'
#' @description
#' The BH method with e-values (instead of p-values) as the input, and
#' output the selected set with controlled FDR.
#'
#' @param evalues e-values of all features
#' @param tau Target FDR level
#'
#' @return threshold of the eBH method
#' @export
#'
#' @examples
eBH <- function(evalues, tau) {
  len_ <- length(evalues)
  sorted.evalues <- sort(evalues, decreasing = TRUE)
  e.threshold <- 0
  for (k in 1:len_) {
    if (sorted.evalues[k] >= (len_ / (tau * k))) {e.threshold <- k}
  }
  return(sorted.evalues[e.threshold])
}

