# rm(list = ls())
library(MASS)

# ---- ARX ----
regression_ARX <- function(error, base, alpha, X2) {
  # X2 is time series covariates
  indices <- c(1:15)
  
  
  alpha_ <- rep(alpha, length(indices))
  
  
  y <- c(0, 0, 0, 0)
  
  for(i in 1:DIM(X2)[1]) {
    # update times series
    y <- c(y, 0)
    # The first two lags are relevant, and two additional zeros are here for testing.
    y[i + 4] <- y[(i + 3):i] %*% c(base, -base / 2, 0, 0) + c(X2[i, indices] %*% alpha_) + error[i]
  }
  
  y
}


# ---- SETARX ----
regression_SETARX <- function(error, base, alpha, X2) {
  # X2 is time series covariates
  indices <- c(1:15)
  
  alpha_ <- rep(alpha, length(indices))
  r <- 0.7 # threshold
  
  y <- c(0, 0, 0, 0)
  
  for(i in 1:DIM(X2)[1]) {
    # update times series
    y <- c(y, 0)
    if (y[i + 3] > r) {
      y[i + 4] <- y[(i + 3):i] %*% c(base, -base / 2, 0, 0) + c(X2[i, indices] %*% alpha_) + error[i]
    } else {
      y[i + 4] <- y[(i + 3):i] %*% c(-base, base / 2, 0, 0) + c(X2[i, indices] %*% alpha_) + error[i]
    }
  }
  
  y
}

# ---- SETARX2 ----
regression_SETARX2 <- function(error, base, alpha, X2) {
  # X2 is time series covariates
  indices <- c(1:5)
  indices2 <- c(6:15)
  alpha_ <- rep(alpha, length(indices))
  alpha_2 <- rep(alpha, length(indices2))
  r <- 0.7 # threshold
  
  y <- c(0, 0, 0, 0)
  
  for(i in 1:DIM(X2)[1]) {
    # update times series
    y <- c(y, 0)
    if (y[i + 3] > r) {
      y[i + 4] <- y[(i + 3):i] %*% c(base, -base / 2, 0, 0) + c(X2[i, indices] %*% alpha_) + c(X2[i, indices2] %*% alpha_2) + error[i]
    } else {
      y[i + 4] <- y[(i + 3):i] %*% c(-base, base / 2, 0, 0) - c(X2[i, indices] %*% alpha_) + c(X2[i, indices2] %*% alpha_2) + error[i]
    }
  }
  
  y
}



# ---- ARX-ARCH ----
regression_ARXARCH <- function(error, base, alpha, X2) {
  # X2 is time series covarriates
  indices <- c(1:15)
  
  
  alpha_ <- rep(alpha, length(indices))
  
  
  
  gamma_ <- c(0.1, 0.9)
  arch <- c(0) # ARCH(1)
  y <- c(0, 0, 0, 0)
  arch.error <- c()
  for(i in 1:DIM(X2)[1]) {
    # update times seires
    y <- c(y, 0)
    y[i + 4] <- y[(i + 3):i] %*% c(base, -base / 2, 0, 0) + c(X2[i, indices] %*% alpha_) + sqrt(arch[length(arch)]) * error[i]
    arch.error <- c(arch.error, sqrt(arch[length(arch)]) * error[i])
    
    
    arch <- c(arch, 0)
    if (i == 1) {
      arch[i + 1] <- c(1, (rnorm(1))^(2)) %*% gamma_
    } else {
      arch[i + 1] <- c(1, arch[i] * (error[i])^(2)) %*% gamma_
    }
    
  }
  #print(arch.error)
  y
}


data_process <- function(n, serial.rho = 0.2, rho2 = 0.2, base = 0.7, alpha = 0.6, DGP = 'ARX') {
  # ---- generating covariates and time seires ----
  # ---- serial.rho is the gaussian dependency parameter ----
  
  
  # error std is 1
  error <- rnorm(n + 200, sd = 1)
  
  X2 <- time.series.cov(n + 200, serial.rho = serial.rho, rho2 = rho2)
  # AR coefficients are geometrically decay with base 0.5 or 0.7
  if (DGP=='ARX') {
    y <- regression_ARX(error, base, alpha, X2)[-(1:4)]
  } else if (DGP=='SETARX') {
    y <- regression_SETARX(error, base, alpha, X2)[-(1:4)]
  } else if (DGP=='SETARX2') {
    y <- regression_SETARX2(error, base, alpha, X2)[-(1:4)]
  } else if (DGP=='ARXARCH') {
    y <- regression_ARXARCH(error, base, alpha, X2)[-(1:4)]
  }
  y <- y[201:(n + 200)]
  
  X2 <- X2[201:(n + 200),]
  
  
  # l = 20 means that there are 20 lags (t-1 to t-19) in the design
  l <- 20
  design_ <- c()
  for (i in 1:l) {
    design_ <- cbind(design_, y[(l + 1 - i):(length(y) - i)])
  }
  
  
  design <- cbind(design_, X2[(l + 1):length(y), ])
  
  
  response <- y[(l + 1):length(y)]
  
  return(list(design, response))
}



time.series.cov <- function(n, serial.rho = 0.2, rho2 = 0.2) {
  mu <- rep(0, 50)
  Sigma <- array(0, c(50, 50))
  for (i in 1:50) {
    for (j in 1:50) {
      Sigma[i, j] <- (rho2)^(abs(i-j))
    }
  }
  
  e <- mvrnorm(n+5, mu = mu, Sigma = Sigma)
  X.ori <- array(0, c(n+5, 50))
  for (j in 1:50) {
    for (t in 1:(n+5)) {
      if (t <= 4) {
        X.ori[t, j] <- e[t, j]
      } else {
        X.ori[t, j] <- serial.rho * X.ori[t-1, j] + e[t, j]
      }
    }
  }
  
  X2 <- array(0, c(n, 250))
  X2[,1:50] <- X.ori[5:(n+4),]
  X2[,51:100] <- X.ori[4:(n+3),]
  X2[,101:150] <- X.ori[3:(n+2),]
  X2[,151:200] <- X.ori[2:(n+1),]
  X2[,201:250] <- X.ori[1:n,]
  
  
  
  return(X2)
}


DIM <- function(x) {
  if( is.null( dim(x) ) )
    return( length(x) )
  dim(x)
}


# 
# 
# data_ <- data_process(n, serial.rho = serial.rho, rho2 = rho2, base = base, alpha = alpha, DGP = model.dgp)
# X <- data_[[1]]
# y <- data_[[2]]
# 
# 
# plot(X[,1], y, type = 'p', xlab = '', ylab = '', pch = 16)
# 
