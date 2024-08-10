rm(list = ls())

# Package names
packages <- c("randomForest", "MASS", "glmnet", "here", "knockoff", "mnorm", "matrixcalc", "pracma", "cvCovEst", "aTSA")

# Install packages not yet installed
installed_packages <- packages %in% rownames(installed.packages())
if (any(installed_packages == FALSE)) {
  install.packages(packages[!installed_packages])
}

# Packages loading
invisible(lapply(packages, library, character.only = TRUE))


