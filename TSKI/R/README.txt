Requirements:
randomForest
MASS
glumness
here
knockoff
mnorm
matrixcalc
pracma
cvCovEst
aTSA

To start with this TSKI pacakge:

Use requirement.R to install and import all needed packages.

Double click the R package TSKI.Rproj to start the R session in the RStudio.

####################

LCD_MDA.r include main functions for LCD and MDA, as well as some auxiliary functions such as eBH method.

isee_all.R is extracted from the package developed by Fan, Y., & Lv, J. (2016). "Innovated scalable efficient estimation in ultra-large Gaussian graphical models" for estimating the high-dimensional precision matrix. THe precision matrix is the inverse of the sample covariance matrix. See Fan, Y., & Lv, J. for details.

data_generator.R generates three types of time seires data from the paper Chi, C. M., Fan, Y., & Lv, J. (2021). "High-dimensional knockoffs inference for time series data."

section_4_2_LCD_MDA.R is used for the results of LCD and MDA in Section 4.2

section_4_2_adaLasso_LSBY.R is used for the results of adaLasso and LS-BY in Section 4.2
