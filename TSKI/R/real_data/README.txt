LCD_MDA.r include main functions for LCD and MDA, as well as some auxiliary functions such as eBH method.

isee_all.R is extracted from the package developed by Fan, Y., & Lv, J. (2016). "Innovated scalable efficient estimation in ultra-large Gaussian graphical models" for estimating the high-dimensional precision matrix. THe precision matrix is the inverse of the sample covariance matrix. See Fan, Y., & Lv, J. for details.

data_process.R process the economic data downloaded from:
https://research.stlouisfed.org/econ/mccracken/fred-databases/
The data set that we use ends at January 2023.

real_data_simulation.R is the main codes for analyzing the economic data set.

After running real_data_simulation.R, following the instruction (4 steps) in plotting.R to
produce the pictures of interest in the paper.


Data dictionary:
The monthly economic time series, including numerous types of consumer price indices, unemployment rates, and housing prices, 
The rows are time of each month from 1/1/1959. The columns are FRED-MD codes for each time series variable. You may find the detailed codes in McCracken, M. W., & Ng, S. (2016). FRED-MD: A monthly database for macroeconomic research. Journal of Business & Economic Statistics, 34(4), 574-589.