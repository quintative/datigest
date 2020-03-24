library(ggplot2) # Vis
library(data.table) # Convenience
library(dplyr) # Convenience
library(MASS) # For comparing with the rlm - method
library(rbenchmark)
library(zoo)

# Data
x <- rnorm(1000)
y <- x + rnorm(1000) * 0.5

# Outlier
x <- append(x, 1.5)
y <- append(y, 1E16)

# Data tabling
dt <- data.table(x = x, y = y)

# Matrix form
X <- cbind(1, matrix(x))

# Calculating weight matrix
w <- sapply(y, FUN = function(xx){return(ifelse(xx < 5, 1, 5/abs(xx)))})

# Creating the data matrix
M <- cbind(X, y)
M <- cbind(M, w)

# Roll-regression
R <- rollapplyr(M, width = 20, by.column = F, partial = 4, fill = NA, FUN = function(xx){
  fx <- xx[, 2]
  fX <- xx[, 1:2]
  fy <- xx[, 3]
  fw <- xx[, 4]
  ss <- solve(t(fX) %*% (fw * fX))
  coef.al.hub <- ss %*% t(fX) %*% (fw * fy)
  sig.al.hub <- sqrt(var(fx * fw) * coef.al.hub[[2]] ** 2 + var(fy * fw) - 2 * coef.al.hub[[2]] * cov(fx * fw, fy * fw))
  se.al.hub <- sig.al.hub * sqrt(ss[[4]])
  
  return(c(coef.al.hub, sig.al.hub, se.al.hub))
})

colnames(R) <- c("interc", "beta", "vol_res", "se_beta")

R <- as.data.table(R)
R

