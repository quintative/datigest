library(ggplot2) # Vis
library(data.table) # Convenience
library(dplyr) # Convenience
library(MASS) # For comparing with the rlm - method

# Data
x <- rnorm(100)
y <- x + rnorm(100) * 0.5

# Outlier
x <- append(x, 1.5)
y <- append(y, 1E6)

# Data tabling
dt <- data.table(x = x, y = y)

# Using the lm's
model.lm <- lm(y ~ x)
model.rlm <- rlm(y ~ x)

coef.lm  <- coef(model.lm)
coef.rlm <- coef(model.rlm)

var.lm <- var(resid(model.lm))
var.rlm <- var(resid(model.rlm))

se.lm <- model.lm %>% summary() %>% coef() %>% .[[4]]
se.rlm <- model.rlm %>% summary() %>% coef() %>% .[[4]]

dt %>%
  ggplot(aes(x, y)) +
  geom_point() +
  geom_abline(slope = coef.lm[[2]], intercept = coef.lm[[1]]) +
  geom_abline(slope = coef.rlm[[2]], intercept = coef.rlm[[1]], color = "red") +
  ylim(-4, 4)

# Using lina
# ---------------------------------------------------------------------------------
# Creating matrix
X <- matrix(append(rep(1, length(x)), x), ncol = 2)
# Solving the equation for minimization of the LS problem
coef.al <- solve(t(X) %*% X) %*% t(X) %*% y
# Calculating the variance that way
var.al <- var(x) * coef.al[[2]] ** 2 + var(y) - 2 * coef.al[[2]] * cov(x, y)
# Plotting
# dt %>%
#   ggplot(aes(x, y)) +
#   geom_point() + 
#   geom_abline(slope = coef.al[[2]], intercept = coef.al[[1]]) +
#   ylim(-4, 4)

# Using Huber Loss function for doing this in the lina way
# Calculaitng the weight - matrix
# Propose that I know that an outlier is likely to be something like a 5 sigma event

1/(2*pnorm(-5)) #That's roughly 1 in 1.7 Million observations

dt[y < 10] %>% ggplot(aes(y)) + geom_histogram(bins = 20)

# Here, the z-score is equal to y
# That makes it easy to write the weight-matrix

w <- sapply(y, FUN = function(xx){
  if(xx < 5){
    out <- 1
  } else{
    out <- abs(xx)
  }
  return(5/out)
})

W <- w %>% diag()

coef.al.hub <- solve(t(X) %*% W %*% X) %*% t(X) %*% W %*% y
var.al.hub <- var(x) * coef.al.hub[[2]] ** 2 + var(y * w) - 2 * coef.al.hub[[2]] * cov(x, y * w)
se.al.hub <- sqrt(var.al.hub * solve(t(X) %*% X)) %>% diag() %>% .[2]

# Comparing with lm.fitlm.fit(X, y) %>% coef()
model.lmfit.hub <- lm.wfit(X, y, w)
coef.lmfit.hub <- model.lmfit.hub %>% coef()
var.lmfit.hub <- var(model.lmfit.hub %>% resid() * w)
se.lmfit.hub <- sqrt(var.lmfit.hub)

# It's really the same thing!

# Plotting
dt %>%
  ggplot(aes(x, y)) +
  geom_point() + 
  geom_abline(slope = coef.al[[2]], intercept = coef.al[[1]]) +
  geom_abline(slope = coef.al.hub[[2]], intercept = coef.al.hub[[1]], color = "red") +
  geom_abline(slope = coef.lmfit.hub[[2]], intercept = coef.lmfit.hub[[1]], color = "blue") +
  ylim(-4, 4)

# Doing rolling regression, say on 6 points
# Adding the weights

dt[,":=" (w = sapply(y, FUN = function(xx){return(ifelse(xx < 5, 1, 1/abs(xx)))}))]

M <- cbind(1, as.matrix(dt))

rollapplyr(M, width = 5, partial = F, fill = NA, by.column = F, FUN = function(fx){
  lm.wfit(fx[, 1:2], fx[, 3], fx[, 4]) %>% coef()
})

