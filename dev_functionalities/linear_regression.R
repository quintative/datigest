source("libs_functions.R")

# Getting a simple linear regression estimate from a 2D data.table
# Let's see what the most performant approach is
# Making a really big data.table (something that will take at least a second to run)
n <- 1E6
dt.x <- data.table(x = rnorm(n))
dt.x[, y := 1.2 * x + 2]
dt.x[, y := y + rnorm(n, 0, 3)]

# dt.x %>% ggplot(aes(x, y)) + geom_point()

# The normal way (using the lm function)
t.normal <- sapply(1:10, function(x){
  a <- Sys.time()
  lm(y ~ x, dt.x)
  b <- Sys.time()
  return(b - a)
}) %>% sum()


t.stripped <- sapply(1:10, function(x){
  a <- Sys.time()
  # Defining the independent variable
  y <- dt.x[, y]
  # Building the matrix of independent variable
  X <- matrix(c(rep(1, length(y)), dt.x[, x]), ncol = 2)
  # Getting the intercept and coefficient
  M.coefs <- solve(t(X) %*% X) %*% t(X) %*% y
  b <- Sys.time()
  return(b - a)
}) %>% sum()

# The stripped version is ~2.3 times faster for using it on large data (without errors, obviously)
t.normal/t.stripped

# Let's include the errors
t.normal.errors <- sapply(1:10, function(x){
  a <- Sys.time()
  model <- lm(y ~ x, dt.x) 
  summary(model)
  b <- Sys.time()
  return(b - a)
}) %>% sum()

t.stripped.errors <- sapply(1:10, function(x){
  a <- Sys.time()
  # Defining the independent variable
  x <- dt.x[, x]
  y <- dt.x[, y]
  n.l <- length(y)
  # Building the matrix of independent variable
  X <- matrix(c(rep(1, n.l), x), ncol = 2)
  # Getting the intercept and coefficient
  M.coefs <- solve(t(X) %*% X) %*% t(X) %*% y
  # Computing hat matrix and variance
  y.hat <- X %*% M.coefs
  variance <- sum((y - y.hat)**2) / (n.l - 2)
  s.beta <- sqrt(variance / sum((x - mean(x))**2))
  b <- Sys.time()
  return(b - a)
}) %>% sum()

t.c.errors <- sapply(1:10, function(x){
  a <- Sys.time()
  x <- dt.x[, x]
  X <- matrix(c(rep(1, length(x)), x), ncol = 2)
  # Defining the independent variable
  model <- lm.fit(X, dt.x[, y])
  coefs <- coef(model)
  variance <- var(model$residuals)
  s.beta <- sqrt(variance / sum((x - mean(x))**2))
  b <- Sys.time()
  return(b - a)
}) %>% sum()

# Wow - including the standard error of the estimator makes it ~ 6 times faster that way!
# The build in function sucks!! - let's build our own stuff
t.normal.errors 
t.stripped.errors
t.c.errors