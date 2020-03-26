# Defining the function
FitLegendre <- function(dt, n = 3){
  
  dt <- as.data.table(dt)
  
  FLegendre <- function(x, m){
    if(m == 1){y <- x}
    if(m == 2){y <- 1/2 * ( 3 * x ** 2 -  1)}
    if(m == 3){y <- 1/2 * ( 5 * x ** 3 -  3 * x)}
    if(m == 4){y <- 1/8 * (35 * x ** 4 - 30 * x ** 2 +  3)}
    if(m == 5){y <- 1/8 * (63 * x ** 5 - 70 * x ** 3 + 15 * x)}
    return(y)
  }
  
  if(n == 1){
    model <- lm(dt$y ~ FLegendre(x = dt$x, m = 1))
  }
  
  if(n == 2){
    model <- lm(dt$y ~ FLegendre(x = dt$x, m = 1) +
                       FLegendre(x = dt$x, m = 2))
  }
  
  if(n == 3){
    model <- lm(dt$y ~ FLegendre(x = dt$x, m = 1) +
                       FLegendre(x = dt$x, m = 2) +
                       FLegendre(x = dt$x, m = 3))
  }
  
  if(n == 4){
    model <- lm(dt$y ~ FLegendre(x = dt$x, m = 1) +
                       FLegendre(x = dt$x, m = 2) +
                       FLegendre(x = dt$x, m = 3) +
                       FLegendre(x = dt$x, m = 4))
  }
  
  if(n == 5){
    model <- lm(dt$y ~ FLegendre(x = dt$x, m = 1) +
                       FLegendre(x = dt$x, m = 2) +
                       FLegendre(x = dt$x, m = 3) +
                       FLegendre(x = dt$x, m = 4) +
                       FLegendre(x = dt$x, m = 5))
  }
  
  return(model)
}

FitLegendre(dt)


# Simulation of the fitting

# Generate noisy data from the function itself
v.seq    <- seq(-10, 10, 0.1)
v.coef   <- c(5.4, 0.1, -.01)
x.noise  <- rnorm(length(v.seq), 0, 0.5)
y.noise  <- rnorm(length(v.seq), 0, 25)
dt.noise <- data.table(x = v.seq + x.noise,
                       y = v.coef[1] * FLegendre(v.seq, 1) +
                         v.coef[2] * FLegendre(v.seq, 2) +
                         v.coef[3] * FLegendre(v.seq, 3) +
                         y.noise)

ggplot(dt.noise, aes(x = x)) +
  geom_point(aes(y = y))

# Simple fitting a 3rd order polynomial to the noisy data.

model <- FitLegendre(dt.noise)
dt.noise <- cbind(dt.noise, fit = model$fitted.values)

ggplot(dt.noise, aes(x = x)) +
  geom_point(aes(y = y)) +
  geom_line(aes(y = fit))


