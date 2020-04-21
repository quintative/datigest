source("libs_functions.R")

#############################################################################################################################
# First, let's make a martingale
# Here, I am going to use a Gaussian random walk, where the step size varies according to a normal distribution

# Create brownian motions using annual drift and volatility
GenBrownMotion <- function(n.lambda = 10, s0 = c(1000), mu.lambda = c(0.1), sig.lambda = c(0.2), n.steps = 252){
  
  # Examplified on Monte Carlo simulation of stock returns!
  # dS(t) = mu S(t) dt + sigma S(t) dB(t), S(0) = S0
  # mu := drift of the process
  # sigma := volatility
  # B(t) := Brownian motion (randomness injection)
  
  # Solution to the SDE (stochastic differential equation) is:
  # S(t + h) = S(t) exp{(mu - 1/2 sigma^2) h + sigma B(h)}
  # We chose B(h) ~ N(0, h) >> the normal distribution
  # because of exp{x} >> this is lognormal return
  
  # Calculating the stock return 
  # R(t + h) = ln(S(t + h) / S(t)) ~ N[(mu - 1/2 sigma^2) h, sigma^2 h]
  
  # Conditional mean of the stock price is 
  # E[S(t + h) | S(t)] = S(t) * exp{mu h}
  
  # For stocks - typical annual sigma is ~ 0.35
  # The mean return is !!!! >> mu = mean(returns) + 1/2 sigma^2 (mind here the sigma^2 !!!)
  
  # Now, let's do the MC simulation of such a martingale. With B(h) = e sqrt(h) >> e ~ N(0, 1) ... mean 0 and variance h
  
  # Using the power of vectorization in R (super duper fast!)
  h <- 1 / n.steps
  
  s <- matrix(0, length(mu.lambda), n.lambda * n.steps + 1) 
  s[, 1] <- s0
  
  for(i in 2:(n.yrs * n.steps + 1)){
    s[, i] <- s[, i - 1] * exp((mu.lambda - sig.lambda**2 / 2) * h + sig.lambda * rnorm(length(mu.lambda)) * sqrt(h))
  }
  
  
  dt.int <- data.table(t = 0:(n.bdays * n.yrs))
  dt.int <- cbind(dt.int, data.table(t(s)))
  
  col.names <- paste0("b_", seq(1:length(mu.lambda)))
  colnames(dt.int) <- c("t", col.names)
  
  # Ultra fast data.table dcast
  dt.int <- melt.data.table(dt.int, id.vars = "t", variable.name = "martingale")
  
  return(dt.int)
}

n.rep <- 20
brewer.pal(100, "RdYlBu")
test <- GenBrownMotion(n.lambda = 10, 
                       s0 = rep(1, n.rep), 
                       mu.lambda = rep(0.1, n.rep), 
                       sig.lambda = rep(0.25, n.rep), 
                       n.steps = 252)


ggplotly(test %>% ggplot(aes(t, value, colour = martingale)) + 
           geom_line() +
           scale_y_continuous(breaks = seq(0, 50, 2))) %>% 
  layout(showlegend = F)

#############################################################################################################################


# Create a Gaussian random walk with n steps and first two moments of the normal distribution
GenNormRandomWalk <- function(n = 100, mu = 0.01, sd = 0.1){
  
  dt.int <- data.table(t = 0:(n - 1), z = rnorm(n, mu, sd))
  dt.int[1, z := 0]
  
  setkey(dt.int, t)
  
  dt.int[, z := cumsum(z)]
  
  return(dt.int)
}

# Super fast
a <- Sys.time()
dt.x <- GenNormRandomWalk(10000, 0.002, 0.15)
Sys.time() - a
dt.x %>% ggplot(aes(t, z)) + geom_line()

# Create a Gaussian random walk with n steps and first two moments of the normal distribution
GenLogNormRandomWalk <- function(n = 100, mu = 0.01, sd = 0.1){
  
  dt.int <- data.table(t = 0:(n - 1), z = log(1 + rnorm(n, mu, sd)))
  dt.int[1, z := 1]
  
  setkey(dt.int, t)
  
  dt.int[, z := cumsum(z)]
  dt.int[, z := exp(z)]
  
  return(dt.int)
}

dt.x <- GenLogNormRandomWalk(252 * 10, 1.1**(1/252) - 1, 2.5**(1/252) - 1)
dt.x %>% ggplot(aes(t, z)) + geom_line()
