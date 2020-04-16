HuberLossWins <- function(y, inner.quantile = 0.9, sig.outlier = 5){
  
  # Getting the inner distribution (presumably exclouding outliers, assuming they are rare and not fat tails)
  inner.y <- y[which(y <= as.numeric(quantile(y, inner.quantile)))]
  inner.y <- inner.y[which(inner.y >= as.numeric(quantile(inner.y, (1 - inner.quantile))))]
  sd.y <- sd(inner.y)
  
  # Defining the boundary for what we consider an outlier to be
  outlier.boundary <- sig.outlier  * sd.y
  
  # Creating the weight vector based on Huber loss function
  w <- sapply(y, function(val) {
    if(abs(val) < outlier.boundary){out <- 1}
    else{out <- abs(val)}
    return(outlier.boundary / out)
  })
  
  return(w)
}

LinRegRobPlot <- function(data){
  
  colnames(data) <- c("x", "y")
  
  x <- data[, x]
  y <- data[, y]
  n.l <- length(y)
  # Building the matrix of independent variable
  X <- matrix(c(rep(1, n.l), x), ncol = 2)
  
  # Computing the weight vecotor for the regression
  w <- HuberLossWins(y, inner.quantile = 0.9, sig.outlier = 5)
  
  coef.hub <- solve(t(X) %*% (w * X)) %*% t(X) %*% (w * y)
  var.hub <- var(x) * coef.hub[[2]] ** 2 + var(y * w) - 2 * coef.hub[[2]] * cov(x, y * w)
  se.hub <- sqrt(var.hub * solve(t(X) %*% (w * X))) %>% diag() %>% .[2]
  
  # Computing the sums of squares and variance
  y.hat <- X %*% coef.hub
  variance <- sum(((w * y) - y.hat)**2) / (n.l - 2)
  s.beta <- sqrt(variance / sum((x - mean(x))**2))
  
  if(nrow(data) > 5000){
    dt.int <- as.data.table(sample_n(data, 5000))
  } else{
    dt.int <- data
  }
  
  abline.trace <- data.table(x = seq(from = dt.int[, min(x)], to = dt.int[, max(x)], length.out = 10))
  abline.trace[, y := coef.hub[2, 1] * x + coef.hub[1, 1]]
  
  plt <- dt.int %>%
    plot_ly(x = ~x, y = ~y) %>%
    add_markers(color = I("black"), alpha = 0.4)  %>% 
    add_lines(data = abline.trace, x = ~x, y = ~y, color = I("red")) %>%
    layout(showlegend = F)
  
  return(plt)
}