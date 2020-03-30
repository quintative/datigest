#######################################################################
# All independent libraries
# We want to keep this minimal
library(data.table)
library(ggplot2)
library(dplyr)
require(MASS)
# Shiny app
require(shiny)
# Density Based Clustering
require(fpc)
# Developer tools
require(devtools)
#######################################################################
# Paths
p.shiny.fcts <- paste0(getwd(), "/shiny_functions/")
p.shiny.mods <- paste0(getwd(), "/shiny_modules/")
p.dev.fncion <- paste0(getwd(), "/dev_functionalities/")
#######################################################################
# Helper functions

# Winsorization function
Wins <- function(x, sig = 4, p_prior = 0.95){
  # First, create the prior (just the input distribution ommited by outliers)
  prior <- x[which(x < quantile(x, probs = p_prior))]
  prior <- prior[which(prior > quantile(x, probs = (1 - p_prior)))]
  # Calculate the first two moments of that prior (assuming normal distribution)
  prior.avg <- mean(prior)
  prior.sig <- sd(prior) 
  # Calculate the values to which outlier values will get winsorized
  wins.abs.val.h <- prior.avg + sig * prior.sig
  wins.abs.val.l <- prior.avg - sig * prior.sig
  output <- x
  output[which(output > wins.abs.val.h)] <- wins.abs.val.h
  output[which(output < wins.abs.val.l)] <- wins.abs.val.l
  return(output)
}

# One hot encoding for the categorical covariates on data.table
OneHotEncoding <- function(datatable, colname = "test", min.factor.pct = 0.01){
  dt.int <- copy(datatable)
  # Collecting the factors that fulfill minimum frequency condition
  factor.picks <- dt.int[, .(.N), by = .(col = get(colname))] %>% 
    .[!is.na(col), .(col, N, N_tot = sum(N))] %>% 
    .[N / N_tot >= min.factor.pct, col]
  # Aggregation
  dt.int[get(colname) %in% factor.picks, (paste0(colname, "_agg")) := as.character(get(colname))]
  dt.int[is.na(get(paste0(colname, "_agg"))), (paste0(colname, "_agg")) := "other"]
  # Doing the 1-hot encoding
  dt.int <- cbind(
    dt.int,
    dt.int[, .(id = 1:.N, col = paste0("OHE_", colname, "_", get(paste0(colname, "_agg"))), val = as.integer(1))] %>% 
      dcast(id ~ col, fill = 0, value.var = "val") %>%
      .[, !c("id"), with = F])
  # Differenciate between "other" and NA
  dt.int[is.na(get(paste0(colname))), (paste0("OHE_", colname, "_other")) := 0]
  dt.int[is.na(get(paste0(colname))), (paste0("OHE_", colname, "_NA")) := 1]
  dt.int[is.na(get(paste0("OHE_", colname, "_NA"))), (paste0("OHE_", colname, "_NA")) := 0]
  # Remove temp column
  dt.int[, (paste0(colname, "_agg")) := NULL]
  return(dt.int)
}

# A legendre fit up to 5th order
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
  
  if(n == 3){
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

# Create a function that, similar to the winsorization function, looks at the inner x percentile of the data and then puts out a weight 
# vector based on a z-score that is deemed to be an outlier.
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