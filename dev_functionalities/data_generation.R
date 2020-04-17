source("libs_functions.R")

# Generating linear dependency
GenLinDataNorm <- function(n = 1000, 
                           y.norm = c(0, 1),
                           x.norm = c(0, 1),
                           rsq = 0.1){
  
  # Define independent variables
  dt.int <- data.table(x = rnorm(n, x.norm[1], x.norm[2]))
  
  # Total variance of dependent variable
  var.tot <- y.norm[2]**2
  # Residual variance (total - explained)
  var.res <- (1 - rsq) * var.tot

  # Create the dependent variable
  dt.int[, y := sqrt(rsq) * y.norm[2] / x.norm[2] * x + rnorm(n, y.norm[1], sqrt(var.res))]
  
  return(dt.int)
}

# Adding linear dependent variable to data.table
AddDepLinNorm <- function(datatable, x.name = "x", y.name = "y",
                          y.norm = c(0, 1), rsq = 0.1){
  
  # Total variance of dependent variable
  var.tot <- y.norm[2]**2
  # Residual variance (total - explained)
  var.res <- (1 - rsq) * var.tot
  
  # Create the dependent variable
  datatable[, (y.name) := sqrt(rsq) * y.norm[2] / sd(get(x.name)) * get(x.name) + rnorm(nrow(datatable), y.norm[1], sqrt(var.res))]
}

# Adding linear independent variable to data.table
AddIndepLinNorm <- function(datatable, x.name = "x", y.name = "y",
                            x.norm = c(0, 1), rsq = 0.1){
  
  # Create the independent variable
  datatable[, (x.name) := (sqrt(rsq) * get(y.name) * x.norm[2] / sd(get(y.name)) + 
                             sqrt(1 - rsq) * rnorm(nrow(datatable), x.norm[1], x.norm[2]))]
}

# Create a circle of n points with radius r and noise sig.
GenCircleData <- function(n = 1000, r = 1, center = c(0, 0), sig = 0.1){
  # First, start with the first point at phi = 0
  dt.int <- data.table(x = r * (1 + rnorm(1) * sig), y = 0 + r * rnorm(1) * sig)
  # Let's think in polar coordinates. Distribute points over equal angles from each other:
  d.phi <- seq(1 / n, 1 - 1 / n, length.out = n - 1) * 2
  # Transform polar coordinates into cartesian
  for(i in d.phi){
    dt.int <- rbindlist(list(dt.int, data.table(x = r * (cospi(i) + rnorm(1) * sig), y = r * (sinpi(i) + rnorm(1) * sig))))
  }
  # Off-centering
  dt.int[, ":=" (x = x + center[1], y = y + center[2])]
  return(dt.int)
}

# Create normally distributed data
GenClustData <- function(n = 1000, center = c(0, 0), sigx = 0.1, sigy = 0.1){
  dt.int <- data.table(x = rnorm(n) * sigx + center[1], y = rnorm(n) * sigy + center[2])
  return(dt.int)
}

