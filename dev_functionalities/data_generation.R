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

