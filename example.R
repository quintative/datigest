source("libs_functions.R")
source("dev_functionalities/data_generation.R")

############################################################################################################
# OLS regression

# Create data with a linear dpendence
dt.x <- GenLinDataNorm(5000, y.norm = c(0, 3), x.norm = c(0, 3), rsq = 0.05)

source(paste0(p.shiny.fcts, "GUI_2danalysis.R"))
GUI_2Danalysis(dt.x)

# Create data with higher order non-linear dependence
dt.x <- data.table(x = rnorm(3000))
dt.x[, y := 1.2 * FLegendre(x, 1) + rnorm(3000) +
       0.1 * FLegendre(x, 2) + rnorm(3000)+ 
       0.3 * FLegendre(x, 3) + rnorm(3000) + 
       0.05 * FLegendre(x, 4) + rnorm(3000)]

source(paste0(p.shiny.fcts, "GUI_2danalysis.R"))
GUI_2Danalysis(dt.x)

############################################################################################################
# Clustering
  
# Creating two normally distributed clusters
dt.x <- GenClustData(n = 100, center = c(0, 2), sigx = 0.1, sigy = 0.5)
dt.x <- rbind(dt.x, GenClustData(n = 100, center = c(-1, -1), sigx = 1.8, sigy = 1.1))
dt.x <- rbind(dt.x, GenClustData(n = 400, center = c(-3, 2), sigx = 0.1, sigy = 3.1))
dt.x <- rbind(dt.x, GenCircleData(n = 500, r = 5, center = c(0, 0), sig = 0.1))

# test
source(paste0(p.shiny.fcts, "GUI_2danalysis.R"))
GUI_2Danalysis(dt.x)


############################################################################################################
# Trying what happens with ranked variables

# Create data with a linear dpendence
dt.x <- GenLinDataNorm(5000, y.norm = c(0, 3), x.norm = c(0, 3), rsq = 0.2)

# test
source(paste0(p.shiny.fcts, "GUI_2danalysis.R"))
GUI_2Danalysis(dt.x)


