source("libs_functions.R")
source("dev_functionalities/data_generation.R")

############################################################################################################

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

# Creating two normally distributed clusters
dt.x <- data.table(x1 = rnorm(2000), y1 = rnorm(2000))
dt.x <- rbind(dt.x, data.table(x1 = rnorm(1000, 4), y1 = rnorm(1000, -4)))

# Creating a few more weird clusters
dt.x[, x2 := rnorm(2000)]
dt.x[, y2 := x2**2]

# test
GUI_2DClust(dt.x)
