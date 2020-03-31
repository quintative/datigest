source("libs_functions.R")
source("dev_functionalities/data_generation.R")
source(paste0(p.shiny.fcts, "GUI_2DClust.R"))
source(paste0(p.shiny.fcts, "GUI_regression.R"))


# Clustering
############################################################################################################

# Creating two normally distributed clusters
dt.x <- data.table(x1 = rnorm(1000), y1 = rnorm(1000))
dt.x <- rbind(dt.x, data.table(x1 = rnorm(1000, 2), y1 = rnorm(1000, 2)))

# Creating a few more weird clusters
dt.x[, x2 := rnorm(2000)]
dt.x[, y2 := x2**2]

# test
GUI_2DClust(dt.x)

# Clustering
############################################################################################################

# Create some data
dt.x <- GenLinDataNorm(1000, y.norm = c(0, 1), x.norm = c(0, 1), rsq = 0.4)

# Starting the regression
source(paste0(p.shiny.fcts, "GUI_regression.R"))
GUI_Regress(dt.x)

