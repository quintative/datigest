source("libs_functions.R")
source("dev_functionalities/data_generation.R")

############################################################################################################

# Create some data
dt.x <- GenLinDataNorm(1000, y.norm = c(0, 1), x.norm = c(0, 1), rsq = 0.4)
# AddIndepLinNorm(dt.x, "x2", "y")
# AddIndepLinNorm(dt.x, "x3", "y")
# Adding an outlier
# dt.x <- rbind(dt.x, data.table(x = 140, y = 400))

# Starting the regression
source(paste0(p.shiny.fcts, "GUI_2danalysis.R"))
GUI_2Danalysis(dt.x)


# Creating two normally distributed clusters
dt.x <- data.table(x1 = rnorm(2000), y1 = rnorm(2000))
dt.x <- rbind(dt.x, data.table(x1 = rnorm(1000, 4), y1 = rnorm(1000, -4)))

# Creating a few more weird clusters
dt.x[, x2 := rnorm(2000)]
dt.x[, y2 := x2**2]

# test
GUI_2DClust(dt.x)
