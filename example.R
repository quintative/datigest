source("libs_functions.R")
source(paste0(p.shiny.fcts, "GUI_2DClust.R"))
source(paste0(p.shiny.fcts, "GUI_regression.R"))

dt.x <- data.table(x = rnorm(1000), y = rnorm(1000))
dt.x <- rbind(dt.x, data.table(x = rnorm(1000, 2), y = rnorm(1000, 2)))


# test
GUI_2DClust(dt.x)
GUI_Regress(dt.x)
