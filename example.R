source("libs_functions.R")
source(paste0(p.shiny.fcts, "GUI_2DClust.R"))

dt.x <- data.table(x = rnorm(1000), y = rnorm(1000))
dt.x <- rbind(dt.x, data.table(x = rnorm(1000, 2), y = rnorm(1000, 2)))

GUI_2DClust(dt.x)