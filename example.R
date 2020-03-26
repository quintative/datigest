source("libs_functions.R")
source(paste0(p.shiny.fcts, "GUI_2DClust.R"))
source(paste0(p.shiny.fcts, "GUI_regression.R"))

# Creating two normally distributed clusters
dt.x <- data.table(x1 = rnorm(1000), y1 = rnorm(1000))
dt.x <- rbind(dt.x, data.table(x1 = rnorm(1000, 2), y1 = rnorm(1000, 2)))

# Creating a few more weird clusters
dt.x[, x2 := rnorm(2000)]
dt.x[, y2 := x2**2]

dt.x %>% ggplot(aes(x2, y2)) + geom_point()

# test
GUI_2DClust(dt.x)
GUI_Regress(dt.x)

