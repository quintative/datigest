source("libs_functions.R")
source("dev_functionalities/data_generation.R")

TransfRank <- function(datatable, var = "x", mode = c(0, 1)){
  dt.int <- copy(datatable)
  dt.int[, (var) := frank(get(var)) - 1]
  max.rank <- dt.int[, max(get(var))]
  dt.int[, (var) := get(var) / max.rank]
  mode.diff <- mode[2] - mode[1]
  dt.int[, (var) := (get(var) + mode[1] / mode.diff) * mode.diff]
  return(dt.int)
}

# Creating some highly correlated data
dt.x <- GenLinDataNorm(1000, rsq = 0.5)
dt.x %>% ggplot(aes(x, y)) + geom_point()

# Rank the independent variable only
test <- TransfRank(dt.x, "x", mode = c(-1, 1))
test[, summary(x)]
test %>% ggplot(aes(x, y)) + geom_point()

# Also rank the dependent variable
test <- TransfRank(test, "y", mode = c(-1, 1))
test %>% ggplot(aes(x, y)) + geom_point()

