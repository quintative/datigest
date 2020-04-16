source("libs_functions.R")
source("dev_functionalities/data_generation.R")

TransfQuant <- function(datatable, varx = "x", vary = "y", quantiles = 10){
  dt.int <- copy(datatable)
  dt.int[, x_q := ntile(get(varx), quantiles)]
  dt.int[, y_m := mean(get(vary), na.rm = T), by = .(x_q)]
  dt.int[, y_s := sd(get(vary), na.rm = T), by = .(x_q)]
  dt.int[, n := .N, by = .(x_q)]
  dt.int[, y_95 := qt(0.975, n) * y_s / sqrt(n - 1)]
  
  dt.int <- unique(dt.int[, .(x = x_q, y = y_m, y_95)])
  dt.int[, x := as.factor(x)]
  setkey(dt.int, x)
  return(dt.int)
}

# Creating some highly correlated data
dt.x <- GenLinDataNorm(1000, rsq = 0.5)
dt.x %>% ggplot(aes(x, y)) + geom_point()

test <- TransfQuant(dt.x, "x", "y", 5)
test %>% ggplot(aes(x, y)) + geom_bar(stat = "identity") + geom_errorbar(aes(ymin = y - y_95, ymax = y + y_95))


