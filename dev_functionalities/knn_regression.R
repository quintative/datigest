source("libs_functions.R")

# Creating some highly correlated data
dt.x <- GenLinDataNorm(1000, rsq = 0.5)
dt.x %>% ggplot(aes(x, y)) + geom_point()

model <- knnreg(y ~ x, dt.x, k = 200)

dt.x[, y_p := predict(model, dt.x)]

dt.x %>% ggplot() +
  geom_point(aes(x, y)) +
  geom_line(aes(x, y_p), color = "red")
