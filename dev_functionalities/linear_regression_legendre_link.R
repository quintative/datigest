# I want to fit nonlinear functions to data using OLS on Legendre polinomials



test <- dt.x %>% FitLegendre(n = 4)
ggplot() +
  geom_point(data = dt.x, aes(x, y)) +
  geom_line(data = test$fitline, aes(x, y), color = "red", size = 1.5)


test$model %>% summary()
test$fitline