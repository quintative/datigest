
test <- GenCircleData(1000, 1, c(2, 3), 0.1)
test %>% ggplot(aes(x, y)) + geom_point()
