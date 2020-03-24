# Core Tidyverse
library(tidyverse)
library(glue)
library(forcats)

# Time Series
library(timetk)
library(tidyquant)
library(tibbletime)

# Visualization
library(cowplot)

# Preprocessing
library(recipes)

# Sampling / Accuracy
library(rsample)
library(yardstick) 

# Modeling
library(keras)

# Data
library(quantmod)

################################################################################################
# Data
################################################################################################

# tk_tbl() conversion to tidy (timetk::)
# lubridate::as_date (loaded via tidyquant) converting zoo index to date
# as_tbl_time object makes time series operations easier
sun_spots <- datasets::sunspot.month %>% 
  tk_tbl() %>% 
  mutate(index = lubridate::as_date(index)) %>% 
  as_tbl_time(index = index)

# Exploratory

# Full data
p1 <- sun_spots %>% 
  ggplot(aes(index, value)) + 
  geom_point(color = palette_light()[[1]], alpha = 0.5) +
  theme_tq() +
  labs(title = "From 1749 to 2013 (full data set)")

# Subset of data
p2 <- sun_spots %>% filter_time("start" ~ "1800") %>% 
  ggplot(aes(index, value)) +
  geom_line(color = palette_light()[[1]], alpha = 0.5) +
  geom_point(color = palette_light()[[1]]) +
  geom_smooth(method = "loess", span = 0.2, se = F) +
  theme_tq() +
  labs(title = "1749 to 1800")

# Title of the plot
p_title <- ggdraw() + 
  draw_label("Sunspots", size = 18, fontface = "bold", colour = palette_light()[[1]])

# Creating the plot
plot_grid(p_title, p1, p2, ncol = 1, rel_heights = c(0.1, 1, 1))

################################################################################################
# Could LSTM work? Using ACF to figure this out
################################################################################################

# writing a cool function that creates a real good acf
tidy_acf <- function(data, value, lags = 0:20){
  
  value_expr <- enquo(value)

  acf_values <- data %>% pull(value) %>% acf(lag.max = tail(lags, 1), plot = F) %>% .$acf %>% .[,,1]

  ret <- tibble(acf = acf_values) %>% rowid_to_column(var = "lag") %>% mutate(lag = lag - 1) %>% filter(lag %in% lags)

  return(ret)
}

# Testing of the function
max_lag <- 12 * 50

sun_spots %>% tidy_acf(value, lags = 1:max_lag)

# Does high autocorrelation lag exist beyond 10 years?
sun_spots %>% tidy_acf(value, lags = 0:max_lag) %>% 
  ggplot(aes(lag, acf)) +
  geom_segment(aes(xend = lag, yend = 0), color = palette_light()[[1]]) +
  geom_vline(xintercept = 120, size = 2, color = palette_light()[[2]]) +
  annotate("text", label = "10 year mark", x = 130, y = 0.8, color = palette_light()[[2]]) +
  theme_tq() +
  labs(title = "ACF: Sunspots")
  
# Zooming for LSTM development
sun_spots %>% tidy_acf(value, lags = 115:135) %>%
  ggplot(aes(lag, acf)) +
  geom_vline(xintercept = 120, size = 3, color = palette_light()[[2]]) +
  geom_segment(aes(xend = lag, yend = 0), color = palette_light()[[1]]) +
  geom_point(color = palette_light()[[1]], size = 2) +
  geom_label(aes(label = acf %>% round(2)), vjust = -1,
             color = palette_light()[[1]]) +
  annotate("text", label = "10 Year Mark", x = 121, y = 0.8, 
           color = palette_light()[[2]], size = 5, hjust = 0) +
  theme_tq() +
  labs(title = "ACF: Sunspots",
       subtitle = "Zoomed in on Lags 115 to 135")

# Using filter() to get the optimal lag setting
optimal_lag_setting <- sun_spots %>% 
  tidy_acf(value, lags = 115:135) %>%
  filter(acf == max(acf)) %>%
  pull(lag)

################################################################################################
# Developing a backtesting strategy
################################################################################################

# Here, we will use 50 years for the training set and 10 years for the test set.
# We will also define a skip span of twenty years to evenly distribute the samples 
# into 11 sets that span the entire 265 years of sunspot history. We also don't want
# to give the most recent data an unfair advantage, so we don't want to be biased there.

periods_train <- 12 * 50
periods_test  <- 12 * 10
skip_span     <- 12 * 20

rolling_origin_resamples <- rolling_origin(sun_spots, 
                                           initial = periods_train,
                                           assess = periods_test,
                                           cumulative = F,
                                           skip = skip_span)

# Visualization of the backtesting strategy


# Plotting function for a single split
plot_split <- function(split, expand_y_axis = TRUE, alpha = 1, size = 1, base_size = 14) {
  
  # Manipulate data
  train_tbl <- training(split) %>%
    add_column(key = "training") 
  
  test_tbl  <- testing(split) %>%
    add_column(key = "testing") 
  
  data_manipulated <- bind_rows(train_tbl, test_tbl) %>%
    as_tbl_time(index = index) %>%
    mutate(key = fct_relevel(key, "training", "testing"))
  
  # Collect attributes
  train_time_summary <- train_tbl %>%
    tk_index() %>%
    tk_get_timeseries_summary()
  
  test_time_summary <- test_tbl %>%
    tk_index() %>%
    tk_get_timeseries_summary()
  
  # Visualize
  g <- data_manipulated %>%
    ggplot(aes(x = index, y = value, color = key)) +
    geom_line(size = size, alpha = alpha) +
    theme_tq(base_size = base_size) +
    scale_color_tq() +
    labs(
      title    = glue("Split: {split$id}"),
      subtitle = glue("{train_time_summary$start} to {test_time_summary$end}"),
      y = "", x = ""
    ) +
    theme(legend.position = "none") 
  
  if (expand_y_axis) {
    
    sun_spots_time_summary <- sun_spots %>% 
      tk_index() %>% 
      tk_get_timeseries_summary()
    
    g <- g +
      scale_x_date(limits = c(sun_spots_time_summary$start, 
                              sun_spots_time_summary$end))
  }
  
  return(g)
}

rolling_origin_resamples$splits[[2]] %>% plot_split(expand_y_axis = T) + theme(legend.position = "bottom")

# Sampling plan


# Plotting function that scales to all splits 
plot_sampling_plan <- function(sampling_tbl, expand_y_axis = TRUE, 
                               ncol = 3, alpha = 1, size = 1, base_size = 14, 
                               title = "Sampling Plan") {
  
  # Map plot_split() to sampling_tbl
  sampling_tbl_with_plots <- sampling_tbl %>%
    mutate(gg_plots = map(splits, plot_split, 
                          expand_y_axis = expand_y_axis,
                          alpha = alpha, base_size = base_size))
  
  # Make plots with cowplot
  plot_list <- sampling_tbl_with_plots$gg_plots 
  
  p_temp <- plot_list[[1]] + theme(legend.position = "bottom")
  legend <- get_legend(p_temp)
  
  p_body  <- plot_grid(plotlist = plot_list, ncol = ncol)
  
  p_title <- ggdraw() + 
    draw_label(title, size = 18, fontface = "bold", colour = palette_light()[[1]])
  
  g <- plot_grid(p_title, p_body, legend, ncol = 1, rel_heights = c(0.05, 1, 0.05))
  
  return(g)
  
}

rolling_origin_resamples %>% plot_sampling_plan(expand_y_axis = F, ncol = 3, alpha = 1, size = 1, base_size = 10,
                                                title = "Backtesting strategy: Rolling Origin Sampling Plan")


################################################################################################
# Modeling the LSTM model
################################################################################################

# First, we will develop a model on a single sample
# Use most recent sample (split 11)

split <- rolling_origin_resamples$splits[[11]]

split_id <- rolling_origin_resamples$id[[11]]

plot_split(split, expand_y_axis = F) + theme(legend.position = "bottom")

# Combining the training and testing data within one data set including a key

dt_trn <- training(split)
dt_tst <- testing(split)

dt <- bind_rows(
  dt_trn %>% add_column(key = "training"),
  dt_tst %>% add_column(key = "testing")) %>% as_tbl_time(index = index)

# The algorithm needs the data to be centered and scaled. This can be done with the recipes package

rec_obj <- recipe(value ~ ., dt) %>% step_sqrt(value) %>% step_center(value) %>% step_scale(value) %>% prep()

dt_processed_tbl <- bake(rec_obj, dt)

# Remember the scaling and recentering history for the reformatting of the data after modeling 

center_history <- rec_obj$steps[[2]]$means["value"]
scale_history <- rec_obj$steps[[3]]$sds["value"]

c("center" = center_history, "scale" = scale_history)

# LSTM plan (PRO TIPS)

# Tensor format
# input: 3D array with [samples, timesteps, features]
# output: 2D arrau with [samples, timesteps]

# Training/Testing
# The training and testing length must be evenly divisible (training length / testing length must be a whole number)

# Batch size
# The batch size is the number of training examples in one forward/backward pass of a RNN before weight update
# The batch size must be evenly divisible into both the training and testing length (training length / batch size AND testing 
# length / batch size must be whole numbers)

# Time steps
# A time step is the number of lags included in a training/testing set
# Here, we use a single lag

# Epochs
# The epochs are the total number of forward/backward pass iterations
# Typically more epochs improve model performance unless overfitting occurs, at which time the validation
# accuracy/loss will not improve

# Model inputs

lag_setting <- 120 # = nrow(dt_tst)
batch_size <- 40
train_length <- 440
tsteps <- 1
epochs <- 100

# 2D/3D train/test arrays

# Training set

lag_train_tbl <- dt_processed_tbl %>% 
  mutate(value_lag = lag(value, n = lag_setting)) %>%
  filter(!is.na(value_lag)) %>%
  filter(key == "training") %>%
  tail(train_length)

x_train_vec <- lag_train_tbl$value_lag
x_train_arr <- array(data = x_train_vec, dim = c(length(x_train_vec), 1, 1))

y_train_vec <- lag_train_tbl$value
y_train_arr <- array(data = y_train_vec, dim = c(length(y_train_vec), 1))

# Testing set

lag_test_tbl <- dt_processed_tbl %>%
  mutate(value_lag = lag(value, n = lag_setting)) %>%
  filter(!is.na(value_lag)) %>%
  filter(key == "testing")

x_test_vec <- lag_test_tbl$value_lag
x_test_arr <- array(data = x_test_vec, dim = c(length(x_test_vec), 1, 1))

y_test_vec <- lag_test_tbl$value
y_test_arr <- array(data = y_test_vec, dim = c(length(y_test_vec), 1))


# Building the model

model <- keras_model_sequential()

model %>% 
  layer_lstm(units = 50, 
             input_shape = c(tsteps, 1), 
             batch_size = batch_size,
             return_sequences = T,
             stateful = T) %>%
  layer_lstm(units = 50,
             return_sequences = F,
             stateful = T) %>%
  layer_dense(units = 1)

model %>% compile(loss = "mae", optimizer = "adam")

model

# Fitting the model

for (i in 1 : epochs) {
  model %>% fit(x          = x_train_arr, 
                y          = y_train_arr, 
                batch_size = batch_size,
                epochs     = 1, 
                verbose    = 1, 
                shuffle    = FALSE)
  
  model %>% reset_states()
  cat("Epoch: ", i)
  
}

# Predicting Using the LSTM model

# Make Predictions
pred_out <- model %>% 
  predict(x_test_arr, batch_size = batch_size) %>%
  .[,1] 

# Retransform values
pred_tbl <- tibble(
  index   = lag_test_tbl$index,
  value   = (pred_out * scale_history + center_history)^2
) 

# Combine actual data with predictions
tbl_1 <- dt_trn %>%
  add_column(key = "actual")

tbl_2 <- dt_tst %>%
  add_column(key = "actual")

tbl_3 <- pred_tbl %>%
  add_column(key = "predict")

# Create time_bind_rows() to solve dplyr issue
time_bind_rows <- function(data_1, data_2, index) {
  index_expr <- enquo(index)
  bind_rows(data_1, data_2) %>%
    as_tbl_time(index = !! index_expr)
}

ret <- list(tbl_1, tbl_2, tbl_3) %>%
  reduce(time_bind_rows, index = index) %>%
  arrange(key, index) %>%
  mutate(key = as_factor(key))

ret

# Assessing the performance of the model

calc_rmse <- function(prediction_tbl) {
  
  rmse_calculation <- function(data) {
    data %>%
      spread(key = key, value = value) %>%
      select(-index) %>%
      filter(!is.na(predict)) %>%
      rename(
        truth    = actual,
        estimate = predict
      ) %>%
      rmse(truth, estimate)
  }
  
  safe_rmse <- possibly(rmse_calculation, otherwise = NA)
  
  safe_rmse(prediction_tbl)
  
}

calc_rmse(ret)[[3]]

# Visualization

# Setup single plot function
plot_prediction <- function(data, id, alpha = 1, size = 2, base_size = 14) {
  
  rmse_val <- calc_rmse(data)[[3]]
  
  g <- data %>%
    ggplot(aes(index, value, color = key)) +
    geom_point(alpha = alpha, size = size) + 
    theme_tq(base_size = base_size) +
    scale_color_tq() +
    theme(legend.position = "none") +
    labs(
      title = glue("{id}, RMSE: {round(rmse_val, digits = 1)}"),
      x = "", y = ""
    )
  
  return(g)
}

ret %>% plot_prediction(id = split_id, alpha = 0.65) +
  theme(legend.position = "bottom")




