library(keras)

# Generate a train set.
# First element is the input x and 
# the second element is the output y.
train_set <- data.frame(x = c(3.0,4.0,1.0),
                        y = c(1.5, 2.0, 0.5))

# Instantiate a sequential model.
model <- keras_model_sequential()

# Define model.
model %>%
  layer_dense(units = 1,
              use_bias = FALSE,
              activation = 'linear',
              input_shape = 1)

# Compile model.
model %>%compile(
  optimizer = optimizer_sgd(lr = 0.01),
  loss = 'mse',
  metrics = list('mse','mae')
)

# Print a summary of the model.
summary(model)

# Train the model.
history <- model %>% fit(
  as.matrix(train_set$x), as.matrix(train_set$y),
  epochs = 30,
  batch_size = 3,
  validation_split = 0,
  verbose = 2,
  view_metrics = TRUE
)

plot(history)

# Make predictions on new points.
model %>% predict_on_batch(c(7, 50, -220))
