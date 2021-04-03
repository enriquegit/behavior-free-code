library(keras)


#########################
####### Example 1 #######
#########################

model <- keras_model_sequential()

model %>%
  layer_conv_2d(filters = 4,
                kernel_size = c(3,3),
                padding = "valid",
                activation = 'relu',
                input_shape = c(10,10,1)) %>%
  layer_max_pooling_2d(pool_size = c(2, 2)) %>%
  layer_flatten() %>%
  layer_dense(units = 32, activation = 'relu') %>%
  layer_dense(units = 2, activation = 'softmax')

summary(model)


#########################
####### Example 2 #######
#########################

model2 <- keras_model_sequential()

model2 %>%
  layer_conv_2d(filters = 16,
                kernel_size = c(3,3),
                padding = "same",
                activation = 'relu',
                input_shape = c(28,28,3)) %>%
  layer_max_pooling_2d(pool_size = c(2, 2)) %>%
  layer_flatten() %>%
  layer_dense(units = 64, activation = 'relu') %>%
  layer_dense(units = 5, activation = 'softmax')

summary(model2)
