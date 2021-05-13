#### This script demonstrates how to train an adaptive model. ####

source(file.path("..","..","auxiliary_functions","globals.R"))
library(keras)
library(abind)

# Path to smartphone activities in image format.
filepath <- file.path(datasets_path,"smartphone_activities","images.txt")

# Read data.
df <- read.csv(filepath, stringsAsFactors = F)

# Shuffle rows.
set.seed(1234)
df <- df[sample(nrow(df)),]

## Convert labels to integers starting at 0. ##

# Get the unique labels.
labels <- unique(df$label)

mapping <- 0:(length(labels) - 1)

names(mapping) <- labels

print(mapping)

# Append labels as integers at the end of data frame.
df$intlabel <- mapping[df$label]

# Get the unique user ids.
users <- unique(df$userid)

# Print all user ids.
print(users)

# Choose one user at random to be the target user.
targetUser <- sample(users, 1)

# Split into train and target user sets. The train set includes data from all users excluding targetUser.
trainset <- df[df$userid != targetUser,]

# Save train labels in a separate variable.
train.y <- trainset$intlabel

# Save train pixels in a separate variable.
train.x <- as.matrix(trainset[,-c(301,302,303)])

# This contains all data from the target user.
target.data <- df[df$userid == targetUser,]

# Split the target user's data into 50% adaptive and 50% test data.
idxs <- sample(nrow(target.data),
               size = nrow(target.data) * 0.5,
               replace = FALSE)

# Target user adaptive set. This data is used to adapt the model to the target user.
target.adaptive <- target.data[idxs,]

# Save target.adaptive labels in a separate variable.
target.adaptive.y <- target.adaptive$intlabel

# Save target.adaptive pixels in a separate variable.
target.adaptive.x <- as.matrix(target.adaptive[,-c(301,302,303)])

# Target user test set.
# This data is used to test the performance.
target.test <- target.data[-idxs,]

# Save target.test labels in a separate variable.
target.test.y <- target.test$intlabel

# Save target.test pixels in a separate variable.
target.test.x <- as.matrix(target.test[,-c(301,302,303)])

# Learn min and max values from train set for normalization.
maxv <- max(train.x)
minv <- min(train.x)

# Function to normalize and reshape arrays into 3D images.
normalize.reshape <- function(x, minv, maxv){
  # Truncate values if out of range.
  x[x < minv] <- minv
  x[x > maxv] <- maxv
  
  # Normalize.
  x <- (x - minv) / (maxv - minv)
  
  # Reshape into 10x10x3 images.
  x.reshaped <- NULL; adim <- 0
  for(i in 1:dim(x)[1]){
    img <- array(x[i,], c(10,10,3))
    x.reshaped <- abind(x.reshaped, img, along = adim)
    adim <- 1
  }
  return(x.reshaped)
}

# Normalize and reshape. May take some minutes.
train.x <- normalize.reshape(train.x, minv, maxv)
target.adaptive.x <- normalize.reshape(target.adaptive.x, minv, maxv)
target.test.x <- normalize.reshape(target.test.x, minv, maxv)

dim(train.x)

dim(target.adaptive.x)

dim(target.test.x)

#### Build the initial user-independent model ####

model <- keras_model_sequential()

model %>%
  layer_conv_2d(name = "conv1",
                filters = 8,
                kernel_size = c(2,2),
                activation = 'relu',
                input_shape = c(10,10,3)) %>%
  layer_conv_2d(name = "conv2",
                filters = 16,
                kernel_size = c(2,2),
                activation = 'relu') %>%
  layer_max_pooling_2d(pool_size = c(2, 2)) %>%
  layer_flatten() %>%
  layer_dense(name = "hidden1", units = 32, activation = 'relu') %>%
  layer_dropout(0.25) %>%
  layer_dense(units = 6, activation = 'softmax')

# Print summary.
summary(model)

# Compile model.
model %>% compile(
  loss = 'sparse_categorical_crossentropy',
  optimizer = optimizer_sgd(lr = 0.01),
  metrics = c("accuracy")
)

# Fit the user-independent model.
history <- model %>% fit(
  train.x, train.y,
  epochs = 50,
  batch_size = 8,
  validation_split = 0.15,
  verbose = 1,
  view_metrics = TRUE
)

plot(history)

# Save model.
save_model_hdf5(model, "user-independent.hdf5")
#save_model_tf(model, "user-independent_tf")

# Load model.
#model <- load_model_hdf5("user-independent.hdf5")

# Alternatively, if you saved it as SavedModel.
#model <- load_model_tf("user-independent_tf")

# Compute performance (accuracy) on the target user test set.
model %>% evaluate(target.test.x, target.test.y)

########################################################
#### Adapt the model using the target adaptive data ####
########################################################

# Load model.
adaptive.model <- load_model_hdf5("user-independent.hdf5")

# Alternatively, if you saved it as SavedModel.
#adaptive.model <- load_model_tf("user-independent_tf")

# Freeze all layers.
freeze_weights(adaptive.model, from = 1)

# Unfreeze layers from conv2.
unfreeze_weights(adaptive.model, from = "conv2")

# Compile model. We need to compile after freezing/unfreezing weights.
adaptive.model %>% compile(
  loss = 'sparse_categorical_crossentropy',
  optimizer = optimizer_sgd(lr = 0.01),
  metrics = c("accuracy")
)

summary(adaptive.model)

# Update model with adaptive data.
history <- adaptive.model %>% fit(
  target.adaptive.x, target.adaptive.y,
  epochs = 50,
  batch_size = 8,
  validation_split = 0,
  verbose = 1,
  view_metrics = TRUE
)

print(history)

# Save model.
#save_model_hdf5(adaptive.model, "adaptive-model.hdf5")

# Or save it as SavedModel.
#save_model_tf(adaptive.model, "adaptive-model_tf")

# Load model.
#adaptive.model <- load_model_hdf5("adaptive-model.hdf5")

# Or if you saved it as SavedModel.
#adaptive.model <- load_model_tf("adaptive-model_tf")


# Compute performance (accuracy) on the target user test set.
adaptive.model %>% evaluate(target.test.x, target.test.y)


#### Train user-dependent model for 50 more epochs. ####
# This is to check that the accuracy increase was not due to more epochs
# but because the adaptation strategy.
retrained_model <- load_model_hdf5("user-independent.hdf5")

# Or if you saved it as SavedModel.
#retrained_model <- load_model_tf("user-independent_tf")


# Fit the user-independent model for 50 more epochs.
history <- retrained_model %>% fit(
  train.x, train.y,
  epochs = 50,
  batch_size = 8,
  validation_split = 0.15,
  verbose = 1,
  view_metrics = TRUE
)

# Save retrained model.
#save_model_hdf5(retrained_model, "user-independent-retrained.hdf5")

# Alternatively, save it as SavedModel.
#save_model_tf(retrained_model, "user-independent-retrained_tf")

# Load model.
#retrained_model <- load_model_hdf5("user-independent-retrained.hdf5")

# Or if you saved it as SavedModel.
#retrained_model <- load_model_tf("user-independent-retrained_tf")

# Compute performance (accuracy) on the target user test set.
retrained_model %>% evaluate(target.test.x, target.test.y)
