source(file.path("..","..","auxiliary_functions","globals.R"))
source(file.path("..","..","auxiliary_functions","functions.R"))
library(keras)
library(PRROC)


# Read dataset. This one contains the extracted features from the raw fish trajectories.
dataset <- read.csv(file.path(datasets_path,
                              "fish_trajectories","fishFeatures.csv"),
                    stringsAsFactors = T)

#### Split into train/test sets ####

# Since this is unsupervised, we do not need the abnormal cases during training,
# so all abnormal cases are used for testing.
test.abnormal <- dataset[dataset$label == "abnormal",]

# Percent to use as train set.
pctTrain <- 0.80

# Corresponding number of samples.
nsamples <- ceiling(sum(dataset$label == "normal") * pctTrain)

# Generate the indices of the train set instances.
set.seed(123)

idxs <- sample(nsamples)

train.normal <- dataset[dataset$label == "normal", ][idxs,]

test.normal <- dataset[dataset$label == "normal", ][-idxs,]

# Define function to standardize the data.
normalize.standard <- function(data, means = NA, sds = NA){

  # Define variables to store the learned parameters. (mean and sd).
  paramMeans <- NULL
  paramSds <- NULL
  
  # Iterate columns
  for(i in 1:ncol(data)){
    
    c <- data[,i]
    
    if(is.na(means) || is.na(sds)){
      tmpMean <- mean(c)
      tmpSd <- sd(c)
      
      paramMeans <- c(paramMeans, tmpMean)
      paramSds <- c(paramSds, tmpSd)
    }
    else{
      tmpMean <- means[i]
      tmpSd <- sds[i]
    }
    
    normalizedCol <- (c - tmpMean) / tmpSd
    
    data[,i] <- normalizedCol
  }
  
  return(list(data=data, means=paramMeans, sds=paramSds))
  
}

# Use our previously defined function to normalize the data.
res <- normalize.standard(train.normal[,-c(1:2)])

train.normal <- cbind(train.normal[,1:2], res$data)

# Normalize the test data using the learned parameters.
test.normal <- cbind(test.normal[,1:2],
                     normalize.standard(test.normal[,-c(1:2)],
                                        res$means, res$sds)$data)

test.abnormal <- cbind(test.abnormal[,1:2],
                       normalize.standard(test.abnormal[,-c(1:2)],
                                          res$means,
                                          res$sds)$data)


#### Define Autoenconder ####
autoencoder <- keras_model_sequential()

autoencoder %>%
  layer_dense(units = 32, activation = 'relu', input_shape = ncol(train.normal)-2) %>% 
  layer_dense(units = 16, activation = 'relu') %>%
  layer_dense(units = 8, activation = 'relu') %>%
  layer_dense(units = 16, activation = 'relu') %>%
  layer_dense(units = 32, activation = 'relu') %>%
  layer_dense(units = ncol(train.normal)-2, activation = 'linear')

# Print a summary of the autoencoder.
summary(autoencoder)

autoencoder %>% compile(
  loss = 'mse',
  optimizer = optimizer_sgd(lr = 0.01),
  metrics = c('mse')
)

history <- autoencoder %>% fit(
  as.matrix(train.normal[,-c(1:2)]),
  as.matrix(train.normal[,-c(1:2)]),
  epochs = 100,
  batch_size = 32,
  validation_split = 0.10,
  verbose = 2,
  view_metrics = TRUE
)

plot(history)

# Save model.
#save_model_hdf5(autoencoder, "fishAutoencoder.hdf5")

# Save it as SavedModel.
#save_model_tf(autoencoder, "fishAutoencoder_tf")

# Load pre-trained model if you want to get same results from the book.
#autoencoder <- load_model_hdf5("fishAutoencoder.hdf5")

# Or load the SavedModel version.
#autoencoder <- load_model_tf("fishAutoencoder_tf")

# Compute MSE of normal test set.
autoencoder %>% evaluate(as.matrix(test.normal[,-c(1:2)]),
                         as.matrix(test.normal[,-c(1:2)]))

# Compute MSE of abnormal test set.
autoencoder %>% evaluate(as.matrix(test.abnormal[,-c(1:2)]),
                         as.matrix(test.abnormal[,-c(1:2)]))

# This function computes the squared errors for each instance.
# The total error for an instance is the sum of the squared errors of each feature.
squared.errors <- function(preds, groundTruth){
  
  squaredErrors <- NULL

  for(i in 1:nrow(preds)){
    
    se <- sum((preds[i,] - groundTruth[i,])^2)
    
    squaredErrors <- c(squaredErrors, se)
    
  }
  
  return(squaredErrors) 
}

# Predict values on the normal train set.
preds.train.normal <- autoencoder %>%
  predict_on_batch(as.matrix(train.normal[,-c(1:2)]))

# Compute individual reconstruction errors in train set.
errors.train.normal <- squared.errors(preds.train.normal, as.matrix(train.normal[,-c(1:2)]))

hist(errors.train.normal)

mean(errors.train.normal)

quantile(errors.train.normal)

threshold <- 1.0

# Make predictions on the abnormal test set.
preds.test.abnormal <- autoencoder %>% predict_on_batch(as.matrix(test.abnormal[,-c(1:2)]))

# Compute reconstruction errors.
errors.test.abnormal <- squared.errors(preds.test.abnormal, as.matrix(test.abnormal[,-c(1:2)]))

# Predict labels based on threshold 1:abnormal, 0:normal.
pred.labels.abnormal <- as.integer((errors.test.abnormal > threshold))

# Count how many abnormal instances were detected.
sum(pred.labels.abnormal)

# Make predictions on the normal test set.
preds.test.normal <- autoencoder %>% predict_on_batch(as.matrix(test.normal[,-c(1:2)]))

# Compute reconstruction errors.
errors.test.normal <- squared.errors(preds.test.normal, as.matrix(test.normal[,-c(1:2)]))

# Predict labels based on threshold 1:abnormal, 0:normal.
pred.labels.normal <- as.integer((errors.test.normal > threshold))

# Count how many normal instances were misclassified as abnormal (false positives).
sum(pred.labels.normal)


# Compute some metrics.
pred.labels.all <- as.factor(c(pred.labels.normal, pred.labels.abnormal))

gt.all <- c(rep(0,nrow(test.normal)), rep(1,nrow(test.abnormal)))

cm <- confusionMatrix(pred.labels.all, as.factor(gt.all), positive = "1")

# Print confusion matrix.
cm$table

# Print sensitivity
cm$byClass["Sensitivity"]

# Generate ROC curve with PRROC package.
scores.all <- c(errors.test.normal, errors.test.abnormal)

roc_obj <- roc.curve(scores.class0 = scores.all,
                       weights.class0 = gt.all, curve = TRUE)

#png("roc_curve_autoencoder.png", width = 6, height = 4, units = "in", res = 720)
plot(roc_obj)
#dev.off()
