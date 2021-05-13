source(file.path("..","..","auxiliary_functions","globals.R"))
library(keras)
library(caret)

# Path to dataset directory.
pathdir <- file.path(datasets_path,
                     "electromyography/")

# Read dataset.
dataset <- NULL

# Each class is saved in a different file so we append all.
for(i in 0:3){
  tmp <- read.csv(file.path(pathdir,paste0(i,".csv")), header = F)
  dataset <- rbind(dataset,tmp)
}

# Check the class distribution.
table(dataset$V65)

# Shuffle rows.
set.seed(1234)
dataset <- dataset[sample(nrow(dataset), size = nrow(dataset)),]

# Split data. train 60%, validation 10%, test 30%.
group <- sample(1:3, size = nrow(dataset),
                replace = TRUE,
                prob = c(0.6, 0.10, 0.30))

trainset <- dataset[which(group == 1),]
valset <- dataset[which(group == 2),]
testset <- dataset[which(group == 3),]

# Write a function to normalize the variables.
normalize <- function(trainset, valset, testset){
  # Function to normalize the train, validation and test set
  # based on the parameters learned from the trainset.
  
  # Iterate columns except the last one which is the class.
  for(i in 1:(ncol(trainset)-1)){
    
    c1 <- trainset[,i] # trainset column
    c2 <- valset[,i] # valset column
    c3 <- testset[,i] # testset column
    
    max <- max(c1, na.rm = T) # Learn the max value from the trainset's column.
    min <- min(c1, na.rm = T) # Learn the min value from the trainset's column.
    
    if(max==min){ # If all values are the same set it to max.
      trainset[,i] <- max
      valset[,i] <- max
      testset[,i] <- max
    }
    else{
      # Normalize trainset's column.
      trainset[,i] <- (c1 - min) / (max - min)
      
      # Truncate max values in validation and test set.
      idxs <- which(c2 > max)
      if(length(idxs) > 0){
        c2[idxs] <- max
      }
      
      idxs <- which(c3 > max)
      if(length(idxs) > 0){
        c3[idxs] <- max
      }
      
      # Truncate min values in validation and test set.
      idxs <- which(c2 < min)
      if(length(idxs) > 0){
        c2[idxs] <- min
      }
      
      idxs <- which(c3 < min)
      if(length(idxs) > 0){
        c3[idxs] <- min
      }
      
      # Normalize validation and test set column.
      valset[,i] <- (c2 - min) / (max - min)
      testset[,i] <- (c3 - min) / (max - min)
    }
  }
  return(list(train=trainset, val=valset, test=testset))
}

# Normalize data.
res <- normalize(trainset, valset, testset)

trainset <- res$train
valset <- res$val
testset <- res$test

# Define a function to format features and one-hot encode the class.
format.to.array <- function(data, numclasses = 4){
  
  x <- as.matrix(data[, 1:(ncol(data)-1)])
  y <- as.array(data[, ncol(data)])
  y <- to_categorical(y, num_classes = numclasses)
  l <- list(x=x, y=y)
  
  return(l)
}

# Format data
trainset <- format.to.array(trainset, numclasses = 4)
valset <- format.to.array(valset, numclasses = 4)
testset <- format.to.array(testset, numclasses = 4)

# Display first classes from train set.
head(trainset$y)

# Define the network's architecture.
get.nn <- function(ninputs = 64, nclasses = 4, lr = 0.01){
  
  model <- keras_model_sequential()
  
  model %>%
    layer_dense(units = 32, activation = 'relu', input_shape = ninputs) %>% 
    layer_dense(units = 16, activation = 'relu') %>%
    layer_dense(units = nclasses, activation = 'softmax')
  
  model %>% compile(
    loss = 'categorical_crossentropy',
    optimizer = optimizer_sgd(lr = lr),
    metrics = c('accuracy')
  )
  
  return(model)
}

model <- get.nn(64, 4, lr = 0.01)

summary(model)

history <- model %>% fit(
  trainset$x, trainset$y,
  epochs = 300,
  batch_size = 8,
  validation_data = list(valset$x, valset$y),
  verbose = 1,
  view_metrics = TRUE
)

plot(history)

# Save model (optional).
#save_model_hdf5(model, "electromyography.hdf5")
#save_model_tf(model, "electromyography_tf")

# Load model (optional).
#model <- load_model_hdf5("electromyography.hdf5")
#model <- load_model_tf("electromyography_tf")

# Evaluate model.
model %>% evaluate(testset$x, testset$y)

# Predict classes.
classes <- model %>% predict_classes(testset$x)

head(classes)

# Make predictions on the test set.
predictions <- model %>% predict_on_batch(testset$x)

head(predictions)

classes <- max.col(predictions) - 1

head(classes)

groundTruth <- max.col(testset$y) - 1

# Compute accuracy.
sum(classes == groundTruth) / length(classes)

# Convert classes to strings.
# Class mapping by index: rock 0, scissors 1, paper 2, ok 3.
mapping <- c("rock", "scissors", "paper", "ok")

str.predictions <- mapping[classes+1] # Need to add 1 because indices in R start at 1.

str.groundTruth <- mapping[groundTruth+1]

cm <- confusionMatrix(as.factor(str.predictions), as.factor(str.groundTruth))

cm$table
