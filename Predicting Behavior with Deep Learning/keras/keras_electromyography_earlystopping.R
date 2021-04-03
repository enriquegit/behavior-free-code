source(file.path("..","..","auxiliary_functions","globals.R"))
library(keras)

# Path to dataset.
path <- file.path(datasets_path,"electromyography")

# Read dataset.
dataset <- NULL

# Each class is saved in a different file so we append all.
for(i in 0:3){
  tmp <- read.csv(file.path(path,paste0(i,".csv")), header = F)
  dataset <- rbind(dataset,tmp)
}

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

# Define early stopping callback.
my_callback <- callback_early_stopping(monitor = "val_accuracy",
                                            min_delta = 0.01,
                                            patience = 50,
                                            verbose = 1,
                                            mode = "max",
                                            restore_best_weights = TRUE)

history <- model %>% fit(
  trainset$x, trainset$y,
  epochs = 500,
  batch_size = 8,
  validation_data = list(valset$x, valset$y),
  verbose = 1,
  view_metrics = TRUE,
  callbacks = list(my_callback)
)

# We need a small hack because in the last version of keras the
# plot() function throws an error because number of epochs
# is greater than the actual size of the data because
# it stopped earlier.
history$params$epochs <- length(history$metrics$loss)

plot(history)

# Evaluate model.
model %>% evaluate(testset$x, testset$y)
