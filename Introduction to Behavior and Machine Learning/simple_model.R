
#### Generate a synthetic dataset of tiger and leopard speeds ####

# Number of points per class.
n <- 50

# Set seed for reproducibility.
set.seed(1234)

# Generate random tiger speeds in km/hr.
tiger.speed <- rnorm(n, mean = 50, sd = 3)

# Generate random leopard speeds in km/hr.
leopard.speed <- rnorm(n, mean = 54, sd = 3)

dataset <- data.frame(speed = c(tiger.speed, leopard.speed),
                      class = c(rep("tiger",n), rep("leopard",n)),
                      stringsAsFactors = TRUE)

# Shuffle rows.
dataset <- dataset[sample(nrow(dataset)),]

#############################
#### Explore the dataset ####
#############################

# Print number of rows and columns.
dim(dataset)

# Count instances in each class.
table(dataset$class)

# Compute some summary statistics.
summary(dataset)

# Plot data points.
#png("felineSpeeds.png", width = 7, height = 3, units = "in", res = 200)

plot(dataset$speed, rep(1, n*2),
     col=as.integer(dataset$class)+2,
     pch=as.integer(dataset$class),
     cex = 2,
     yaxt = 'n',
     xlab = "Speed",
     ylab = "",
     ylim = c(1,1.5),
     main = "Tiger and leopard speeds")

# Plot vertical lines at the mean.
abline(v = mean(tiger.speed), col = 4, lty = 2)
abline(v = mean(leopard.speed), col = 3, lty = 2)

# Add a legend.
legend("topleft", c("tiger","leopard"),
       pch = c(2,1),
       col=c(4,3))

#abline(v = mean(tiger.speed) +
#         (mean(leopard.speed)-mean(tiger.speed))/
#         2, col = "red", lty = 2)
#text(51.5,1.47,"decision\nboundary", cex=0.7, col="red")

#dev.off()

# Define a simple classifier that learns
# a centrality measure for each class.
simple.model.train <- function(data, centrality=mean){
  
  # Store unique classes.
  classes <- unique(data$class)
  
  # Define an array to store the learned parameters.
  params <- numeric(length(classes))
  
  # Make this a named array.
  names(params) <- classes
  
  # Iterate through each class and compute its centrality measure.
  for(c in classes){
    
    # Filter instances by class.
    tmp <- data[which(data$class == c),]
    
    # Compute the centrality measure.
    centrality.measure <- centrality(tmp$speed)
    
    # Store the centrality measure for this class.
    params[c] <- centrality.measure
  }
  
  return(params)
  
}

# Define a function that predicts a class
# based on the learned parameters.
simple.classifier.predict <- function(newdata, params){
  
  # Variable to store the predictions of
  # each instance in newdata.
  predictions <- NULL
  
  # Iterate instances in newdata
  for(i in 1:nrow(newdata)){
    
    instance <- newdata[i,]
    
    # Predict the name of the class whose
    # centrality measure is the closest.
    pred <- names(which.min(abs(instance$speed - params)))
    
    predictions <- c(predictions, pred)
  }
  
  return(predictions)
}

###############################
#### Test the simple model ####
###############################

#### Holdout validation ####

# Percent to be used as training data.
pctTrain <- 0.7

# Set seed for reproducibility.
set.seed(123)

idxs <- sample(nrow(dataset),
               size = nrow(dataset) * pctTrain,
               replace = FALSE)

trainset <- dataset[idxs,]

testset <- dataset[-idxs,]

# Train the model using the trainset.
params <- simple.model.train(trainset, mean)

# Print the learned parameters.
print(params)

# Predict classes on the test set.
test.predictions <- simple.classifier.predict(testset, params)

# Display first predictions.
head(test.predictions)

# Compute test accuracy.
sum(test.predictions == as.character(testset$class)) /
  nrow(testset)

# Compute train accuracy.
train.predictions <- simple.classifier.predict(trainset, params)
sum(train.predictions == as.character(trainset$class)) /
  nrow(trainset)


#### k-fold cross validation ####

# Number of folds.
k <- 5

set.seed(123)

# Generate random folds.
folds <- sample(k,
                size = nrow(dataset),
                replace = TRUE)

# Print how many instances ended up in each fold.
table(folds)

# Variable to store accuracies on each fold.
test.accuracies <- NULL; train.accuracies <- NULL

for(i in 1:k){
  
  testset <- dataset[which(folds == i),]
  trainset <- dataset[which(folds != i),]
  
  params <- simple.model.train(trainset, mean)
  
  test.predictions <- simple.classifier.predict(testset, params)
  
  train.predictions <- simple.classifier.predict(trainset, params)
  
  # Accuracy on test set.
  acc <- sum(test.predictions == 
               as.character(testset$class)) /
    nrow(testset)
  
  test.accuracies <- c(test.accuracies, acc)
  
  # Accuracy on train set.
  acc <- sum(train.predictions == 
               as.character(trainset$class)) /
    nrow(trainset)
  
  train.accuracies <- c(train.accuracies, acc)
}

# Print mean accuracy across folds on the test set.
mean(test.accuracies)

# Print mean accuracy across folds on the train set.
mean(train.accuracies)

#######################################
#### Classification with 3 classes ####
#######################################

set.seed(123)

# Generate random jaguar speeds in km/hr.
jaguar.speed <- rnorm(n, mean = 60, sd = 3)

# Generate dataset with 3 classes.
dataset3classes <- rbind(dataset,
                         data.frame(speed=jaguar.speed,
                                    class="jaguar"))

# Shuffle rows.
dataset3classes <- dataset3classes[sample(nrow(dataset3classes)),]

# Percent to be used as training data.
pctTrain <- 0.7

idxs <- sample(nrow(dataset3classes),
               size = nrow(dataset3classes) * pctTrain,
               replace = FALSE)

trainset <- dataset3classes[idxs,]

testset <- dataset3classes[-idxs,]

# Train the model using the trainset.
params <- simple.model.train(trainset, mean)

# Print the learned parameters.
print(params)

# Predict classes on the test set.
predictions <- simple.classifier.predict(testset, params)

# Display first predictions.
head(predictions)

# Compute accuracy.
sum(predictions == as.character(testset$class)) / nrow(testset)


###########################
#### Simple regression ####
###########################

# Define a function that predicts speed
# based on the type of feline.
simple.regression.predict <- function(newdata, params){
  
  # Variable to store the predictions of
  # each instance in newdata.
  predictions <- NULL
  
  # Iterate instances in newdata
  for(i in 1:nrow(newdata)){
    
    instance <- newdata[i,]
    
    # Return the mean value of the corresponding class stored in params.
    pred <- params[which(names(params) == instance$class)]
    
    predictions <- c(predictions, pred)
  }
  
  return(predictions)
}

pctTrain <- 0.7

set.seed(123)

idxs <- sample(nrow(dataset),
               size = nrow(dataset) * pctTrain,
               replace = FALSE)

trainset <- dataset[idxs,]

testset <- dataset[-idxs,]

# Reuse our train function.
params <- simple.model.train(trainset, mean)

print(params)

# Predict speeds on the test set.
test.predictions <- simple.regression.predict(testset, params)

# Print first predictions.
head(test.predictions)

# Compute mean absolute error (MAE) on the test set.
mean(abs(test.predictions - testset$speed))

# Predict speeds on the train set.
train.predictions <- simple.regression.predict(trainset, params)

# Print first predictions.
head(train.predictions)

# Compute mean absolute error (MAE) on the train set.
mean(abs(train.predictions - trainset$speed))
