source(file.path("..","auxiliary_functions","globals.R"))
source(file.path("..","auxiliary_functions","functions.R"))
library(randomForest)
library(caret)

# Load UTD-MHAD dataset.

# Path to the csv file containing the extracted features.
# preprocess_skeleton_actins.R contains the code used to extract the features.
filepath <- file.path(datasets_path,"skeleton_actions","dataset.csv")

# Load dataset.
dataset <- read.csv(filepath, stringsAsFactors = T)

# Print summary.
summary(dataset)

# Extract unique labels.
unique.actions <- as.character(unique(dataset$label))

# Print the unique labels.
print(unique.actions)

##################################
########## Mixed Model ###########
##################################

k <- 10 # Number of folds.

set.seed(1234)

folds <- sample(k, nrow(dataset), replace = TRUE)

# Variables to store performance metrics.
accuracies <- NULL; recalls <- NULL; precisions <- NULL

# Perform k-fold cross-validation.
for(i in 1:k){
  
  trainset <- dataset[which(folds != i,),]
  testset <- dataset[which(folds == i,),]
  
  #Normalize.
  # Not really needed here since trees are not sensitive to different scales.
  res <- normalize(trainset, testset)
  trainset <- res$train
  testset <- res$test
    
  rf <- randomForest(label ~., trainset[,-1])
  
  preds.rf <- as.character(predict(rf, newdata = testset[,-1]))
  
  groundTruth <- as.character(testset$label)
  
  cm.rf <- confusionMatrix(factor(preds.rf, levels = unique.actions),
                           factor(groundTruth, levels = unique.actions))
  
  accuracies <- c(accuracies, cm.rf$overall["Accuracy"])
  
  metrics <- colMeans(cm.rf$byClass[,c("Recall",
                                       "Specificity",
                                       "Precision",
                                       "F1")],
                      na.rm = TRUE)
  
  recalls <- c(recalls, metrics["Recall"])
  
  precisions <- c(precisions, metrics["Precision"])
}

# Print performance metrics.

mean(accuracies)

mean(recalls)

mean(precisions)


##################################
##### User Independent Model #####
##################################

# Get a list of unique users.
unique.users <- as.character(unique(dataset$userid))

# Print the unique user ids.
unique.users

accuracies <- NULL; recalls <- NULL; precisions <- NULL

set.seed(1234)

for(user in unique.users){
  
  testset <- dataset[which(dataset$userid == user),]
  trainset <- dataset[which(dataset$userid != user),]
  
  # Normalize. Not really needed since RF
  # is not affected by different features scale.
  res <- normalize(trainset, testset)
  trainset <- res$train
  testset <- res$test
  
  rf <- randomForest(label ~., trainset[,-1])
  
  preds.rf <- as.character(predict(rf, newdata = testset[,-1]))
  
  groundTruth <- as.character(testset$label)
  
  cm.rf <- confusionMatrix(factor(preds.rf, levels = unique.actions),
                           factor(groundTruth, levels = unique.actions))
  
  
  accuracies <- c(accuracies, cm.rf$overall["Accuracy"])
  
  metrics <- colMeans(cm.rf$byClass[,c("Recall", "Specificity", "Precision", "F1")],
           na.rm = TRUE)
  
  recalls <- c(recalls, metrics["Recall"])
  
  precisions <- c(precisions, metrics["Precision"])
  
}

# Average performance across users.

mean(accuracies)

mean(recalls)

mean(precisions)

##################################
###### User Dependent Model ######
##################################

unique.users <- as.character(unique(dataset$userid))

unique.users

accuracies <- NULL; recalls <- NULL; precisions <- NULL

set.seed(1234)

# Iterate through each user.
for(user in unique.users){
  
  print(paste0("Evaluating user ", user))
  
  user.data <- dataset[which(dataset$userid == user), -1]
  
  # Leave-one-out cross validation within each user.
  
  predictions.rf <- NULL; groundTruth <- NULL
  
  for(i in 1:nrow(user.data)){
    
    # Normalize. Not really needed since RF
    # is not affected by different features scale.
    testset <- user.data[i,]
    trainset <- user.data[-i,]
    res <- normalize(trainset, testset)
    
    testset <- res$test
    trainset <- res$train
    
    rf <- randomForest(label ~., trainset)
    
    preds.rf <- as.character(predict(rf, newdata = testset))
    
    predictions.rf <- c(predictions.rf, preds.rf)
    
    groundTruth <- c(groundTruth, as.character(testset$label))
  }
  
  cm.rf <- confusionMatrix(factor(predictions.rf, levels = unique.actions),
                           factor(groundTruth, levels = unique.actions))
  
  
  accuracies <- c(accuracies, cm.rf$overall["Accuracy"])
  
  metrics <- colMeans(cm.rf$byClass[,c("Recall", "Specificity", "Precision", "F1")],
                      na.rm = TRUE)
  
  recalls <- c(recalls, metrics["Recall"])
  
  precisions <- c(precisions, metrics["Precision"])
} # end of users iteration.


# Print average performance across users.

mean(accuracies)

mean(recalls)

mean(precisions)
