# This script trains a series of Bagging and RF classifiers
# with increasing number of trees and plots the results.

source(file.path("..","auxiliary_functions","globals.R"))
source(file.path("..","auxiliary_functions","functions.R"))
library(caret)

# Package with implementations of decision trees
library(rpart)
library(randomForest)


# Define our bagging classifier.
my_bagging <- function(theFormula, data, ntrees = 10){
  
  N <- nrow(data)
  
  # A list to store the individual trees
  models <- list()
  
  # Train individual trees and add each to 'models' list.
  for(i in 1:ntrees){
    
    # Bootstrap instances from data.
    idxs <- sample(1:N, size = N, replace = T)
    
    bootstrappedInstances <- data[idxs,]
    
    treeModel <- rpart(as.formula(theFormula),
                       bootstrappedInstances,
                       xval = 0,
                       cp = 0)
    
    models <- c(models, list(treeModel))
  }
  
  res <- structure(list(models = models), class = "my_bagging")
  
  return(res)
}

# Define the predict function for my_bagging
predict.my_bagging <- function(object, newdata){
  
  ntrees <- length(object$models)
  N <- nrow(newdata)
  
  # Matrix to store predictions for each instance
  # in newdata and for each tree.
  M <- matrix(data = rep("",N * ntrees), nrow = N)
  
  # Populate matrix.
  # Each column of M contains all predictions for a given tree.
  # Each row contains the predictions for a given instance.
  for(i in 1:ntrees){
    
    m <- object$models[[i]]
    
    tmp <- as.character(predict(m, newdata, type = "class"))
    
    M[,i] <- tmp
  }
  
  # Final predictions
  predictions <- character()
  
  # Iterate through each row of M.
  for(i in 1:N){
    
    # Compute class counts
    classCounts <- table(M[i,])
    
    # Get the class with the most counts.
    predictions <- c(predictions, names(classCounts)[which.max(classCounts)])
  }
  
  return(predictions)
}

# Set path.
filepath <- file.path(datasets_path,
                      "smartphone_activities",
                      "WISDM_ar_v1.1_transformed.csv")

# Read data
df <- read.csv(filepath)

# Remove unique_id and user since we don't need those.
# The other variables are deleted since they contain missing values or just repeated values.
df <- df[, -which(colnames(df) %in% c("UNIQUE_ID", "user", "XAVG", "XPEAK", "YPEAK", "ZPEAK"))]

# Do some cleaning.
df$YAVG <- as.numeric(gsub("\\?", "", df$YAVG))

df$ZAVG <- as.numeric(gsub("\\?", "", df$ZAVG))

df$class <- as.factor(df$class)

set.seed(1234)

k <- 5

folds <- sample(k, size = nrow(df), replace = TRUE)

accuracies.bagging <- NULL
accuracies.rf <- NULL

for(nt in 1:50){

  print(paste0("Evaluating with ", nt, " trees."))
  
  # Variable to store ground truth classes.
  groundTruth <- NULL
  
  # Variable to store the classifier's predictions.
  predictions.bagging <- NULL
  
  predictions.rf <- NULL
  
  for(i in 1:k){
    
    trainSet <- df[which(folds != i), ]
    testSet <- df[which(folds == i), ]
    
    treeClassifier <- my_bagging(class ~ ., trainSet, ntree = nt)
    
    foldPredictions <- predict(treeClassifier, testSet)
    
    predictions.bagging <- c(predictions.bagging, as.character(foldPredictions))
    
    # RF predictions
    rf <- randomForest(class ~ ., trainSet, ntree = nt)
    
    foldPredictions <- predict(rf, testSet)
    
    predictions.rf <- c(predictions.rf, as.character(foldPredictions))
    
    groundTruth <- c(groundTruth, as.character(testSet$class))
  }
  
  cm <- confusionMatrix(as.factor(predictions.bagging), as.factor(groundTruth))
  
  cm.rf <- confusionMatrix(as.factor(predictions.rf), as.factor(groundTruth))
  
  accuracies.bagging <- c(accuracies.bagging, cm$overall["Accuracy"])
  
  accuracies.rf <- c(accuracies.rf, cm.rf$overall["Accuracy"])
}


tmp <- data.frame(ntrees = 1:length(accuracies.rf), type = "RF", accuracy = accuracies.rf)

tmp <- rbind(tmp, data.frame(ntrees = 1:length(accuracies.bagging), type = "Bagging", accuracy = accuracies.bagging))


acc.plot <- ggplot(data=tmp,
                   aes(x = ntrees, y = accuracy,
                   colour = type, group = type,
                   linetype = type)) +
  ggtitle("Bagging v.s RF test accuracy") + xlab("# trees") + ylab("Accuracy") +
  scale_linetype_manual(values=c("dotted", "solid"), name = "") +
  scale_color_manual(values=c("black","black"), name = "") +
  geom_line(aes(color=type), size=0.6) +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5),
        legend.position="right",
        legend.key.width = unit(1.0,"cm"),
        legend.key.size = unit(0.5,"cm"))

#png("iterated_bagging_rf.png", width = 5, height = 2.5, units = "in", res = 720)
print(acc.plot)
#dev.off()
