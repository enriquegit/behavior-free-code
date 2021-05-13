# This script trains a series of  Random Forest classifiers
# with increasing number of trees and plots the results.

source(file.path("..","auxiliary_functions","globals.R"))
source(file.path("..","auxiliary_functions","functions.R"))
library(caret)

# Package with implementations of Random Forest
library(randomForest)

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

# Convert class to factor.
df$class <- as.factor(df$class)

set.seed(1234)

k <- 5

folds <- sample(k, size = nrow(df), replace = TRUE)

accuracies <- NULL
accuracies.train <- NULL

for(nt in 1:50){

  print(paste0("Evaluating with ", nt, " trees."))
  
  # Variable to store ground truth classes.
  groundTruth <- NULL
  
  # Variable to store the classifier's predictions.
  predictions <- NULL
  
  # Train set accuracies.
  groundTruth.train <- NULL
  predictions.train <- NULL
  
  for(i in 1:k){
    
    trainSet <- df[which(folds != i), ]
    testSet <- df[which(folds == i), ]
    
    rfClassifier <- randomForest(class ~ ., trainSet, ntree = nt)
    
    foldPredictions <- predict(rfClassifier, testSet)
    
    predictions <- c(predictions, as.character(foldPredictions))
    
    # Training set predictions
    foldPredictions <- predict(rfClassifier, trainSet)
    
    predictions.train <- c(predictions.train, as.character(foldPredictions))
    
    groundTruth <- c(groundTruth, as.character(testSet$class))
    
    groundTruth.train <- c(groundTruth.train, as.character(trainSet$class))
  }
  
  cm <- confusionMatrix(as.factor(predictions), as.factor(groundTruth))
  
  cm.train <- confusionMatrix(as.factor(predictions.train), as.factor(groundTruth.train))
  
  accuracies <- c(accuracies, cm$overall["Accuracy"])
  
  accuracies.train <- c(accuracies.train, cm.train$overall["Accuracy"])
}

tmp <- data.frame(ntrees = 1:length(accuracies), type = "train", accuracy = accuracies.train)

tmp <- rbind(tmp, data.frame(ntrees = 1:length(accuracies), type = "test", accuracy = accuracies))


acc.plot <- ggplot(data=tmp,
                   aes(x = ntrees, y = accuracy,
                   colour = type, group = type,
                   linetype = type)) +
  ggtitle("Random Forest Train/Test accuracy") + xlab("# trees") + ylab("Accuracy") +
  scale_linetype_manual(values=c("dotted", "solid"), name = "") +
  scale_color_manual(values=c("red","blue"), name = "") +
  geom_line(aes(color=type), size=0.6) +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5),
        legend.position="right",
        legend.key.width = unit(1.0,"cm"),
        legend.key.size = unit(0.5,"cm"))

#png("iterated_rf.png", width = 5, height = 2.5, units = "in", res = 720)
print(acc.plot)
#dev.off()
