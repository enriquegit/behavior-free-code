source(file.path("..","auxiliary_functions","globals.R"))
source(file.path("..","auxiliary_functions","functions.R"))
library(caret)

# Package with implementations of decision trees
library(rpart)

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

set.seed(1234)

k <- 5

folds <- sample(k, size = nrow(df), replace = TRUE)

# Variable to store ground truth classes.
groundTruth <- NULL

# Variable to store the classifier's predictions.
predictions <- NULL

for(i in 1:k){
  
  trainSet <- df[which(folds != i), ]
  testSet <- df[which(folds == i), ]
  
  treeClassifier <- my_bagging(class ~ ., trainSet, ntree = 10)
  
  foldPredictions <- predict(treeClassifier, testSet)
  
  predictions <- c(predictions, as.character(foldPredictions))
  
  groundTruth <- c(groundTruth, as.character(testSet$class))
}

cm <- confusionMatrix(as.factor(predictions), as.factor(groundTruth))

# Print accuracy
cm$overall["Accuracy"]

# Print other metrics per class.
cm$byClass[,c("Recall", "Specificity", "Precision", "F1")]

# Print average performance metrics across classes.
colMeans(cm$byClass[,c("Recall", "Specificity", "Precision", "F1")])

# Plot confusion matrix.
noClasses <- 6
letterSize <- 8
numSize <- 6
m <- cm$table
levels <- colnames(m)

for(col in 1:noClasses){total <- sum(m[,col]);for(row in 1:noClasses){m[row,col] <- m[row,col] / total}}
m[is.na(m)] <- 0.0
z <- matrix(m, ncol=length(levels), byrow=TRUE, dimnames=list(levels,levels))
confusion <- as.data.frame(as.table(z))

plot1 <- ggplot()
plot1 <- plot1 + geom_tile(aes(x=Var1, y=Var2, fill=Freq), data=confusion, color="black", size=0.1) +   labs(x=expression(atop("True classes",bold(""))), y=expression("Predicted classes")) +
  geom_text(aes(x=Var1,y=Var2, label=sprintf("%.2f", Freq)),data=confusion,
            size=numSize, colour="black") +
  scale_fill_gradient(low="white", high="gray", name="counts(%)") +
  theme(axis.text=element_text(colour="black", size = letterSize), 
        axis.title = element_text(face="bold", colour="black", size=10),
        axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position="none",  plot.margin = unit(c(.1,.1,.1,.1),"cm"))

print(plot1)

# Plot first tree of the last fold.
# Note that it looks very complex since we set cp = 0.
library(rpart.plot)
rpart.plot(treeClassifier$models[[1]], fallen.leaves = F, branch.lty = 1, shadow.col = "gray", roundint=FALSE)
