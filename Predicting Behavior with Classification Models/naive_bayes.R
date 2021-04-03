source(file.path("..","auxiliary_functions","globals.R"))
library(caret)

# Path to the file: WISDM_ar_v1.1_transformed.csv which contains extracted features from the smartphones dataset.
datapath <- file.path(datasets_path,"smartphone_activities","WISDM_ar_v1.1_transformed.csv")

# Read data
df <- read.csv(datapath, stringsAsFactors = F)

# Remove unique_id and user since we don't need those.
# The other variables are deleted since they contain missing values or just repeated values.
df <- df[, -which(colnames(df) %in% c("UNIQUE_ID", "user", "XAVG", "XPEAK", "YPEAK", "ZPEAK"))]

# Do some cleaning.
df$YAVG <- as.numeric(gsub("\\?", "", df$YAVG))
df$ZAVG <- as.numeric(gsub("\\?", "", df$ZAVG))


#### Divide into train and test sets. ####

set.seed(1234) # Set seed for reproducibility.

# Select 70% of the data for the train set.
idxs <- sample(nrow(df), size = ceiling(nrow(df) * 0.7), replace = FALSE)

trainset <- df[idxs,]

# 30% of the data goes to the test set.
testset <- df[-idxs,]

#### Example of Naive Bayes with 1 feature ####

# Compute prior probabilities.
priors <- table(trainset$class) / nrow(trainset)

# Print the table of priors.
priors

# Get the prior for "Jogging".
priors["Jogging"]

# Probability density function of normal distribution.
f <- function(x, m, s){
  
  (1 / (sqrt(2*pi)*s)) * exp(-((x-m)^2) / (2 * s^2))
  
}

# Compute the mean and sd of
# the feature RESULTANT (column 39)
# when the class = "Standing".
mean.standing <- mean(trainset[which(trainset$class == "Standing"), 39])
sd.standing <- sd(trainset[which(trainset$class == "Standing"), 39])

# Compute mean and sd when
# the class = "Jogging".
mean.jogging <- mean(trainset[which(trainset$class == "Jogging"), 39])
sd.jogging <- sd(trainset[which(trainset$class == "Jogging"), 39])

# Select a query instance from the test set.
query <- testset[1,] # Select the first one.

# Compute P(Standing)P(RESULTANT|Standing)
priors["Standing"] * f(query$RESULTANT, mean.standing, sd.standing)

# Compute P(Jogging)P(RESULTANT|Jogging)
priors["Jogging"] * f(query$RESULTANT, mean.jogging, sd.jogging)

# Inspect the true class of the query instance.
query$class


#####################################################
#### Implement Naive Bayes for multiple features ####
#####################################################

# Function to learn the parameters of 
# a Naive Bayes classifier.
# Assumes that the last column of data is the class.
naive.bayes.train <- function(data){

  # Unique classes.
  classes <- unique(data$class)

  # Number of features.
  nfeatures <- ncol(data) - 1
  
  # List to store the learned means and sds.
  list.means.sds <- list()
  
  for(c in classes){
    
    # Matrix to store the mean and sd for each feature.
    # First column stores the mean and second column
    # stores the sd.
    M <- matrix(0, nrow = nfeatures, ncol = 2)
    
    # Populate matrix.
    for(i in 1:nfeatures){
      feature.values <- data[which(data$class == c),i]
      M[i,1] <- mean(feature.values)
      M[i,2] <- sd(feature.values)
    }
    
    list.means.sds[c] <- list(M)
  }

  # Compute prior probabilities.
  priors <- table(data$class) / nrow(data)
  
  return(list(list.means.sds=list.means.sds,
              priors=priors))
}

# Function to make predictions using
# the learned parameters.
naive.bayes.predict <- function(params, data){
  
  # Variable to store the prediction for each instance.
  predictions <- NULL
  
  n <- nrow(data)
  
  # Get class names.
  classes <- names(params$priors)
  
  # Get number of features.
  nfeatures <- nrow(params$list.means.sds[[1]])
  
  # Iterate instances.
  for(i in 1:n){
    
    query <- data[i,]
    
    max.probability <- -Inf
    
    predicted.class <- ""
    
    # Find the class with highest probability.
    for(c in classes){

      # Get the prior probability for class c.
      acum.prob <- params$priors[c]
      
      # Fix to avoid loss of precision.
      #acum.prob <- log(params$priors[c])
      
      # Iterate features.
      for(j in 1:nfeatures){
        
        # Compute P(feature|class)
        tmp <- f(query[,j],
          params$list.means.sds[[c]][j,1],
          params$list.means.sds[[c]][j,2])
        
        # Accumulate result.
        acum.prob <- acum.prob * tmp
        
        # Fix to avoid loss of precision.
        #acum.prob <- acum.prob + log(tmp)
      }
      
      if(acum.prob > max.probability){
        max.probability <- acum.prob
        predicted.class <- c
      }
    }
    
    predictions <- c(predictions, predicted.class)
  }
  
  return(predictions)
}

### Try our implementation of Naive Bayes ###

# Learn Naive Bayes parameters.
nb.model <- naive.bayes.train(trainset)

# Make predictions.
predictions <- naive.bayes.predict(nb.model, testset)

# Compute confusion matrix and other performance metrics.
groundTruth <- testset$class

cm <- confusionMatrix(as.factor(predictions),
                      as.factor(groundTruth))

# Print accuracy
cm$overall["Accuracy"]

# Print overall metrics across classes.
colMeans(cm$byClass[,c("Recall", "Specificity",
                       "Precision", "F1")])


#### Use Naive Bayes implementation from package e1071 ####
library(e1071)

# We need to convert the class into a factor.
trainset$class <- as.factor(trainset$class)

nb.model2 <- naiveBayes(class ~., trainset)

predictions2 <- predict(nb.model2, testset)

cm2 <- confusionMatrix(as.factor(predictions2),
                      as.factor(groundTruth))

# Print accuracy
cm2$overall["Accuracy"]

# Print overall metrics across classes.
colMeans(cm2$byClass[,c("Recall", "Specificity",
                       "Precision", "F1")])




####################
#### Plots code ####
####################

# Plot normal (Gaussian) pdf.

x <- seq(-5, 15, length=200)

values <- dnorm(x, mean = 5, sd = 3)

#png("pdf1.png", width = 5.5, height = 4, units = "in", res = 720)
plot(x, values, type="l", lty=1, xlab="x", col="blue",
     ylab="Density", main="")
points(1.7, dnorm(x=1.7, mean = 5, sd = 3), col="red", pch=19, cex=1.0)
#dev.off()
