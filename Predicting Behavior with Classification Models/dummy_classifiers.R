source(file.path("..","auxiliary_functions","globals.R"))
library(caret)
library(rpart)

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

#### Make this dataset imbalanced ####

# Only keep the 'Walking' and 'Upstairs' classes
# and only select a small percent of the 'Upstairs' instances.

dataset <- df[df$class=="Walking",]
dataset <- rbind(dataset,
                 df[sample(which(df$class=="Upstairs"), size = 200),])

# Store the two possible classes.
levels <- c("Walking", "Upstairs")

set.seed(1234) # Set seed for reproducibility.

# Shuffle rows.
dataset <- dataset[sample(nrow(dataset), size = nrow(dataset), replace = F),]

# Print class counts.
table(dataset$class)

# In percentages.
table(dataset$class) / nrow(dataset)

#### Divide into train and test sets. ####


# Select 50% of the data for the train set.
idxs <- sample(nrow(dataset), size = ceiling(nrow(dataset) * 0.5), replace = FALSE)

trainset <- dataset[idxs,]

# 50% of the data goes to the test set.
testset <- dataset[-idxs,]


########################################
#### Most-frequent-class classifier ####
########################################

# Define the dummy classifier's train function.
most.frequent.class.train <- function(data){
  
  # Get a table with the class counts.
  counts <- table(data$class)
  
  # Select the label with the most counts.
  most.frequent <- names(which.max(counts))
  
  return(most.frequent)
}

# Define the dummy classifier's predict function.
most.frequent.class.predict <- function(params, data){

  # Return the same label for as many rows as there are in data.
  return(rep(params, nrow(data)))
  
}

# Learn the parameters.
dummy.model1 <- most.frequent.class.train(trainset)

# Print the learned parameter.
dummy.model1

# Make predictions.
predictions <- most.frequent.class.predict(dummy.model1, testset)

# Compute confusion matrix and other performance metrics.
cm <- confusionMatrix(factor(predictions, levels),
                       factor(testset$class, levels))

# Print accuracy
cm$overall["Accuracy"]

# Print confusion matrix.
cm$table

### Let's try with a decision tree ###
treeClassifier <- rpart(class ~ ., trainset)

tree.predictions <- predict(treeClassifier, testset, type = "class")

cm.tree <- confusionMatrix(factor(tree.predictions, levels),
                      factor(testset$class, levels))

# Print accuracy
cm.tree$overall["Accuracy"]

# Print confusion matrix.
cm.tree$table

########################################
########## Uniform classifier ##########
########################################

# Define the dummy classifier's train function.
uniform.train <- function(data){
  
  # Get the unique classes.
  unique.classes <- unique(data$class)
  
  return(unique.classes)
}

# Define the dummy classifier's predict function.
uniform.predict <- function(params, data){
  
  # Sample classes uniformly.
  return(sample(params, size = nrow(data), replace = T))
  
}

dummy.model2 <- uniform.train(trainset)

predictions2 <- uniform.predict(dummy.model2, testset)

# Print prediction counts.
table(predictions2)

cm2 <- confusionMatrix(factor(predictions2, levels),
                      factor(testset$class, levels))

# Print accuracy
cm2$overall["Accuracy"]

# Print confusion matrix.
cm2$table

########################################
###### Frequency-based classifier ######
########################################

# Define the dummy classifier's train function.
frequency.train <- function(data){

  frequencies <- table(data$class) / nrow(data)
  
  return(frequencies)
}

# Define the dummy classifier's predict function.
frequency.predict <- function(params, data){
  
  predictions <- sample(names(params),
                        size = nrow(data),
                        prob = params,
                        replace = T)
  
  return(predictions)
  
}

dummy.model3 <- frequency.train(trainset)

predictions3 <- frequency.predict(dummy.model3, testset)

# Print prediction counts.
table(predictions3)

cm3 <- confusionMatrix(factor(predictions3, levels),
                       factor(testset$class, levels))

# Print accuracy
cm3$overall["Accuracy"]

# Print confusion matrix.
cm3$table

