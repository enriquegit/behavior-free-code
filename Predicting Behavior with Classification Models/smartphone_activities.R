source(file.path("..","auxiliary_functions","globals.R"))
source(file.path("..","auxiliary_functions","functions.R"))
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

# Print first rows of the dataset.
head(df[,c(1:5,40)])

# Package with implementations of decision trees
library(rpart)

# Package to plot functions for rpart trees
library(rpart.plot)

set.seed(1234)

k <- 10

folds <- sample(k, size = nrow(df), replace = TRUE)

# Print first 10 values.
head(folds)

# Variable to store ground truth classes.
groundTruth <- NULL

# Variable to store the classifier's predictions.
predictions <- NULL

for(i in 1:k){
  
  trainSet <- df[which(folds != i), ]
  testSet <- df[which(folds == i), ]
  
  # Train the decision tree
  treeClassifier <- rpart(class ~ .,
                          trainSet, xval=0)
  
  # Get predictions on the test set.
  foldPredictions <- predict(treeClassifier,
                             testSet, type = "class")
  
  predictions <- c(predictions,
                   as.character(foldPredictions))
  
  groundTruth <- c(groundTruth,
                   as.character(testSet$class))
}

# Compute confusion matrix and other performance metrics.
cm <- confusionMatrix(as.factor(predictions),
                      as.factor(groundTruth))

# Print accuracy
cm$overall["Accuracy"]

# Print other metrics per class.
cm$byClass[,c("Recall", "Specificity", "Precision", "F1")]

# Print overall metrics across classes.
colMeans(cm$byClass[,c("Recall", "Specificity",
                       "Precision", "F1")])

# Plot confusion matrix.
noClasses <- 6
letterSize <- 7
numSize <- 5
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

#png("activitiesTreeCM.png", width = 4, height = 4, units = "in", res = 720)
print(plot1)
#dev.off()

# Plot tree of the last fold.
#png("activitiesTree.png", width = 5.5, height = 4, units = "in", res = 720)
rpart.plot(treeClassifier, fallen.leaves = F, shadow.col = "gray", legend.y = 1)
#dev.off()

# Prior probabilities.
table(trainSet$class) / nrow(trainSet)
