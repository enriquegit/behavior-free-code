source(file.path("..","auxiliary_functions","globals.R"))
source(file.path("..","auxiliary_functions","functions.R"))
library(caret)

# Package with implementation of random forest
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

# Variable to store ground truth classes.
groundTruth <- NULL

# Variable to store the classifier's predictions.
predictions <- NULL

for(i in 1:k){
  
  trainSet <- df[which(folds != i), ]
  testSet <- df[which(folds == i), ]
  
  rf <- randomForest(class ~ ., trainSet, ntree = 10)
  
  foldPredictions <- predict(rf, testSet)
  
  predictions <- c(predictions, as.character(foldPredictions))
  
  groundTruth <- c(groundTruth, as.character(testSet$class))
}

cm <- confusionMatrix(as.factor(predictions), as.factor(groundTruth))

# Print accuracy
cm$overall["Accuracy"]

# Print other metrics per class.
cm$byClass[,c("Recall", "Specificity", "Precision", "F1")]

# Print other metrics overall.
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
