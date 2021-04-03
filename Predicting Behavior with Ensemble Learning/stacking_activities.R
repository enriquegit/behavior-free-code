source(file.path("..","auxiliary_functions","globals.R"))
source("stacking_algorithms.R")


# Set local path to file.
filepath <- file.path(datasets_path,
                      "home_tasks",
                      "sound_acc.csv")

# Read dataset.
dataset <- read.csv(filepath, stringsAsFactors = TRUE)

# Get column names.
colNames <- names(dataset)

# Store sound feature names. This is going to be view 1.
v1cols <- colNames[grep(colNames, pattern = "v1_")]

# Store accelerometer feature names. This is view 2.
v2cols <- colNames[grep(colNames, pattern = "v2_")]

# Class names.
levels <- as.character(unique(dataset$label))

# Number of folds for cross validation.
k <- 10

# Seed for reproducibility.
set.seed(1234)

# Generate cross validation folds.
folds <- sample(1:k, size = nrow(dataset), replace = T)

# Variables to store predictions.

reference <- NULL # Ground truth.
predicted.v1 <- NULL # Predictions from view 1 (sound)
predicted.v2 <- NULL # Predictions from view 2 (accelerometer)
predicted.stacking <- NULL # Predictions with stacking.

# Perform cross validation.
for(i in 1:k){
  
  print(paste0("fold = ",i))
  
  trainset <- dataset[folds != i, ]
  
  testset <- dataset[folds == i, ]
  
  groundTruth <- as.character(testset$label)
  
  reference <- c(reference, groundTruth)
  
  # Train a model with view 1 (sound).
  m.v1 <- randomForest(label ~., trainset[,c("label",v1cols)], nt = 100)
  pred.v1 <- as.character(predict(m.v1, newdata = testset[,v1cols], type = "class"))
  predicted.v1 <- c(predicted.v1, pred.v1)
  
  
  # Train a model with view 2 (accelerometer).
  m.v2 <- randomForest(label ~., trainset[,c("label",v2cols)], nt = 100)
  pred.v2 <- as.character(predict(m.v2, newdata = testset[,v2cols], type = "class"))
  predicted.v2 <- c(predicted.v2, pred.v2)
  
  # Train multi-view stacking model wiht both views.
  m.stacking <- mvstacking(trainset, v1cols, v2cols, k = 10)
  pred.stacking <- predict(m.stacking, newdata = testset[,-1])
  predicted.stacking <- c(predicted.stacking, pred.stacking)

} # end of cv


#### Results for view 1 ####
cm.v1 <- confusionMatrix(factor(predicted.v1, levels = levels),
                         reference = factor(reference, levels = levels))

# Print accuracy
cm.v1$overall["Accuracy"]

# Print other metrics per class.
cm.v1$byClass[,c("Recall", "Specificity", "Precision", "F1")]

# Print other metrics overall.
colMeans(cm.v1$byClass[,c("Recall", "Specificity", "Precision", "F1")])

#### Results for view 2 ####
cm.v2 <- confusionMatrix(factor(predicted.v2, levels = levels),
                         reference = factor(reference, levels = levels))

cm.v2$overall["Accuracy"]

cm.v2$byClass[,c("Recall", "Specificity", "Precision", "F1")]

colMeans(cm.v2$byClass[,c("Recall", "Specificity", "Precision", "F1")])

#### Results for stacking ####
cm.stacking <- confusionMatrix(factor(predicted.stacking, levels = levels),
                         reference = factor(reference, levels = levels))

cm.stacking$overall["Accuracy"]

cm.stacking$byClass[,c("Recall", "Specificity", "Precision", "F1")]

colMeans(cm.stacking$byClass[,c("Recall", "Specificity", "Precision", "F1")])


### Create table with results of each view independently and with multi-view stacking.

df <- as.data.frame(rbind(colMeans(cm.v1$byClass[,c("Recall", "Specificity", "Precision", "F1")]),
      colMeans(cm.v2$byClass[,c("Recall", "Specificity", "Precision", "F1")]),colMeans(cm.stacking$byClass[,c("Recall", "Specificity", "Precision", "F1")])))

row.names(df) <- c("Audio", "Accelerometer", "Multi-view Stacking")

df <- cbind(Accuracy=c(cm.v1$overall["Accuracy"],
                       cm.v2$overall["Accuracy"],
                       cm.stacking$overall["Accuracy"]), df)

# Save results so they can be printed in the book.
#save(df, file = "results_stacking.RData")

# Load results. 
#load("results_stacking.RData")

# Print table.
print(df)

### Plot confusion matrices.
library(gridExtra)

noClasses <- length(levels)
tamletras <- 7
tamnums <- 3

#view 1 (audio)
cm <- cm.v1
m <- cm$table
for(col in 1:noClasses){total <- sum(m[,col]);for(row in 1:noClasses){m[row,col] <- m[row,col] / total}}
m[is.na(m)] <- 0.0
z <- matrix(m, ncol=length(levels), byrow=TRUE, dimnames=list(levels,levels))
confusion <- as.data.frame(as.table(z))

plot1 <- ggplot()
plot1 <- plot1 + geom_tile(aes(x=Var1, y=Var2, fill=Freq), data=confusion, color="black", size=0.1) +
  labs(x=expression(atop("True Labels",bold("audio view"))), y=expression("Predicted Labels")) +
  geom_text(aes(x=Var1,y=Var2, label=sprintf("%.2f", Freq)),data=confusion, size=tamnums, colour="black") +
  scale_fill_gradient(low="white", high="gray", name="counts(%)")+
  theme(axis.text=element_text(colour="black", size =tamletras),
        axis.title = element_text(face="bold", colour="black", size=12),
        axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position="none",  plot.margin = unit(c(.1,.1,.1,.1),"cm"))

#view 2
cm <- cm.v2
m <- cm$table
for(col in 1:noClasses){total <- sum(m[,col]);for(row in 1:noClasses){m[row,col] <- m[row,col] / total}}
m[is.na(m)] <- 0.0
z <- matrix(m, ncol=length(levels), byrow=TRUE, dimnames=list(levels,levels))
confusion <- as.data.frame(as.table(z))

plot2 <- ggplot()
plot2 <- plot1 + geom_tile(aes(x=Var1, y=Var2, fill=Freq), data=confusion, color="black", size=0.1) +
  labs(x=expression(atop("True Labels",bold("accelerometer view"))), y=expression("Predicted Labels")) +
  geom_text(aes(x=Var1,y=Var2, label=sprintf("%.2f", Freq)),data=confusion, size=tamnums, colour="black") +
  theme(axis.text=element_text(colour="black", size =tamletras),
        axis.title = element_text(face="bold", colour="black", size=12),
        axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position="none")

#stacking views
cm <- cm.stacking
m <- cm$table
for(col in 1:noClasses){total <- sum(m[,col]);for(row in 1:noClasses){m[row,col] <- m[row,col] / total}}
m[is.na(m)] <- 0.0
z <- matrix(m, ncol=length(levels), byrow=TRUE, dimnames=list(levels,levels))
confusion <- as.data.frame(as.table(z))

plot3 <- ggplot()
plot3 <- plot1 + geom_tile(aes(x=Var1, y=Var2, fill=Freq), data=confusion, color="black", size=0.1) +
  labs(x=expression(atop("True Labels",bold("multi-view stacking"))), y=expression("Predicted Labels")) +
  geom_text(aes(x=Var1,y=Var2, label=sprintf("%.2f", Freq)),data=confusion, size=tamnums, colour="black") +
  theme(axis.text=element_text(colour="black", size =tamletras),
        axis.title = element_text(face="bold", colour="black", size=12),
        axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position="none")

#png("stacking_cm.png", width = 7, height = 6, units = "in", res = 720)
grid.arrange(plot1, plot2, plot3, ncol=2)
#dev.off()
