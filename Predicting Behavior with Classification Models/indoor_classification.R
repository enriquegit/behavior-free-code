source(file.path("..","auxiliary_functions","globals.R"))
source(file.path("indoor_auxiliary.R"))
library(caret)

# Path to the Wi-Fi dataset.
datapath <- file.path(datasets_path,"indoor_location","wifi.csv")

# Read Wi-Fi data
df <- read.csv(datapath, stringsAsFactors = F)

# Convert data frame into a list of lists where each inner list represents one instance.
dataset <- wifiScansToList(df)

# Print number of instances in the dataset.
length(dataset)

# Print the first instance in the list.
dataset[[1]]

# Lets' compute the jaccard distance between instances of the same class (bedroomA)
set1 <- dataset[[1]]$accessPoints$mac
set2 <- dataset[[4]]$accessPoints$mac
jaccardDistance(set1, set2)

# Now lets' compare between instances of different classes (bedroomA and bedroomB)
set1 <- dataset[[1]]$accessPoints$mac
set2 <- dataset[[210]]$accessPoints$mac
jaccardDistance(set1, set2)

# Total number of instances
numberInstances <- length(dataset)

# Set seed for reproducibility
set.seed(12345)

# Split into train and test sets.
trainSetIndices <- sample(1:numberInstances,
                          size = round(numberInstances * 0.7),
                          replace = F)

testSetIndices <- (1:numberInstances)[-trainSetIndices]

# Obtain predictions from the test set.
result <- knn_classifier(dataset, k = 3,
                         trainSetIndices,
                         testSetIndices)

# Calculate accuracy.
sum(result$predictions == result$groundTruth) /
  length(result$predictions)

# Compute confusion matrix.
cm <- confusionMatrix(factor(result$predictions),
                      factor(result$groundTruth))

cm$table # Access the confusion matrix.

# Plot Confusion matrix as proportions.
noClasses <- 4
letterSize <- 10
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

# Save to file.
#png("indoorCM.png", width = 4, height = 4, units = "in", res = 720)
print(plot1)
#dev.off()

# Print performance metrics.
cm$byClass[,c("Recall", "Specificity", "Precision", "F1")]

# Compute mean across classes.
colMeans(cm$byClass[,c("Recall", "Specificity", "Precision", "F1")])
