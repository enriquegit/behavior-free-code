source(file.path("..","auxiliary_functions","globals.R"))
source(file.path("hand_gestures_auxiliary.R"))

# Load library for dynamic time warping.
library(dtw)
library(caret)

# *******************************************************************
# ************* Explore data and perform classification *************
# *******************************************************************

# Path to dataset.
datadir <- file.path(datasets_path,"hand_gestures")

# Format instances from files.
# The gen.instances() function is implemented
# in hand_gestures_auxiliary.R
instances <- gen.instances(datadir)

# Use first instance as the query.
query <- instances[[1]]

# Use second instance as the reference.
ref <- instances[[2]]

# Print their respective classes
print(query$type)
print(ref$type)

# Print values.
print(query$values)

# Plot query.
plot(query$values, type = "l")

# Compute dynamic time warping.
alignment <- dtw(query$values, ref$values, keep = TRUE)

# Print the distance.
alignment$distance

# Print the normalized distance.
alignment$normalizedDistance

# Query is in black color.
#png("alignmentExample.png", width = 6, height = 4, units = "in", res = 720)
plot(alignment, type="two", off=1, match.lty=2, match.indices=40,
     main="DTW resulting alignment", xlab="time", ylab="magnitude")
#dev.off()

##################################################################################
#### Classify gestures and evaluate performance using 10-fold cross-validation ###
##################################################################################

# Compute distances and save them. This can take some time so the result is already
# stored in D.RData. which can be loaded below.
D <- matrix.distances(instances)
save(D, file="D.RData")

# Load matrix with pair distances.
load("D.RData")

set.seed(1234)

k <- 10 # Number of folds.

# Generate folds.
folds <- sample(k, size = length(D[[1]]), replace = T)

# Variables to store predictions and ground truth.
predictions <- NULL
groundTruth <- NULL

# Implement k-nn with k=1.
for(i in 1:k){
  
  trainSet <- which(folds != i)
  testSet <- which(folds == i)
  train.labels <- D[[1]][trainSet]
  
  for(query in testSet){
    
    type <- D[[1]][query]
    
    distances <- D[[2]][query, ][trainSet]
    
    # Store the index of the nearest neighbor.
    nn <- sort(distances, index.return = T)$ix[1]
    
    pred <- train.labels[nn]
    
    predictions <- c(predictions, pred)
    
    groundTruth <- c(groundTruth, type)
    
  }
} # end of for

# Generate confusion matrix.
cm <- confusionMatrix(factor(predictions), factor(groundTruth))

# Compute performance metrics per class.
cm$byClass[,c("Recall", "Specificity", "Precision", "F1")]

# Overall performance metrics
colMeans(cm$byClass[,c("Recall", "Specificity", "Precision", "F1")])

# Plot confusion matrix.
noClasses <- 10
letterSize <- 8
numSize <- 4
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

#png("gesturesCM.png", width = 6, height = 4, units = "in", res = 720)
print(plot1)
#dev.off()


# ******************
# *** Line plot ****
# ******************

# Path to one of the gesture files.
filepath <- file.path(datadir,"2","triangle_20130621-170906.txt")

df <- read.csv(filepath, header = F)

# Compute magnitude.
norm <- apply(df, 1, function(x) sqrt(x[1]^2 + x[2]^2 + x[3]^2))

n <- nrow(df)

tmp <- data.frame(type = "x", time = 1:n, values = df$V1)

tmp <- rbind(tmp, data.frame(type = "y", time = 1:n, values = df$V2))

tmp <- rbind(tmp, data.frame(type = "z", time = 1:n, values = df$V3))

tmp <- rbind(tmp, data.frame(type = "magnitude", time = 1:n, values = norm))


acc.plot <- ggplot(data=tmp,
                   aes(x = time, y = values,
                       colour = type, group = type,
                       linetype = type)) +
  ggtitle("") + xlab("Time") + ylab("Acceleration") +
  scale_linetype_manual(values=c("dotted", "dotted", "dotted", "solid"), name = "") +
  scale_color_manual(values=c("red","green","blue","black"), name = "") +
  geom_line(aes(color=type), size=0.6) +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5),
        legend.position="right",
        legend.key.width = unit(1.0,"cm"),
        legend.key.size = unit(0.5,"cm"))

#png("gesture_xyzmagnitude.png", width = 5, height = 2.5, units = "in", res = 720)
print(acc.plot)
#dev.off()
