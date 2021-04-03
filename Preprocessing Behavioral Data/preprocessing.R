source(file.path("..","auxiliary_functions","globals.R"))

##################################
######## Missing values ##########
##################################

# Load naniar package
library(naniar)

# The sheep and goats dataset used here can be downloaded from https://easy.dans.knaw.nl/ui/datasets/id/easy-dataset:76131

# Path to S1.csv file.
datapath <- file.path(datasets_path,
                      "sheep_goats","S1.csv")

df <- read.csv(datapath, stringsAsFactors = TRUE) # Can take some seconds to load.

# Print a summary.
summary(df)

# Plot missing values.
p1 <- gg_miss_var(df)
print(p1)

#png("ggmissvar.png", width = 5, height = 2.5, units = "in", res = 720)
#print(p1)
#dev.off()

# Plot observations with missing values.
p2 <- vis_miss(df[1:1000,], warn_large_data = F)
print(p2)

#png("vismiss.png", width = 8, height = 6, units = "in", res = 720)
#print(p2)
#dev.off()

#### Impute missing values ###
library(simputation)

# Replace NaN with NAs.
# Since missing values are represented as NaN,
# first we need to replace them with NAs.

# Code to replace NaN with NA was taken from Hong Ooi:
# https://stackoverflow.com/questions/18142117/how-to-replace-nan-value-with-zero-in-a-huge-data-frame/18143097
is.nan.data.frame <- function(x)do.call(cbind, lapply(x, is.nan))

df[is.nan(df)] <- NA

# Use simputation package to impute values.
# The first 4 columns are removed since we do not want to use them as predictor variables.
imp_df <- impute_lm(df[,-c(1:4)], cx + cy + cz + pressure ~ . - cx - cy - cz - pressure)

# Print summary.
summary(imp_df)

##################################
########## Smoothing #############
##################################

movingAvg <- function(x, w = 5){
  # Applies moving average to x with a window of size w.
  
  n <- length(x) # Total number of points.
  
  smoothedX <- rep(NA, n)
  
  for(i in 1:(n-w+1)){
    smoothedX[i] <- mean(x[i:(i-1+w)])
  }
  
  return(smoothedX)
}

movingAvgCent <- function(x, w = 5){
  # Applies centered moving average to x with a window of size w.
  
  if(w %% 2 == 0){
    stop("w must be an odd number.")
  }
  
  n <- length(x) # Total number of points.
  
  smoothedX <- rep(NA, n)
  
  sidePoints <- (w-1) / 2 # Number of points on each side.
  
  for(i in (sidePoints+1):(n-sidePoints)){
    smoothedX[i-sidePoints-1] <- mean(x[(i-sidePoints):(i+sidePoints)])
  }
  
  return(smoothedX)
}

# Path to S1.csv file.
datapath <- file.path(datasets_path,
                      "sheep_goats","S1.csv")

df <- read.csv(datapath, stringsAsFactors = TRUE)

# Only select a subset of the whole time series.
dfsegment <- df[df$timestamp_ms < 6000,]

x <- dfsegment$ax
plot(x, type = "l")

smoothed <- movingAvg(x, w = 21)
plot(smoothed, type = "l")

smoothedC <- movingAvgCent(x, w = 21)
plot(smoothedC, type = "l")

smoothed2 <- movingAvg(x, w = 51)
plot(smoothed2, type = "l")

smoothed2C <- movingAvgCent(x, w = 51)
plot(smoothed2C, type = "l")

# Plot original timeseries v.s. smoothed one.
library(ggplot2)

type <- c(rep("original", length(x)), rep("movingAvg", length(x)))

type <- as.factor(type)

values <- c(x, smoothed)

t <- rep(1:length(x), 2)

df <- data.frame(timestep = t, type = type, values = values)

tsPlot <- ggplot(data = df,
                 aes(x = timestep,
                     y = values,
                     colour = type,
                     linetype = type)) +
  ggtitle("Original v.s. Smoothed timeseries.") +
  xlab("Timestep") +
  ylab("Acceleration in X") +
  geom_line(aes(color=type)) +
  scale_color_manual(values=c("blue","black"), name = "") +
  scale_linetype_manual(values=c("solid", "solid"), name = "") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5),
        legend.position="right",
        legend.key.width = unit(1.0,"cm"),
        legend.key.size = unit(0.5,"cm"))

#png("smoothingExample.png", width = 5, height = 2.5, units = "in", res = 720)
print(tsPlot)
#dev.off()

##################################
########## Normalize #############
##################################

# Load home activities dataset.
dataset <- read.csv(file.path(datasets_path,
                              "home_tasks",
                              "sound_acc.csv"),
                    stringsAsFactors = T)

# Check first 4 variables' min and max values.
summary(dataset[,1:4])

# Divide into 50/50% train and test set.
set.seed(1234)

folds <- sample(2, nrow(dataset), replace = T)

trainset <- dataset[folds == 1,]

testset <- dataset[folds == 2,]

# Define a function to normalize the train and test set
# based on the parameters learned from the train set.
normalize <- function(trainset, testset){
  
  # Iterate columns
  for(i in 1:ncol(trainset)){
    
    c <- trainset[,i] # trainset column
    c2 <- testset[,i] # testset column
    
    # Skip if the variable is not numeric or integer.
    if(class(c) != "numeric" && class(c) != "integer")next;
    
    max <- max(c, na.rm = T) # Learn the max value from the trainset's column.
    min <- min(c, na.rm = T) # Learn the min value from the trainset's column.
    
    if(max==min){ # If all values are the same set it to max.
      trainset[,i] <- max
      testset[,i] <- max
    }
    else{
      
      # Normalize trainset's column.
      trainset[,i] <- (c - min) / (max - min)
      
      # Truncate max values in testset.
      idxs <- which(c2 > max)
      if(length(idxs) > 0){
        c2[idxs] <- max
      }
      
      # Truncate min values in testset.
      idxs <- which(c2 < min)
      if(length(idxs) > 0){
        c2[idxs] <- min
      }
      
      # Normalize testset's column.
      testset[,i] <- (c2 - min) / (max - min)
    }
  }
  
  return(list(train=trainset, test=testset))
}

# Call our function to normalize each set.
normalizedData <- normalize(trainset, testset)

# Inspect the normalized train set.
summary(normalizedData$train)

# Inspect the normalized test set.
summary(normalizedData$test)


##################################
######### Oversampling ###########
##################################

set.seed(1234)

# Create random data
n1 <- 200 # Number of points of majority class.
n2 <- 15 # Number of points of minority class.

# Generate random values for class1.
x <- rnorm(mean = 0, sd = 0.5, n = n1)
y <- rnorm(mean = 0, sd = 1, n = n1)
df1 <- data.frame(label=rep("class1", n1),
                  x=x, y=y, stringsAsFactors = T)

# Generate random values for class2.
x2 <- rnorm(mean = 1.5, sd = 0.5, n = n2)
y2 <- rnorm(mean = 1.5, sd = 1, n = n2)
df2 <- data.frame(label=rep("class2", n2),
                  x=x2, y=y2, stringsAsFactors = T)

# This is our imbalanced dataset.
imbalancedDf <- rbind(df1, df2)

# Print class counts.
summary(imbalancedDf$label)

# Generate new points from the minority class.
new.points <- df2[sample(nrow(df2), size = 185, replace = T),]

# Add new points to the imbalanced dataset and save the
# result in balancedDf.
balancedDf <- rbind(imbalancedDf, new.points)

# Print class counts.
summary(balancedDf$label)

##################################
############ SMOTE ###############
##################################

# Load smote.class() function.
source(file.path("..","auxiliary_functions","functions.R"))

# Print class counts.
summary(imbalancedDf$label)

# To balance the dataset, we need to oversample 1200%.
# This means that the method will create 12 * 15 new points.
ceiling(180 / n2) * 100

N <- 1200 # 1200% which means the method will create 12 * 15 new points.

# Generate new data points.
synthetic.points <- smote.class(imbalancedDf,
                                targetClass = "class2",
                                N = N,
                                k = 5)$synthetic

# Append the new points to the original dataset.
smote.balancedDf <- rbind(imbalancedDf, synthetic.points)

# Print class counts.
summary(smote.balancedDf$label)

##################################
####### One-hot encoding #########
##################################

# Load students mental health behavior dataset.
# stringsAsFactors is set to F since the function
# that we will use to one-hot encode expects characters.
dataset <- read.csv(file.path(datasets_path,
                              "students_mental_health",
                              "data.csv"),
                              stringsAsFactors = F)

# The dataset contains several empty strings.
# Replace those empty strings with NAs so the following
# methods will work properly.
# We can use the replace_with_na_all() function
# from naniar package to do the replacement.
library(naniar)
dataset <- replace_with_na_all(dataset,
                               ~.x %in% common_na_strings)

# Visualize missing values.
p <- vis_miss(dataset, warn_large_data = F)
print(p)

#png("mentalmissing.png", width = 8, height = 6, units = "in", res = 720)
#print(p)
#dev.off()

# Since the last rows starting at 269 are full of missing values we will discard them.
dataset <- dataset[1:268,]

# One-hot encode the Stay_Cate variable.
# This variable Stay_Cate has three possible values: Long, Short and Medium.
# First, create a dummyVars object with the dummyVars() function from caret package.
library(caret)

dummyObj <- dummyVars( ~ Stay_Cate, data = dataset)

# Perform the actual encoding using predict()
encodedVars <- data.frame(predict(dummyObj, newdata = dataset))

# Print first rows.
head(encodedVars)

# Notice that the resulting data frame contains three variables
# one for each possible value. To avoid the dummy variable trap
# we should discard one of the columns. The parameter fullRank of dummyVars
# can be set to TRUE in which case one of the columns will be dropped.

dummyObj <- dummyVars( ~ Stay_Cate, data = dataset, fullRank = TRUE)

# Perform the actual encoding using predict()
encodedVars <- data.frame(predict(dummyObj, newdata = dataset))


# One-hot encode everything.
dummyObj <- dummyVars( ~ ., data = dataset, fullRank = TRUE)

# Perform the actual encoding using predict()
encodedVars <- data.frame(predict(dummyObj, newdata = dataset))

