source(file.path("..","auxiliary_functions","globals.R"))
source(file.path("..","auxiliary_functions","functions.R"))
library(solitude)
library(PRROC)


# Read dataset. This one contains the extracted features from the raw fish trajectories.
dataset <- read.csv(file.path(datasets_path,
                              "fish_trajectories","fishFeatures.csv"),
                    stringsAsFactors = T)

#### Split into train/test sets ####

# We will try without abnormal cases during training. We will do the same in the autoencoder example.
# All the abnormal cases go to the test set.
test.abnormal <- dataset[dataset$label == "abnormal",]

# Percent to use as train set.
pctTrain <- 0.80

# Corresponding number of samples.
nsamples <- ceiling(sum(dataset$label == "normal") * pctTrain)

# Generate the indices of the train set instances.
set.seed(123)

idxs <- sample(nsamples)

# Train set consisting of only normal instances.
train.normal <- dataset[dataset$label == "normal", ][idxs,]

# The test set of normal instances.
test.normal <- dataset[dataset$label == "normal", ][-idxs,]

# Build a test set with both, normal and abnormal points.
test.all <- rbind(test.normal, test.abnormal)

# No need for normalization since trees are not sensitive to different scales!

#### Train the isolation forest ####

# In the original paper (Liu, F. T., Ting, K. M., & Zhou, Z. H., 2008)
# sample size is set to 256 and no. of trees to 100.

# First we define the parameters. nproc is the numbers of cores in the CPU.
# I set it to 1 to get reproducible results.
m.iforest <- isolationForest$new(sample_size = 256,
                                 num_trees = 100,
                                 nproc = 1)

# Fit the model with the train data and without ids and labels.
m.iforest$fit(train.normal[,-c(1:2)])

# Predict anomaly scores on train set.
train.scores <- m.iforest$predict(train.normal[,-c(1:2)])

# Print first rows of predictions.
head(train.scores)

# Sort and display instances with the highest anomaly scores.
head(train.scores[order(anomaly_score, decreasing = TRUE)])

hist(train.scores$anomaly_score)

quantile(train.scores$anomaly_score)

threshold <- 0.7603

# Predict anomaly scores on test set.
test.scores <- m.iforest$predict(test.all[,-c(1:2)])

# Predict labels based on threshold.
predicted.labels <- as.integer((test.scores$anomaly_score > threshold))

# Count how many abnormal instances were detected (not necessarily true positives).
sum(predicted.labels)

#### Compute performance metrics ####

# All abnormal cases are at the end so we can compute the ground truth as follows.
gt.all <- c(rep(0,nrow(test.normal)), rep(1, nrow(test.abnormal)))

levels <- c("0","1")

cm <- confusionMatrix(factor(predicted.labels, levels = levels),
                      factor(gt.all, levels = levels),
                      positive = "1")

# Print confusion matrix.
cm$table

# Print sensitivity
cm$byClass["Sensitivity"]

#### Generate ROC curve with PRROC package ####

roc_obj <- roc.curve(scores.class0 = test.scores$anomaly_score,
                       weights.class0 = gt.all, curve = TRUE, rand.compute = T)

# Print first values of the curve table.
head(roc_obj$curve)

#png("roc_curve.png", width = 6, height = 4, units = "in", res = 720)
plot(roc_obj, rand.plot = T)
#dev.off()
