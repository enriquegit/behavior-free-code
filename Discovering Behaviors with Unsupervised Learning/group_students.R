source(file.path("..","auxiliary_functions","globals.R"))
source(file.path("..","auxiliary_functions","functions.R"))

##############################################################
########### K-means: Grouping student responses ##############
##############################################################

# Load students mental health behavior dataset.
dataset <- read.csv(file.path(datasets_path,
                              "students_mental_health","data.csv"),
                    stringsAsFactors = F)

# Replace empty strings with NAs so the following methods will work properly.
library(naniar)
dataset <- replace_with_na_all(dataset, ~.x %in% common_na_strings)

# Since the last rows starting at 269 are full of missing values we will discard them.
dataset <- dataset[1:268,]

# Discard internet column since it has many missing values.
dataset <- dataset[, -39]

# Remove rows with missing values in the variable Intimate.
dataset <- dataset[-which(is.na(dataset$Intimate)), ]

# Select which variables are going to be used for clustering.
selvars <- c("Stay","English_cate","Intimate","APD","AHome","APH","Afear","ACS","AGuilt","ToAS")

# Reduced dataset after dropping unused columns.
reddataset <- dataset[, selvars]

# One-hot encode our variables.
library(caret)

dummyObj <- dummyVars( ~ ., data = reddataset, fullRank = TRUE)

# Perform the encoding.
encodedVars <- data.frame(predict(dummyObj, newdata = reddataset))

# Normalize variables from 0 to 1.
# Since we are not dividing into train and test sets we pass the same dataset
# to the normalize() function defined in auxiliary_functions/functions.R
res <- normalize(encodedVars, encodedVars)

# Extract normalized data frame from result.
normdf <- res$train

# Use MDS to plot in two dimensions.
d <- dist(normdf)

fit <- cmdscale(d, k = 2)

x <- fit[,1]; y <- fit[,2]

# Plot MDS projection.
#png("cluster1.png", width = 4, height = 3, units = "in", res = 720)
plot(x, y, xlab="Coordinate 1", ylab="Coordinate 2", main="Students Responses", cex=0.7)
#dev.off()

# Run K-means.
set.seed(1234)

clusters <- kmeans(normdf, 4)

#png("cluster2.png", width = 4, height = 3, units = "in", res = 720)
plot(x, y, xlab="Coordinate 1", ylab="Coordinate 2", main="Students Responses", col=clusters$cluster, cex=0.7)
#dev.off()

# Add the cluster assignment as factor and store in new data frame.
grouping <- as.factor(paste0("cluster_",clusters$cluster))
gdf <- cbind(grouping, normdf)

#png("bp_intimate.png", width = 5, height = 3, units = "in", res = 720)
#par(mar=c(5,5,1,2))
boxplot(IntimateYes ~ grouping, data=gdf)
#dev.off()

#png("bp_ACS.png", width = 5, height = 3, units = "in", res = 720)
#par(mar=c(5,5,1,2))
boxplot(ACS ~ grouping, data=gdf)
#dev.off()


###################################################
########### Silhouette Index Example ##############
###################################################

# We will use the same normalized students responses data frame (normdf) from previous section.

library(cluster)

# Try with k=4
set.seed(1234)

clusters <- kmeans(normdf, 4)

# Compute silhouette index.
si <- silhouette(clusters$cluster, dist(normdf))

# Print first rows.
head(si)

# Compute total Silhouette index by averaging the individual indices.
mean(si[,3])

#png("si_4.png", width = 5, height = 4, units = "in", res = 720)
plot(si, cex.names=0.6, col = 1:4,
     main = "Silhouette plot, k=4",
     border=NA)
#dev.off()

# Try with k=7
set.seed(1234)

clusters <- kmeans(normdf, 7)

si <- silhouette(clusters$cluster, dist(normdf))

#png("si_7.png", width = 5, height = 4, units = "in", res = 720)
plot(si, cex.names=0.6, col = 1:7,
     main = "Silhouette plot, k=7",
     border=NA)
#dev.off()
