# This script demonstrates k-means implementation.

source(file.path("..","auxiliary_functions","globals.R"))
source(file.path("..","auxiliary_functions","functions.R"))

# Prepare students mental health dataset.
# Load students mental health behavior dataset.
dataset <- read.csv(file.path(datasets_path,"students_mental_health",
                              "data.csv"),
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
selvars <- c("ToAS","ToSC")

# Reduced dataset after dropping unused columns.
reddataset <- dataset[, selvars]

# Normalize variables from 0 to 1.
# Since we are not dividing into train and test sets we pass the same dataset
# to the normalize() function defined in auxiliary_functions/functions.R
res <- normalize(reddataset, reddataset)

# Extract normalized data frame from result.
data <- res$train

# Compute centroid of all data points
centroid <- colMeans(data[,c(1,2)])

# Plot points
plot(data$ToAS, data$ToSC)

# Plot centroid
points(centroid[1], centroid[2], col="red", pch=18, cex = 2)

############################
#### Implement k-means. ####
############################

dataset <- as.data.frame(data)

k <- 3 # number of clusters.

maxiter <- 10 # maximum number of iterations.

plotit <- 4 # plot first plotit iterations.

# Generate random centroids by picking k random points from dataset.
set.seed(124)

centroids <- dataset[sample(nrow(dataset), size = k, replace = F),]

# Plot dimensions
pwidth <- 5; pheight <- 4

#png("clust_it0.png", width = pwidth, height = pheight, units = "in", res = 200)
plot(data$ToAS, data$ToSC, xlab = "ToAS", ylab = "ToSC", main = "Initial random centroids")
points(centroids$ToAS, centroids$ToSC, col=2:4, pch=18, cex = 1.5)
#dev.off()

# Keep track to which group each point is assigned to.
assignments <- rep(-1, nrow(dataset))

#png(paste0("clust_its.png"), width = pwidth*2, height = pheight*2, units = "in", res = 200)
par(mfrow=c(2,2))

for(iter in 1:maxiter){
  
  print(paste0("Iteration ",iter))
  
  # compute distance to closest centroid
  for(i in 1:nrow(dataset)){
    
    p <- dataset[i,]
    mind <- Inf # min distance
    minc <- -1 # min cluster
    
    for(j in 1:k){
      d <- dist(rbind(centroids[j,],p))
      if(d < mind){
        mind <- d
        minc <- j
      }
    }
    assignments[i] <- minc
  }
  
  if(iter <= plotit){
    #png(paste0("clust_it",iter,".png"), width = pwidth, height = pheight)
    plot(dataset$ToAS, dataset$ToSC, xlab = "ToAS", ylab = "ToSC", col=assignments+1, cex=1, main=paste0("Iteration ",iter))
    points(centroids$ToAS, centroids$ToSC, col=2:4, pch=18, cex = 2.5)
    #dev.off()
  }
  
  # update centroids.
  for(i in 1:k){
    idxs <- which(assignments == i)
    centroids[i,] <- colMeans(dataset[idxs,])
  }
  
} # end of maxiter
#dev.off()
