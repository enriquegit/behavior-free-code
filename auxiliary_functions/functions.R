library(caret)
library(ggplot2)
library(gridExtra)


normalize <- function(trainset, testset){
  # Function to normalize a train and test set
  # based on the parameters learned from the trainset.
  
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


smote.class <- function(completeDf, targetClass, N, k){
  # Smotes the targetClass.
  # N smote percent. Multiple of 100.
  # k number of nieghbors.
  # Assumes that the first column is the class.
  
  if(N == 0) return(NULL)
  
  classIdxs <- which(completeDf$label == targetClass)
  
  # Data frame containing only the targetClass.
  targetClassDf <- completeDf[classIdxs,]
  
  # Number of minority class samples.
  n <- nrow(targetClassDf)
  
  # Indexes for targetClassDf.
  idxs <- 1:n
  
  if(N < 100){
    n <- (N/100) * n
    if(n < 1) n <- 1
    N <- 100
    idxs <- sample(idxs, size = n, replace = F)
  }
  
  N <- as.integer(N/100)
  
  # Compute k nearest neighbors #
  # NN stores the indexes to completeDf from the k nearest neighbors of each instance in targetClassDf.
  NN <- NULL
  
  distances <- as.matrix(dist(targetClassDf[,-1]))
  for(i in idxs){
    sorted <- sort(distances[i,], decreasing = F, index.return = T)
    
    # Indexes of nn from targetClassDf.
    nn <- sorted$ix[2:(k+1)]
    
    # Update indexes to point to completeDf.
    nn <- classIdxs[nn]
    
    NN <- rbind(NN, nn)
  }
  
  # Generate synthetic instances. #
  newInstances <- NULL
  
  # Indexes of original sample used to create the synthetic one (point to originalDf).
  originalSampleIdxs <- NULL
  
  # Indexes of nearest neighbor used to create the synthetic instance (point to originalDf).
  nnIdxs <- NULL
  
  for(i in 1:n){
    for(j in 1:N){
      # Select a random nn.
      originalSample <- completeDf[classIdxs[i],]
      originalSampleIdxs <- c(originalSampleIdxs, classIdxs[i])
      tmp <- NN[i, sample(1:k, 1)]
      nnSample <- completeDf[tmp,]
      nnIdxs <- c(nnIdxs, tmp)
      
      dif <- nnSample[,-1] - originalSample[,-1]
      gap <- runif(ncol(dif))
      synthetic <- originalSample
      synthetic[,2:ncol(originalSample)] <- originalSample[,-1] + (gap * dif)
      newInstances <- rbind(newInstances, synthetic)
    }
  }
  
  res <- list(synthetic=newInstances, sampleIdxs=originalSampleIdxs, nnIdxs = nnIdxs)
  return(res)
}

