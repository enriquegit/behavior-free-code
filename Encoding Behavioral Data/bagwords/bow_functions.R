source(file.path("..","..","auxiliary_functions","globals.R"))

constants <- list()

constants$datapath <- file.path(datasets_path,"complex_activities")

constants$winsize <- 150 # window size.

constants$wordsize <- 15 # number of words.

constants$labels <- c("commuting", "work", "home", "shopping", "exercising")

# Function to extract simple activities from raw accelerometer data.
extractSimpleActivities <- function(train = TRUE){
  
  folder <- "train"
  if(train == FALSE)folder <- "test"
  
  # List files in directory.
  files <- list.files(file.path(constants$datapath,folder), pattern = "*.txt")
  
  for(f in files){
    
    # Read file contents.
    df <- read.csv(file.path(constants$datapath,folder,f), header = F, col.names = c("timestamp","x","y","z"))
    
    n <- nrow(df) # number of rows
    
    starts <- seq(1,n,constants$winsize) # start index of each window
    
    starts <- starts[-length(starts)] # omit last window in case is shorter than winsize
    
    dataset <- NULL
    
    for(i in starts){
      block <- df[i:(i + constants$winsize - 1), ]
      
      meanX <- mean(block$x); meanY <- mean(block$y); meanZ <- mean(block$z)
      
      sdX <- sd(block$x); sdY <- sd(block$y); sdZ <- sd(block$z)
      
      maxX <- max(block$x); maxY <- max(block$y); maxZ <- max(block$z)
      
      corXY <- cor(block$x,block$y); corXZ <- cor(block$x,block$z); corYZ <- cor(block$y,block$z)
      
      # Compute the magnitude
      magnitude <- sqrt(block$x^2 + block$y^2 + block$z^2)
      
      meanMagnitude <- mean(magnitude)
      
      sdMagnitude <- sd(magnitude)
      
      tmp <- data.frame(meanX=meanX, meanY=meanY, meanZ=meanZ, sdX=sdX, sdY=sdY, sdZ=sdZ, maxX=maxX, maxY=maxY, maxZ=maxZ, corXY=corXY, corXZ=corXZ, corYZ=corYZ, meanMagnitude=meanMagnitude, sdMagnitude=sdMagnitude)
      
      dataset <- rbind(dataset, tmp)
    }
    
    # Save simple activities
    write.csv(dataset, file.path(constants$datapath,
                                 folder,
                                 "simple_activities",
                                 f),
              row.names = F,
              quote = F)
  }
  
}

# Function to cluster simple activities from the train set and save centroids.
clusterSimpleActivities <- function(){
  
  # List files in directory.
  files <- list.files(file.path(constants$datapath,
                                "train",
                                "simple_activities"),
                      pattern = "*.txt")
  
  # First append all files.
  dataset <- NULL
  for(f in files){
    df <- read.csv(file.path(constants$datapath,
                             "train",
                             "simple_activities",
                             f))
    dataset <- rbind(dataset, df)
  }
  
  # Cluster simple activities.
  ans <- kmeans(dataset, constants$wordsize, iter.max=1000)
  
  # Extract centroids.
  centroids <- ans[["centers"]]
  
  # Save centroids to text file.
  sink(file.path(constants$datapath,
                 "train",
                 "clustering",
                 "centroids.txt"),
       append=F)
  cat(constants$wordsize)
  cat("\n")
  cat(ncol(centroids))
  cat("\n")
  for(i in 1:nrow(centroids)){
    str <- paste(as.character(centroids[i,]), collapse=",")
    str <- paste(str, "\n", sep="")
    cat(str)
  }
  sink()
  print("clustering done")
  
}

# Assign simple activities to clusters.
assignSimpleActivitiesToCluster <- function(){
  
  centroids <- read.csv(file.path(constants$datapath,
                                  "train","clustering",
                                  "centroids.txt"),
                        skip = 2, header = F)
  
  nwords <- nrow(centroids)
  
  nfeatures <- ncol(centroids)
  
  fnames <- paste0("V",1:nfeatures)
  
  # List files in directory.
  files <- list.files(file.path(constants$datapath,
                                "test",
                                "simple_activities"),
                      pattern = "*.txt")
  
  total <- length(files)
  counter <- 0
  
  for(f in files){
    
    counter <- counter + 1
    
    # Read file contents.
    df <- read.csv(file.path(constants$datapath,
                             "test",
                             "simple_activities",
                             f),
                   col.names = fnames,
                   header = T)
    
    print(paste0("processing file ",counter,"/",total," ",f))
    
    centroidids <- NULL
    
    for(i in 1:nrow(df)){
      activity <- df[i,]
      mind <- Inf
      mincentroid <- -1
      for(j in 1:nrow(centroids)){
        centroid <- centroids[j,]
        d <- dist(rbind(centroid, activity))[1]
        if(d < mind){
          mind <- d
          mincentroid <- j
        }
      }
      centroidids <- c(centroidids,mincentroid)
    }
    # Save centroid ids
    write.table(centroidids, file.path(constants$datapath,
                                       "test",
                                       "labeled_activities",
                                       f),
                row.names = F,
                col.names = F)
    
  }

}

# Convert to histograms.
convertToHistogram <- function(){
  
  # List files in directory.
  files <- list.files(file.path(constants$datapath,
                                "test",
                                "labeled_activities"),
                      pattern = "*.txt")
  
  H <- NULL
  
  label <- NULL
  
  for(f in files){
    df <- read.csv(file.path(constants$datapath,
                             "test",
                             "labeled_activities",
                             f),
                   header = F)
    
    t <- table(df$V1)
    histogram <- rep(0, constants$wordsize)
    names(histogram) <- as.character(1:constants$wordsize)
    histogram[names(t)] <- as.integer(t)
    
    # Normalize.
    histogram <- histogram / sum(histogram)
    
    H <- rbind(H, histogram)
    
    l <- constants$labels[as.integer(strsplit(f, "_")[[1]][1])]
    
    label <- c(label, l)
  }
  
  df <- as.data.frame(H)
  
  df <- cbind(label, df)
  
  colnames(df)[2:ncol(df)] <- paste0("w",1:constants$wordsize) 
  
  write.csv(df, file.path(constants$datapath,
                          "test",
                          "histograms",
                          "histograms.csv"),
            row.names = F,
            quote = F)
  
}
