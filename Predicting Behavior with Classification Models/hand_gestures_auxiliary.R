# Auxiliary function to preprocess the hand gestures and store them as a list of lists.



gen.instances <- function(path){
  # Returns a list of lists with each inner list containing a single hand gesture and its class.
  dirs <- list.dirs(path, full.names = F, recursive = F)
  
  instances <- list()
  
  for(d in dirs){
    
    files <- list.files(file.path(path,d))
    
    for(file in files){
      type <- strsplit(file, split = '_')[[1]][1]
      x <- read.csv(file.path(path,d,file), header = F)
      norm <- apply(x, 1, function(x) sqrt(x[1]^2+x[2]^2+x[3]^2))
      instances <- c(instances, list(list(type=type, values=norm)))
    }
  }
  return(instances)
}

matrix.distances <- function(x, stp=symmetric1, normalized = T){
  n <- length(x)
  D <- matrix(nrow = n, ncol = n)
  types <- numeric()
  for(i in 1:n){
    types <- c(types, x[[i]][[1]])
    query <- x[[i]][[2]]
    for(j in 1:n){
      if(i < j){
        ref <- x[[j]][[2]]
        ans <- dtw(query, ref, keep = F, distance.only = T)
        if(normalized){
          D[i,j] <- ans$normalizedDistance
        }
        else{
          D[i,j] <- ans$distance
        }
      }
      else if(i == j){
        D[i,j] <- Inf
      }
      else{
        D[i,j] <- D[j,i]
      }
    }
  }
  return(list(types, D))
}
