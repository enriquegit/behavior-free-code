# An iterative implementation of MDS based on the python code in: Segaran, T. (2007). Programming collective intelligence: building smart web 2.0 applications. O'Reilly Media, Inc.

source(file.path("..","auxiliary_functions","globals.R"))

iterativeMDS <- function(D, maxit = 1000, lr = 0.005){
  # D is a dissimilarity matrix.
  # maxit: maximum number of iterations.
  # lr: learning rate. This controls how fast points are moved.
  
  d <- 2 # Target dimension, 2 in this case.
  
  n <- nrow(D)
  
  # Point coordinates. Initialized at random.
  # This is a matrix of size n x 2.
  coord <- matrix(runif(n*d), nrow = n, ncol = d)
  
  # Plot initial solution.
  plot(-coord[,1], coord[,2], col="white", main = "Iteration 0")
  text(-coord[,1], coord[,2], rownames(D))
  
  # Current distances.
  curdist <- NULL
  
  # Current error.
  curerror <- Inf
  
  for(it in 1:maxit){
    
    # Find current distances.
    curdist <- as.matrix(dist(coord))
    
    # Update coordinates based on the current distances
    # such that curdist gets closer to D.
    
    grad <- matrix(0.0, nrow = n, ncol = d)
    
    acumerror <- 0
    
    for(i in 1:n){
      for(j in 1:n){
        if(i == j)next;
        
        # Error of the current distance with respect to the actual one.
        error <- (curdist[i,j] - D[i,j]) / D[i,j]
        
        # Determine how much to move each point based on the error.
        grad[i,1] <- grad[i,1] + (((coord[i,1] - coord[j,1]) / curdist[i,j]) * error)
        grad[i,2] <- grad[i,2] + (((coord[i,2] - coord[j,2]) / curdist[i,j]) * error)
        
        acumerror <- acumerror + abs(error)
      }
    }
    
    # If moving the points increases the error instead of decreasing it
    # then stop.
    if(acumerror > curerror)break;
    curerror <- acumerror
    
    # Update coordinates
    coord[,1] <- coord[,1] - (lr * grad[,1])
    coord[,2] <- coord[,2] - (lr * grad[,2])
    
    # Plot current solution.
    if(it %% 30 == 0){
      plot(-coord[,1], coord[,2], col="white", main = paste0("Iteration ",it))
      text(-coord[,1], coord[,2], rownames(D))
      print(paste0("It:", it))
    }
    
  }
  
  print(paste0("iteratoins: ", it))
  
  return(coord)
  
}

# Load eurodist dataset and convert to matrix.
D <- eurodist; D <- as.matrix(D)

# Normalize distances from 0 to 1 to make the search more efficient.
maxv <- max(D)
minv <- min(D)
D <- (D - minv) / (maxv - minv)

# Set seed for reproducibility.
set.seed(1234)

# Try our function to reduce the dimension of the data.
res <- iterativeMDS(D, maxit = 1000, lr = 0.005)

# Plot result.
plot(-res[,1], res[,2], col="white", main = "Final Result")
text(-res[,1], res[,2], rownames(D))

