source(file.path("..","auxiliary_functions","globals.R"))
library(igraph)
library(hash)

# Function that reads a file of ids and converts it to an igraph.
ids.to.graph <- function(ids, relative.weights=TRUE){
  
  h <- hash()
  
  n <- length(ids)
  
  for( i in 1:(n-1) ){
    v1 <- ids[i]
    v2 <- ids[i+1]
    tmp <- c(v1,v2)
    key <- paste(tmp[1], tmp[2], sep=",")
    if( has.key(key, h)[[1]] ){
      h[key] <- h[[key]] + 1
    }
    else{
      h[key] <- 1
    }
  }
  
  keys <- keys(h)
  size <- length(keys)
  vertices1 <- character(length=size)
  vertices2 <- character(length=size)
  weight <- numeric(size)
  index <- 1
  
  for( key in keys ){
    parts <- strsplit(key, ",")[[1]]
    v1 <- parts[1]
    v2 <- parts[2]
    w <- h[[key]]
    vertices1[index] <- v1
    vertices2[index] <- v2
    weight[index] <- w
    index <- index + 1
  }
  
  if(relative.weights){
    weight <- weight/sum(weight)
  }
  
  data <- data.frame(vertices1, vertices2, weight)
  
  g <- graph.data.frame(data, directed = T)
  
  clear(h)
  return (g)
}


#### Plot  a complex activity as a graph ####

# Path to labeled_activities/
datapath <- file.path(datasets_path,
                      "complex_activities",
                      "test",
                      "labeled_activities")

filename <- "2_20120606-111732.txt" # Select the activity file.

df <- read.csv(file.path(datapath, filename), header = F)

g <- ids.to.graph(df$V1, relative.weights = T)


#png("graph_activity.png", width = 5, height = 4, units = "in", res = 250)
set.seed(12345)
plot(g, vertex.label.cex = 0.7,
     edge.arrow.size = 0.2,
     edge.arrow.width = 1,
     edge.curved = 0.1,
     edge.width = E(g)$weight * 8,
     edge.label = round(E(g)$weight, digits = 3),
     edge.label.cex = 0.4,
     edge.color = "orange",
     edge.label.color = "black",
     vertex.color = "skyblue"
     )
#dev.off()

# Print adjacency matrix.
as_adjacency_matrix(g)

# Print adjacency matrix including weights.
as_adjacency_matrix(g, attr = "weight")
