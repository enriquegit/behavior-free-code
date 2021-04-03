source(file.path("..","auxiliary_functions","globals.R"))
library(pheatmap)


# Euclidean distance
norm2 <- function(x, y){
  return(sqrt((x - y)^2))
}

# Function that computes a distance matrix and a RP
# and returns both.
rp <- function(x, e, f=norm2){
  #x: vector.
  #e: threshold.
  #f: norm (distance function).
  N <- length(x)
  
  # This variable will store the recurrence plot.
  M <- matrix(nrow=N, ncol=N)
  
  # This variable will store the distance matrix.
  D <- matrix(nrow=N, ncol=N)
  
  for(i in 1:N){
    for(j in 1:N){
      
      # Compute distance between a pair of points.
      d <- f(x[i], x[j])
      
      # Store result in D.
      # Start filling values from bottom left.
      D[N - (i-1), j] <- d 
      
      if(d <= e){
        M[N - (i-1), j] <- 1
      }
      else{
        M[N - (i-1), j] <- 0
      }
    }
  }
  return(list(D=D, RP=M))
}

# Read a timeseries from the HAND GESTURES dataset.
df <- read.csv(file.path(datasets_path,"hand_gestures","1","1_20130703-120056.txt"),
               header = F)

# Extract the x axis values.
x <- df$V1

# Plot timeseries.
#png("rp_series.png", width = 5, height = 4, units = "in", res = 200)
plot(x, type="l", main="Hand gesture 1", xlab = "time", ylab = "Acceleration in x")
#dev.off()

# Compute RP and distance matrix.
res <- rp(x, 0.5, norm2)

# Plot RP.
#png("rp_rp_5.png", width = 5, height = 4, units = "in", res = 200)
pheatmap(res$RP, main="RP with threshold = 0.5", cluster_row = FALSE,
         cluster_col = FALSE,
         show_rownames = T,
         show_colnames = T,
         legend = F,
         color = colorRampPalette(c("white", "black"))(50))
#dev.off()


# Plot distance matrix.
#png("rp_d.png", width = 5, height = 4, units = "in", res = 200)
pheatmap(res$D, main="Distance matrix of gesture 1", cluster_row = FALSE,
         cluster_col = FALSE,
         show_rownames = T,
         show_colnames = T,
         legend = F,
         color = colorRampPalette(c("white", "black"))(50))
#dev.off()


## Try different thresholds ##
res <- rp(x, 0.01, norm2)

pheatmap(res$RP, main="RP with threshold = 0.01", cluster_row = FALSE,
         cluster_col = FALSE,
         legend = F,
         color = colorRampPalette(c("white", "black"))(50))

res <- rp(x, 1.5, norm2)

pheatmap(res$RP, main="RP with threshold = 1.5", cluster_row = FALSE,
         cluster_col = FALSE,
         legend = F,
         color = colorRampPalette(c("white", "black"))(50))

