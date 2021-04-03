
# Create some random data.
set.seed(123)

n <- 50

x <- rnorm(n)

png("anomaly_example.png", width = 7, height = 3, units = "in", res = 200)

par(mar=c(2,0.5,0.5,0.5))

plot(x,rep(1,n), yaxt = 'n', ylab = "", col="gray", pch=1, cex=1.0, ylim = c(1,1.1))

# Make the max be the anomaly.
xanomaly <- max(x)

points(xanomaly,1, col="red", pch = 17, cex=1.1)

# Select another point to be the normal one.
xnormal <- x[10]

points(xnormal,1, col="blue", pch = 19, cex=1.1)

# Add a legend.
legend("topleft", c("anomaly","normal"),
       pch = c(17,19),
       col=c("red","blue"))

dev.off()

# Recursive function to isolate a selected point x.
isolate.point <- function(x, point, res){
  
  # The point of interest is now isolated.
  if(length(x)==1 & x[1] == point){
    return(list(h=0, splitpoint=res$splitpoint))
  }
  
  # The point of interest was not found in this path.
  if(length(x)==1)return(NULL)
  
  # Select a random split point.
  sp <- runif(1, min=min(x), max=max(x))
  
  #abline(v=sp)
  
  lx <- x[which(x <= sp)]
  
  rx <- x[which(x > sp)]
  
  lres <- isolate.point(lx, point, list(h=res$h, splitpoint=sp))
  
  rres <- isolate.point(rx, point, list(h=res$h, splitpoint=sp))

  if(is.null(lres) && is.null(rres))return(NULL)
    
  if(is.null(lres)){
    return(list(h=1+rres$h, splitpoint=c(res$splitpoint, rres$splitpoint)))
  }
  else {
    return(list(h=1+lres$h, splitpoint=c(res$splitpoint, lres$splitpoint)))
  }
  
  
}


res1 <- isolate.point(x, xanomaly, list(h=0, splitpoint=NULL))

abline(v=res1$splitpoint, col="red")

res2 <- isolate.point(x, xnormal, list(h=0, splitpoint=NULL))

abline(v=res2$splitpoint, col="blue")

dev.off()


#### Construct several trees ####
isolate.point.iter <- function(x, point, niter=10){
  
  avg.h <- NULL
  
  for(i in 1:niter){
    
    tmp.h <- 0
    
    for(j in 1:i){
      res <- isolate.point(x, point, list(h=0, splitpoint=NULL))
      tmp.h <- tmp.h + res$h
    }
    
    avg.h <- c(avg.h, tmp.h / i)
  }
  
  return(avg.h)
}

set.seed(123)

heights.anomaly <- isolate.point.iter(x, xanomaly, niter=200)

heights.normal <- isolate.point.iter(x, xnormal, niter=200)

png("anomaly_its.png", width = 7, height = 5, units = "in", res = 200)

plot(heights.anomaly, type="l", ylim = c(2.0,10), col="red",
     main = "Average path lengths",
     ylab = "average path length",
     xlab = "# trees")

lines(heights.normal, col="blue", lty = 2)

legend("right", c("normal","anomaly"),
       lty = c(2,1),
       col=c("blue","red"))

dev.off()


tail(heights.normal)

tail(heights.anomaly)


##########################
#### Sampling example ####
##########################

# Create some random data.
set.seed(1234)

n <- 4000; # Normal points.
n.anomalies <- 100 # Anomaly points.

x <- rnorm(n)
y <- rnorm(n)

ax1 <- rnorm(n.anomalies, mean = -2.7, sd = 0.1)
ay1 <- rnorm(n.anomalies, mean = 1.8, sd = 0.3)

df <- data.frame(x=c(x,ax1), y=c(y,ay1), type=c(rep("normal",n), rep("anomaly",n.anomalies)))

#png("sampling_instances.png", width = 9, height = 6, units = "in", res = 200)
#par(mfrow=c(1,2)) 

plot(df[df$type=="normal",1],df[df$type=="normal",2], col="gray", pch=1, cex=1.0, main = "Original dataset (4100 instances)", xlab = "x", ylab = "y", xlim = c(-3.5,3.5), ylim = c(-3.5,3.5))

points(df[df$type=="anomaly",1], df[df$type=="anomaly",2], col="red", pch=2)

legend("topright", c("anomaly","normal"),
       pch = c(17,1),
       col=c("red","gray"))


# Sample.

p <- 256 # Sampling size.

reduced.df <- df[sample(nrow(df), size = p, replace = F),]

plot(reduced.df[reduced.df$type=="normal",1],reduced.df[reduced.df$type=="normal",2], col="gray", pch=1, cex=1.0, main = "After sampling (256 instances)", xlab = "x", ylab = "y", xlim = c(-3.5,3.5), ylim = c(-3.5,3.5))

points(reduced.df[reduced.df$type=="anomaly",1], reduced.df[reduced.df$type=="anomaly",2], col="red", pch=2)

legend("topright", c("anomaly","normal"),
       pch = c(17,1),
       col=c("red","gray"))

#dev.off()
