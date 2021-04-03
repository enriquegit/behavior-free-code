# Script to plot and extract features from the UTD-MHAD dataset.
# https://personal.utdallas.edu/~kehtar/UTD-MHAD.html

source(file.path("..","auxiliary_functions","globals.R"))
library(R.matlab)
library(rgl)

#############################
######## Plot actions #######
#############################

# Select one of the actions.
filepath <- file.path(datasets_path,"skeleton_actions","a7_s1_t1_skeleton.mat")

# Read skeleton file.
df <- readMat(filepath)$d.skel

dim(df)

# Select the first frame.
frame <- data.frame(df[, , 1])

# Print dimensions.
dim(frame)

# Code to plot taken from: https://stackoverflow.com/questions/8156925/r-package-for-motion-capture-data-analysis-and-visualisation

# Open a graphics device.
open3d()

# Set the view point.
rgl.viewpoint(userMatrix=rotationMatrix(0,0,0,0))
U <- par3d("userMatrix")
par3d(userMatrix = rotate3d(U, 0.95*pi, 0,1,0.1))

# Plot points.
plot3d(frame$X1, frame$X2, frame$X3, type="p", size = 5, col = "red")

# Define the joins.
joins <- c(1, 2, 2, 3, 3, 4, 2, 5, 2, 9, 5, 6, 6, 7, 7, 8, 9, 10, 10, 11, 11, 12, 4, 13, 13, 14, 14, 15, 15, 16, 4, 17, 17, 18, 18, 19, 19, 20)

segments3d(frame[joins, ], col = "blue")


#############################
###### Animate actions ######
#############################

filepath <- file.path(datasets_path,"skeleton_actions","a7_s1_t1_skeleton.mat")

df <- readMat(filepath)$d.skel

joins <- c(1, 2, 2, 3, 3, 4, 2, 5, 2, 9, 5, 6, 6, 7, 7, 8, 9, 10, 10, 11, 11, 12, 4, 13, 13, 14, 14, 15, 15, 16, 4, 17, 17, 18, 18, 19, 19, 20)

open3d()
rgl.viewpoint(userMatrix=rotationMatrix(0,0,0,0))
U <- par3d("userMatrix")
par3d(userMatrix = rotate3d(U, 0.95*pi, 0,1,0.1))

# This for loop will animate the rgl window.
for(i in 1:dim(df)[3]){
  frame <- data.frame(df[, , i])
  next3d(reuse=FALSE)
  #plot3d(frame$X1, frame$X2, frame$X3, type="p", size = 5,
  #       col = "red",
  #       xlab="x", ylab="y", zlab="z")
  
  #plot3d(frame$X1, frame$X2, frame$X3, type="p", size = 5,
  #       col = "red",
  #       xlab="", ylab="", zlab="", box = F, axes = F)
  
  plot3d(frame$X1, frame$X2, frame$X3, type="p", size = 5, col = "red")
  segments3d(frame[joins, ], col = "blue")
  
  Sys.sleep(0.05)
  
  # Save selected frames to png files.
  #selframes <- c(1,10,15,20,40,50)
  #if(i %in% selframes)rgl.snapshot(paste0("skl_",i,".png"))
  
  # Save all frames.
  #rgl.snapshot(paste0("gif/skl_",i,".png"))
}


#############################
##### Extract features ######
#############################

dirpath <- file.path(datasets_path,"skeleton_actions")

files <- list.files(dirpath, "*.mat")

dataset <- NULL

refPoint <- 3 #3 = spine.

joints <- c(1:2,4:20) # Joints excluding the ref point.

# This can take some minutes.
for(f in files){
  
  label <- strsplit(f,"_")[[1]][1]
  userid <- strsplit(f,"_")[[1]][2]
  df <- readMat(file.path(dirpath,f))$d.skel
  
  distances <- matrix(NA, nrow = 19, ncol = dim(df)[3], dimnames = list(x=as.character(joints),y=1:dim(df)[3]))
  
  # Iterate frames.
  for(i in 1:dim(df)[3]){
    # Iterate joints excluding refPoint.
    for(j in joints){
      va <- df[refPoint,,i]
      vb <- df[j,,i]
      d <- dist(rbind(va,vb), method = "manhattan")[1]
      distances[as.character(j),i] <- d
    }
  }
  
  means <- apply(distances, 1, mean); names(means) <- paste0("meanDist",names(means))
  maxs <- apply(distances, 1, max); names(maxs) <- paste0("maxDist",names(maxs))
  mins <- apply(distances, 1, min); names(mins) <- paste0("minDist",names(mins))
  
  tmp <- data.frame(userid=userid, label=label, matrix(means, ncol = length(means), dimnames = list(x=1,y=names(means))))
  
  tmp <- cbind(tmp, matrix(maxs, ncol = length(maxs), dimnames = list(x=1,y=names(maxs))))
  
  tmp <- cbind(tmp, matrix(mins, ncol = length(mins), dimnames = list(x=1,y=names(mins))))
  
  dataset <- rbind(dataset, tmp)

} # end iterate files.


# Write features file.
write.csv(dataset, file.path(dirpath,"dataset.csv"), row.names = F)
