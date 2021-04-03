source(file.path("..","auxiliary_functions","globals.R"))
source(file.path("..","auxiliary_functions","functions.R"))
library(R.matlab)
library(trajr)


# Read the database file.
df <- readMat(file.path(datasets_path,
                        "fish_trajectories","fishDetections_total3102.mat"))$fish.detections

#### Extract features ####

n <- dim(df)[3] # Number of trajectories.

minFrames <- 10 # Only trajectories with a minimum number of frames will be considered.

total <- NULL # data frame to save features.

for(i in 1:n){
  
  if(i %% 100)print(paste0("Processing trajectory ", i, "/", n))
  
  trj <- df[,,i]
  
  if(length(trj$frame.number) < minFrames)next;
  
  # Compute center of bounding box.
  x.coord <- trj$bounding.box.x + (trj$bounding.box.w / 2)
  y.coord <- trj$bounding.box.y + (trj$bounding.box.h / 2)
  times <- trj$frame.number - trj$frame.number[1] # Make times start at 0.
  
  tmp <- data.frame(x.coord, y.coord, time=times)
  
  tmp.trj <- TrajFromCoords(tmp, fps = 1)
  
  #png("traj_resampled_plot.png", width = 6, height = 4, units = "in", res = 200)
  #par(mar=c(5,5,2,2))
  #plot(tmp.trj, lwd = 1, xlab="x", ylab="y")
  #points(tmp.trj, draw.start.pt = T, pch = 1, col = "blue", cex = 1.2)
  
  resampled <- TrajResampleTime(tmp.trj, 1)
  #points(resampled, pch = 4, col = "red", cex = 0.8)
  
  #legend("topright", c("Starting point"), pch = c(16), col=c("black"))
  #legend("topright", c("Starting point","Original trajectory","Resampled trajectory"), pch = c(16,1,4), col=c("black","blue","red"))
  
  #dev.off()
  
  derivs <- TrajDerivatives(resampled)
  
  #head(derivs$speed)
  
  head(derivs$acceleration)
  
  
  f.meanSpeed <- mean(derivs$speed)
  f.sdSpeed <- sd(derivs$speed)
  f.minSpeed <- min(derivs$speed)
  f.maxSpeed <- max(derivs$speed)
  
  f.meanAcc <- mean(derivs$acceleration)
  f.sdAcc <- sd(derivs$acceleration)
  f.minAcc <- min(derivs$acceleration)
  f.maxAcc <- max(derivs$acceleration)
  
  features <- data.frame(id=paste0("id",i), label=trj$classDescription[1],
                         f.meanSpeed, f.sdSpeed, f.minSpeed, f.maxSpeed,
                         f.meanAcc, f.sdAcc, f.minAcc, f.maxAcc)
  
  total <- rbind(total, features)
}

# Save dataset.
write.csv(total, file.path(datasets_path,
                           "fish_trajectories","fishFeatures.csv"), quote = F, row.names = F)


#### Make some plots. ####

# Read dataset.
dataset <- read.csv(file.path(datasets_path,
                              "fish_trajectories","fishFeatures.csv"),
                              stringsAsFactors = T)

# Print first rows of the dataset.
head(dataset)

table(dataset$label)

#### Make a MDS plot but first normalize the data ####

dataset <- normalize(dataset, dataset)$train

summary(dataset)

labels <- unique(dataset$label)

cols <- as.integer(dataset$label) + 1

d <- dist(dataset[,3:ncol(dataset)])

fit <- cmdscale(d, k = 2) # k is the number of dimensions.

x <- fit[,1]; y <- fit[,2]

#png("mdsFishes.png", width = 5, height = 4, units = "in", res = 720)
plot(x, y, xlab="Coordinate 1", ylab="Coordinate 2", main="MDS trajectories features", pch=19, col=cols, cex=0.7)
legend("topleft", legend = labels, pch=19, col=unique(cols), cex=0.7, horiz = F)
#dev.off()
