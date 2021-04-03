source(file.path("..","auxiliary_functions","globals.R"))
source(file.path("..","auxiliary_functions","functions.R"))
library(R.matlab)

# Dataset links and related papers:
# http://groups.inf.ed.ac.uk/f4k/GROUNDTRUTH/BEHAVIOR/
# http://groups.inf.ed.ac.uk/f4k/
# C. Beyan, R. B. Fisher, (2013), Detection of Abnormal Fish Trajectories Using a Clustering Based Hierarchical Classifier, British Machine Vision Conference (BMVC), Bristol, UK.
# C. Beyan, R. B. Fisher, (2013), Detecting abnormal fish trajectories using clustered and labelled data, Proc. of International Conference on Image Processing (ICIP), Melbourne, Australia.
# C.Beyan, R. B. Fisher, (2012), A Filtering Mechanism for Normal Fish Trajectories, Proc. of 21th International Conference on Pattern Recognition (ICPR), Tsukuba Science City, Japan. 


# Read data.
df <- readMat(file.path(datasets_path,
                        "fish_trajectories","fishDetections_total3102.mat"))$fish.detections

# Print data frame dimensions.
dim(df)

# Read one of the trajectories.
trj <- df[,,1]

# Inspect its structure.
str(trj)

# Count how many frames this trajectory has.
length(trj$bounding.box.x)

#### Average number of frames ####
nframes <- NULL
for(i in 1:dim(df)[3]){
  trj <- df[,,i]
  nframes <- c(nframes, nrow(trj$frame.number))
}

# Average number of frames.
mean(nframes)

hist(nframes)

boxplot(nframes)


# The following code will generate an animated visualization of the trajectories.

# Convert to data frame to be plotted with anipaths package
trajectories <- NULL

minFrames <- 10 # Trajectories with less than minFrames will not be considered.

startTime <- unclass(Sys.time())

for(i in 1:dim(df)[3]){
  trj <- df[,,i]
  if(length(trj$frame.number) < minFrames)next;
  # Compute center of bounding box.
  x.coord <- trj$bounding.box.x + (trj$bounding.box.w / 2)
  y.coord <- trj$bounding.box.y + (trj$bounding.box.h / 2)
  label <- trj$classDescription # 1 normal, 2 abnormal.
  
  # Generate PosiXct times.
  t <- 1:length(trj$frame.number)
  times <- as.POSIXct(startTime + t, origin = "1960-01-01")
  
  tmp <- data.frame(times, x.coord, y.coord, label, id = i)
  
  trajectories <- rbind(trajectories, tmp)
}

trajectories$label <- as.factor(trajectories$label)

# Count how many abnormals.
idx <- which(trajectories$label == "abnormal")

length(unique(trajectories$id[idx]))

trj.normal <- trajectories[-idx,]

trj.abnormal <- trajectories[idx,]

# Select 50 normal trajectories.
sel.normal <- head(unique(trj.normal$id),50)

# Select 10 abnormal trajectories.
sel.abnormal <- unique(trj.abnormal$id)[0:10]

trj.selected <- trajectories[which(trajectories$id %in% c(sel.normal, sel.abnormal)), ]

library(anipaths)

animate_paths(paths = trj.selected,
              n.frames = max(table(trj.selected$id)),
              interval = 1/2,
              plot.date = F,
              tail.wd = 0.2,
              tail.length = 100,
              pt.cex = 1.5,
              pt.colors = "white",
              legend.loc = NA,
              tail.colors = c(rep("blue",length(sel.normal)), rep("red",length(sel.abnormal))),
              covariate = "label",
              covariate.colors = c("blue","red"),
              coord = c("x.coord", "y.coord"),
              Time.name = "times",
              ID.name = "id")
