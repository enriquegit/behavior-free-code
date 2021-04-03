source(file.path("..","auxiliary_functions","globals.R"))

library(grid)

filepath <- file.path(datasets_path,"smartphone_activities","images.txt")

df <- read.csv(filepath)

M <- as.matrix(df[,-c(301,302)]) # Remove class and user id.

minv <- min(M); maxv <- max(M) # Obtain max and min for normalization.

# jogging
jogging <- as.vector(t(df[10,1:300])) # extract one of the jogging activities.

# Normalize
jogging <- (jogging - minv) / (maxv - minv)

# Convert into an array with dimension (10,10,3)
arr.jogging <- array(jogging, c(10,10,3))

dim(arr.jogging)

# Plot the array.
grid.raster(arr.jogging, interpolate = FALSE)

# walking
walking <- as.vector(t(df[70,1:300]))

walking <- (walking - minv) / (maxv - minv)

arr.walking <- array(walking, c(10,10,3))

grid.raster(arr.walking, interpolate=FALSE)

# Sitting
sitting <- as.vector(t(df[241,1:300]))

sitting <- (sitting - minv) / (maxv - minv)

arr.sitting <- array(sitting, c(10,10,3))

grid.raster(arr.sitting, interpolate=FALSE)
