source(file.path("..","..","auxiliary_functions","globals.R"))
library(keras)
library(pixmap)
library(abind)
library(caret)


# The ppm files can be downloaded from: http://conradsanderson.id.au/lfwcrop/ (the color version).
# The SMILE_list.txt and NON-SMILE_list.txt can be obtained from: https://data.mendeley.com/datasets/yz4v8tb3tp/5
# Please, read instructions on Appendix B on how to install this dataset.
datapath <- file.path(datasets_path,"smiles")

# Read smiling labels.
smile.list <- read.table(file.path(datapath,"SMILE_list.txt"))

head(smile.list)

#### We need to do some preprocessing ####

# Substitute jpg with ppm.
smile.list <- gsub("jpg", "ppm", smile.list$V1)

# Remove txt entries.
smile.list <- smile.list[-which(smile.list %in% c("listt.txt","SMILE_list.txt"))]

# Read non-smiling labels.
nosmile.list <- read.table(file.path(datapath,"NON-SMILE_list.txt"))

# Substitute jpg with ppm.
nosmile.list <- gsub("jpg", "ppm", nosmile.list$V1)

#########################
#### Plot some faces ####
#########################

# Read an smiling face.
img <- read.pnm(file.path(datapath,"faces", smile.list[10]), cellres = 1)

# Plot the image.
plot(img)

# Print its properties.
print(img)

# Read a non-smiling face.
img <- read.pnm(file.path(datapath,"faces", nosmile.list[129]), cellres = 1)

# Plot the image.
plot(img)

# Read all smiling images (may take some minutes).
smiling.images <- NULL; adim <- 0
for(f in smile.list){
  img <- read.pnm(file.path(datapath,"faces", f), cellres = 1)
  pixels <- getChannels(img) # Get the pixels.
  smiling.images <- abind(smiling.images, pixels, along = adim) # Stack images.
  adim <- 1
}

# Print dimensions.
dim(smiling.images)


# Read all non-smiling images (may take some minutes).
nonsmiling.images <- NULL; adim <- 0
for(f in nosmile.list){
  img <- read.pnm(file.path(datapath,"faces", f), cellres = 1)
  pixels <- getChannels(img) # Get the pixels.
  nonsmiling.images <- abind(nonsmiling.images, pixels, along = adim) # Stack images.
  adim <- 1
}

# Print dimensions.
dim(nonsmiling.images)

#### Normalize. ####
max(smiling.images)
min(smiling.images)

# The images are already between 0 and 1 so no need to normalize.

#### Build train and test sets. ####

# Generate labels as integers.
labels <- c(rep(1, length(smile.list)), rep(0, length(nosmile.list)))

# Store smiling and non-smiling images in the same structure.
dataset <- abind(smiling.images, nonsmiling.images, along = 1)

# Shuffle the dataset rows.
set.seed(1234)
idxs <- sample(length(labels), size = length(labels), replace = F)
dataset <- dataset[idxs,,,]
labels <- labels[idxs]

# Select 15% for test set.
idxs <- sample(nrow(dataset), size = nrow(dataset) * 0.15, replace = F)

testX <- dataset[idxs,,,]
testY <- to_categorical(labels[idxs], num_classes = 2)

trainX <- dataset[-idxs,,,]
trainY <- to_categorical(labels[-idxs], num_classes = 2)

#### Build the CNN model. ####

model <- keras_model_sequential()

model %>%
  layer_conv_2d(filters = 8,
                kernel_size = c(3,3),
                activation = 'relu',
                input_shape = c(64,64,3)) %>%
  layer_max_pooling_2d(pool_size = c(2, 2)) %>%
  layer_dropout(0.25) %>%
  layer_conv_2d(filters = 16,
                kernel_size = c(3,3),
                activation = 'relu') %>%
  layer_max_pooling_2d(pool_size = c(2, 2)) %>%
  layer_dropout(0.25) %>%
  layer_flatten() %>%
  layer_dense(units = 32, activation = 'relu') %>%
  layer_dropout(0.5) %>%
  layer_dense(units = 2, activation = 'softmax')

# Print summary.
summary(model)

# Compile the model.
model %>% compile(
  loss = 'categorical_crossentropy',
  optimizer = optimizer_sgd(lr = 0.01),
  metrics = c("accuracy")
)

# Fit the model.
history <- model %>% fit(
  trainX, trainY,
  epochs = 50,
  batch_size = 8,
  validation_split = 0.10,
  verbose = 1,
  view_metrics = TRUE
)

plot(history)

# Save model (optional).
#save_model_hdf5(model, "smiles.hdf5")
#save_model_tf(model, "smiles_tf")

# Load model (optional to reproduce the book's results).
#model <- load_model_hdf5("smiles.hdf5")

# Alternatively, load the SavedModel version.
#model <- load_model_tf("smiles_tf")

# Evaluate model on test set.
model %>% evaluate(testX, testY)

# Predict classes.
classes <- model %>% predict_classes(testX)

# Make predictions with test set.
predictions <- model %>% predict_on_batch(testX)

classes <- max.col(predictions) - 1

groundTruth <- max.col(testY) - 1

# Compute accuracy.
sum(classes == groundTruth) / length(classes)

# Convert classes to strings.
# Class mapping by index: rock 0, scissors 1, paper 2, ok 3.
mapping <- c("nosmile", "smile")
str.predictions <- mapping[classes+1] # Need to sum 1 because indices in R start at 1.
str.groundTruth <- mapping[groundTruth+1]

cm <- confusionMatrix(as.factor(str.predictions),
                      as.factor(str.groundTruth),
                      positive = "smile")

# Print sensitivity.
cm$byClass["Sensitivity"]


#######################
#### Plot results. ####
#######################

transposeImages <- function(x){
  # Define function to rotate images.
  # source: https://stat.ethz.ch/pipermail/r-help/2009-May/391421.html
  y <- array(NA,c(dim(x)[1],dim(x)[2],dim(x)[3],dim(x)[4]))

  for(i in 1:dim(x)[1]){
    for(j in 1:dim(x)[4])
      y[i,,,j] <- t(rev(x[i,,,j]))
  }
  return (y)
}


# https://stackoverflow.com/questions/62134844/how-to-plot-an-rgb-image-using-the-image-function-from-a-3d-array

data <- testX[1:16,,,]

data <- transposeImages(data)

#png("cnn_smile_predictions.png", width = 5, height = 5, units = "in", res = 300)
par(mfrow=c(4, 4), mar=c(0, 0, 3, 0))
sapply(1:16, function(x){
  plot.new()
  rasterImage(data[x,,,], xleft = 0, xright = 1,
              ytop = 0, ybottom = 1, angle = 0, interpolate = FALSE)

  title(paste0("", str.predictions[x]), line = 0.5,
        col.main=ifelse(str.predictions[x]==str.groundTruth[x],"green","red"))
})
#dev.off()
