source(file.path("..","..","auxiliary_functions","globals.R"))
source("bow_functions.R")

# Extract simple activities from train set.
extractSimpleActivities(train = TRUE)

# Extract simple activities from test set (may take some minutes).
extractSimpleActivities(train = FALSE)

# Cluster simple activities from train set and store centroids.
clusterSimpleActivities()

# Assign simple activities to cluster ids. Label them. (can take some minutes).
assignSimpleActivitiesToCluster()

# Generate histograms.
convertToHistogram()

############################################
######### Plot some histograms #############
############################################

histograms <- read.csv(file.path(constants$datapath,
                                 "test",
                                 "histograms",
                                 "histograms.csv"))

labels <- as.factor(colnames(histograms)[-1])

# Working
work <- histograms[15,]

frequencies <- t(work[,-1])

df.work <- data.frame(labels, freqs=frequencies)

#png(file = "complex_working.png", width = 6, height = 4, units = "in", res = 200)
barplot(df.work$X15, names.arg=df.work$labels, 
        xlab = "label",
        ylab = "Relative Freq.",
        col = "gray",
        main = "Complex activity: Working", las=2)
#dev.off()

# Exercising
exercise <- histograms[65,]

frequencies <- t(exercise[,-1])

df.exercise <- data.frame(labels, freqs=frequencies)

#png(file = "complex_exercising.png", width = 6, height = 4, units = "in", res = 200)
barplot(df.exercise$X65, names.arg=df.exercise$labels, 
        xlab = "label",
        ylab = "Relative Freq.",
        col = "gray",
        main = "Complex activity: Exercising", las=2)
#dev.off()
