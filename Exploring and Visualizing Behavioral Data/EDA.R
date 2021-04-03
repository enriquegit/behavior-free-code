source(file.path("..","auxiliary_functions","globals.R"))
library(ggplot2)

# Read activities dataset.
dataset <- read.csv(file.path(datasets_path,
                              "smartphone_activities",
                              "WISDM_ar_v1.1_transformed.csv"),
                    stringsAsFactors = T)

##################################
#### Print summary statistics ####
##################################

summary(dataset)

# Print first 5 columns, column 33, 35 and the last one (the class).
summary(dataset[,c(1:5,33,35,ncol(dataset))])

##################################
#### Plot class distributions ####
##################################

t <- table(dataset$class)
t <- as.data.frame(t)
colnames(t) <- c("class","count")

p <- ggplot(t, aes(x=class, y=count, fill=class)) +
  geom_bar(stat="identity", color="black") +
  theme_minimal() +
  geom_text(aes(label=count), vjust=-0.3, size=3.5) +
  scale_fill_brewer(palette="Set1")

#png("distact.png", width = 7, height = 4, units = "in", res = 720)
print(p)
#dev.off()

####################################
#### User-class sparsity matrix ####
####################################

sparsityMatrix <- function(dataset){
  # Function to generate a sparsity matrix.
  users <- as.character(sort(as.integer(unique(dataset$user))))
  noUsers <- length(users)
  all.labels <- as.character(unique(dataset$class))
  noLabels <- length(all.labels)
  
  M <- matrix(data = rep(0, noUsers*noLabels), nrow = noLabels, dimnames = list(classes=all.labels, users=users))
  
  for(u in users){
    user.labels <- unique(as.character(dataset[dataset$user == u, "class"]))
    M[user.labels,u] <- 1
  }
  csums <- colSums(M)
  cidxs <- sort(csums, decreasing = T, index.return = T)$ix
  
  rsums <- rowSums(M)
  ridxs <- sort(rsums, decreasing = T, index.return = T)$ix
  
  TMP <- M
  TMP[,1:noUsers] <- M[,cidxs]
  colnames(TMP) <- users[cidxs]
  
  TMP[1:noLabels,] <- TMP[ridxs,]
  rownames(TMP) <- all.labels[ridxs]
  
  return(TMP)
}

# Compute sparsity matrix.
SM <- sparsityMatrix(dataset)

# Plot the sparsity matrix.
tot <- nrow(SM) * ncol(SM); acum <- sum(SM)
sparsity <- 1 - acum/tot
txt <- paste0("sparsity = ",sprintf("%.2f",sparsity))
confusion <- as.data.frame(as.table(t(SM)))

plot0 <- ggplot()
plot0 <- plot0 + geom_tile(aes(x=users, y=classes, fill=Freq), data=confusion, color="black", size=0.1) + 
  labs(x=bquote(atop("user id",bold(.(txt)))), y=expression("labels")) +
  geom_text(aes(x=users,y=classes, label=sprintf("", Freq)),data=confusion, size=1, colour="black") +
  scale_fill_gradient(low="white", high="gray", name="counts")+
  theme(axis.text=element_text(colour="black", size=5),
        axis.text.x = element_text(angle = 0, hjust = 0.5),
        axis.title = element_text(face="bold", colour="black", size=9)
        ,legend.position="none", plot.margin = unit(c(.1,.1,.1,.1),"cm"))

#png("sparsitymatrix.png", width = 5, height = 2.5, units = "in", res = 720)
print(plot0)
#dev.off()

##################################
#### Box plots ###################
##################################
#png("boxplotres.png", width = 7, height = 5, units = "in", res = 720)
boxplot(RESULTANT ~ class, dataset)
#dev.off()

##################################
#### Correlation plots ###########
##################################

library(corrplot)

# Load home activities dataset.
dataset <- read.csv(file.path(datasets_path,
                              "home_tasks",
                              "sound_acc.csv"))

CORRS <- cor(dataset[,-1])

corrplot(CORRS, method = "square")

# Only plot the upper diagonal.
#png("corrhome.png", width = 5, height = 5, units = "in", res = 720)
corrplot(CORRS, diag = FALSE, tl.pos = "td", tl.cex = 0.5, method = "color", type = "upper")
#dev.off()

# Interactive plot.
library(qtlcharts) # library for interactive plots.

iplotCorr(dataset[,-1], reorder=F,
          chartOpts=list(cortitle="Correlation matrix", scattitle="Scatterplot"))


##################################
######## Plot time series ########
##################################

# Read the hand gesture '1' for user 1.
dataset <- read.csv(file.path(datasets_path,
                              "hand_gestures",
                              "1",
                              "1_20130703-120056.txt"),
                    header = F)

# Do some preprocessing.
type <- c(rep("x", nrow(dataset)), rep("y", nrow(dataset)), rep("z", nrow(dataset)))

type <- as.factor(type)

values <- c(dataset$V1, dataset$V2, dataset$V3)

t <- rep(1:nrow(dataset), 3)

df <- data.frame(timestep = t, type = type, values = values)

head(df)

tsPlot <- ggplot(data = df,
                   aes(x = timestep,
                       y = values,
                       colour = type,
                       linetype = type)) +
  ggtitle("Hand gesture '1', user 1") +
  xlab("Timestep") +
  ylab("Acceleration") +
  geom_line(aes(color=type)) +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5),
        legend.position="right",
        legend.key.width = unit(1.0,"cm"),
        legend.key.size = unit(0.5,"cm"))

#png("timeserieshand.png", width = 7, height = 4, units = "in", res = 720)
print(tsPlot)
#dev.off()

##################################
##### Interactive time series ####
##################################

library(dygraphs)

# Read the hand gesture '1' for user 1.
dataset <- read.csv(file.path(datasets_path,
                              "hand_gestures",
                              "1",
                              "1_20130703-120056.txt"),
                    header = F,
                    col.names = c("x","y","z"))

dataset <- cbind(timestep = 1:nrow(dataset), dataset)

dygraph(dataset)

dygraph(dataset, main = "Hand Gesture '1'") %>%
  dyOptions(fillGraph = TRUE, fillAlpha = 0.25) %>%
  dyEvent("10", "Point of interest", labelLoc = "top") %>%
  dyShading(from = "30", to = "40", color = "#CCCCCC")


##################################
#### Multidimensional scaling ####
##################################

### Simple example. ###

# Generate 3 2D random points.
x <- runif(3)
y <- runif(3)

df <- data.frame(x,y)

labels <- c("a","b","c")

print(df)

# Compute distance matrix.
dist(df)


### Plot mds of activities ###

dataset <- read.csv(file.path(datasets_path,
                              "home_tasks",
                              "sound_acc.csv"),
                              stringsAsFactors = T)

colNames <- names(dataset)

v2cols <- colNames[grep(colNames, pattern = "v2_")]

cols <- as.integer(dataset$label)

labels <- unique(dataset$label)

d <- dist(dataset[,v2cols])

fit <- cmdscale(d, k = 2) # k is the number of dimensions.

x <- fit[,1]; y <- fit[,2]

#png("mds2D.png", width = 5, height = 4, units = "in", res = 720)
plot(x, y, xlab="Coordinate 1", ylab="Coordinate 2", main="accelerometer view", pch=19, col=cols, cex=0.7)

legend("topleft",legend = labels, pch=19, col=unique(cols), cex=0.7, horiz = F)
#dev.off()

### Plot 3 dimensions.
library(scatterplot3d)

fit <- cmdscale(d,k = 3)
x <- fit[,1]; y <- fit[,2]; z <- fit[,3]

#png("mds3D.png", width = 5.2, height = 4, units = "in", res = 720)
scatterplot3d(x, y, z,
              xlab = "",
              ylab = "",
              zlab = "",
              main="Accelerometer features in 3D",
              pch=19,
              color=cols,
              tick.marks = F,
              cex.symbols = 0.5,
              cex.lab = 0.7,
              mar = c(1,0,1,0))

legend("topleft",legend = labels, pch=19, col=unique(cols), cex=0.7, horiz = F)
#dev.off()


##################################
########### Heat map #############
##################################

# Load auxiliary functions.
source("auxiliary_eda.R")

# Path to the depresjon dataset.
datapath <- file.path(datasets_path,"depresjon")

# Generate matrix with mean activity levels per hour for the control and condition groups.
map.control <- computeActivityHour(datapath, type = "control")

map.condition <- computeActivityHour(datapath, type = "condition")

# Normalize matrices.
res <- normalizeMatrices(map.control, map.condition)

# Plot the heat maps.
library(pheatmap)
library(gridExtra)

a <- pheatmap(res$M1, main="control group", cluster_row = FALSE, cluster_col = FALSE, show_rownames = T, show_colnames = T, legend = T, color = colorRampPalette(c("white", "blue"))(50))

b <- pheatmap(res$M2, main="condition group", cluster_row = FALSE, cluster_col = FALSE, show_rownames = T, show_colnames = T, legend = T, color = colorRampPalette(c("white", "blue"))(50))

# Save to pdf
#png(paste0("heatmaps.png"), width = 8, height = 8, units = "in", res = 200)
grid.arrange(a$gtable, b$gtable, nrow=2)
#dev.off()


##################################
########## Automated EDA #########
##################################

# Read HOME TASKS activities dataset.
dataset <- read.csv(file.path(datasets_path,"home_tasks","sound_acc.csv"))

library(DataExplorer)

# Plot the dataset structure.
plot_str(dataset)

# Print a summary of the dataset.
t(introduce(dataset))

# Generate a report.
create_report(dataset) # Saves the result in 'report.html' file.


library(inspectdf)

# Plot the distribution of labels.
#png(paste0("heatmapHomeTasks.png"), width = 6, height = 4, units = "in", res = 700)
show_plot(inspect_cat(dataset))
#dev.off()

# Print the actual counts.
table(dataset$label)
