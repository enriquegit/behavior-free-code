# This script is used to clean the HOMICIDE REPORTS dataset, format it as a transactions object and save the results as a .RData file for further processing.

# The HOMICIDE REPORTS dataset can be downloaded from:
# https://www.kaggle.com/murderaccountability/homicide-reports

source(file.path("..","auxiliary_functions","globals.R"))

# Load required packages.
library(naniar)

# Path to dataset directory.
datapath <- file.path(datasets_path,"homicide_reports")

# Read dataset file.
dataset <- read.csv(file.path(datapath,"database.csv"), stringsAsFactors = F)

# Look for missing values.
gg_miss_var(dataset)

summary(dataset)

# Explore unique values.
table(dataset$Crime.Type)

table(dataset$Victim.Sex)

table(dataset$Victim.Race)

table(dataset$Victim.Ethnicity)

table(dataset$Perpetrator.Race)

table(dataset$Perpetrator.Ethnicity)

table(dataset$Relationship)

# Remove rows with 'Unknown' relationship.
dataset <- dataset[-which(dataset$Relationship == "Unknown"),]

table(dataset$Weapon)

# Remove rows with 'Unknown' weapon.
dataset <- dataset[-which(dataset$Weapon == "Unknown"),]

# Remove perpetrators < 10 years old.
dataset <- dataset[-which(dataset$Perpetrator.Age < 10),]

# Group ages into categories.
Perpetrator.CatAge <- character(nrow(dataset))
Perpetrator.CatAge[dataset$Perpetrator.Age < 13] <- "child"
Perpetrator.CatAge[dataset$Perpetrator.Age >= 13 & dataset$Perpetrator.Age <= 17] <- "teen"
Perpetrator.CatAge[dataset$Perpetrator.Age >= 18 & dataset$Perpetrator.Age <= 45] <- "adult"
Perpetrator.CatAge[dataset$Perpetrator.Age > 45] <- "lateAdulthood"

dataset$Perpetrator.CatAge <- Perpetrator.CatAge

# Select columns to keep.
selectcols <- c("Relationship", "Weapon", "Perpetrator.CatAge")

dataset <- dataset[,selectcols]

# Add 'R.' before relationship to make it easier to distinguish.
dataset$Relationship <- paste0("R.", dataset$Relationship)


#### Convert to a transactions format. First convert to logical matrix. ####

# Get all possible item values.
uniqvals <- unlist(apply(dataset, 2, unique))

# Create matrix.
M <- matrix(FALSE, nrow=nrow(dataset), ncol = length(uniqvals),
            dimnames = list(NULL, uniqvals))

# Populate matrix. This may take some minutes.
for(i in 1:nrow(M)){
  tmpitems <- t(apply(dataset[i,], 1, c))
  M[i,tmpitems] <- TRUE
}

# Load the arules package.
library(arules)

# Convert matrix into a transactions object.
transactions <- as(M, "transactions")

# Save results.
save(transactions, file=file.path(datapath,"transactions.RData"))
