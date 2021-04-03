source(file.path("..","auxiliary_functions","globals.R"))

# Load required packages
library(arules)
library(arulesViz)

# Load preprocess data. See crimes_process.R for details on how this file was generated.
load(file.path(datasets_path,"homicide_reports","transactions.RData"))

# Print summary.
summary(transactions)

itemFrequencyPlot(transactions,
                  type = "relative",
                  topN = 15,
                  main = 'Item frequecies')

# Run apriori algorithm.
resrules <- apriori(transactions,
                    parameter = list(support = 0.001,
                                     confidence = 0.5,
                                     # Find rules with at least 2 items.
                                     minlen = 2,
                                     target = 'rules'
                    ))

# Print a summary of the results.
summary(resrules)

# Print the first n rules with highest lift in decreasing order.
inspect(sort(resrules, by='lift', decreasing = T)[1:20])

# Plot a default scatterplot of support v.s. confidence colored by lift.
plot(resrules)

# Customize scatterplot to make it interactive
# and plot support v.s. lift colored by confidence.
plot(resrules, engine = "htmlwidget",
     measure = c("support", "lift"),
     shading = "confidence")

# Generate a two-key plot.
plot(resrules, method = "two-key plot")

# Plot a graph.
plot(head(sort(resrules, by = "lift"), n=25),
     method = "graph",
     control=list(cex=.9),
     engine="htmlwidget")

# Opens a shiny app with several interactive plots.
# Requires shinythemes package.
ruleExplorer(resrules)

# Subset transactions.
rulesGirlfriend <- subset(resrules, subset = lhs %in% "R.Girlfriend")

# Print rules with highest lift.
inspect(head(rulesGirlfriend, n = 3, by = "lift"))
