library(dtw)


# Sequences from the book example.
query <- c(2,2,2,4,4,3)

ref <- c(2,2,3,3,2)

# Find dtw distance.
alignment <- dtw(query, ref, step = symmetric1, keep.internals = T)

alignment$localCostMatrix

alignment$distance

alignment$index1

alignment$index2

# Plot the backtracking.
ccm <- alignment$costMatrix
#png("backtracking.png", width = 6, height = 4, units = "in", res = 720)
image(x = 1:nrow(ccm), y = 1:ncol(ccm),
      ccm, xlab = "Q", ylab = "R")
text(row(ccm), col(ccm), label = ccm)
lines(alignment$index1, alignment$index2)
#dev.off()

# Plot aligned sequences
plot(alignment, type="two", off=1.5,
     match.lty=2,
     match.indices=10,
     main="DTW resulting alignment",
     xlab="time", ylab="magnitude")


################################
#### Generate example plots ####
################################

query <- c(2,2,2,4,4,3)

ref <- c(2,2,3,3,2)

alignment <- dtw(query, ref, step = symmetric1, keep = T)

# Resulting distance.
alignment$distance

#png("query_ref.png", width = 6, height = 4, units = "in", res = 720)
plot(alignment, type="two", off=1.5, match.lty=2, match.indices=10, main="DTW resulting alignment", xlab="time", ylab="magnitude")
#dev.off()
