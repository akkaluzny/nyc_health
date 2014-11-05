dataDirectory <- "data"
restaurantData <- read.table(paste0(dataDirectory, "/restaurantData.csv"), 
                             header=TRUE, sep=",", stringsAsFactors=FALSE)
inspectionData <- read.table(paste0(dataDirectory, "/inspectionData.csv"), 
                             header=TRUE, sep=",", stringsAsFactors=FALSE)
violationData <- read.table(paste0(dataDirectory, "/violationData.csv"), 
                            header=TRUE, sep=",", stringsAsFactors=FALSE,
                            row.names=1, check.names=FALSE)
violationData <- as.matrix(violationData)
transitionData <- read.table(paste0(dataDirectory, "/transitionData.csv"), 
                             header=TRUE, sep=",", stringsAsFactors=FALSE,
                             row.names=1, check.names=FALSE)
transitionData <- as.matrix(transitionData)

# helper function for the elbow method
SSexplained <- function(k, data) {
  result <- kmeans(data, k, iter.max=20, nstart=4)
  return(result$betweenss / result$totss)
}
kValues <- 1:10
violationK <- vapply(kValues, function(k) SSexplained(k, violationData), numeric(1))
transitionK <- vapply(kValues, function(k) SSexplained(k, transitionData), numeric(1))
plot(kValues, violationK, type="b", main="k-Means Clustering of Violation Data",
     xlab="k", ylab="percent of variance explained")
plot(kValues, transitionK, type="b", main="k-Means Clustering of Transition Data",
     xlab="k", ylab="percent of variance explained")

# function to "re-scale" transition matrix to account for the fact that 
# 0s where used when no data about a transition was available
rescale <- function(M) {
  for (ii in 1:3) {
    total <- sum(M[(1:3) + (ii-1)*3])
    if (total > 0) {
      M[(1:3) + (ii-1)*3] <- M[(1:3) + (ii-1)*3] / total
    }
  }
  return(M)
}
# function to rescale a whole set of transition matrices
rescaleCenters <- function(centers) {
  centersScaled <- centers[,]
  for (ii in 1:nrow(centers)) {
    centers[ii, ] <- rescale(centers[ii, ])
  }
  return(centers)
}
# function to turn row representation of transitions into an actual matrix
rowToMatrix <- function(M) {
  M <- matrix(M, ncol=3)
  rownames(M) <- c("to A", "to B", "to C")
  colnames(M) <- c("from A", "from B", "from C")
  return(M)
}

# some overall stats
meanTimeSpacing <- mean(inspectionData$SINCE.LAST, na.rm=TRUE)
meanNumberInspections <- mean(restaurantData$N.INSPECTIONS)