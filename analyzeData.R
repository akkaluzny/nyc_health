dataDirectory <- "data"

# options for turning off parts of the analysis, for testing/debugging
# when the script is going to be sourced repeatedly into the same environment
rereadData <- FALSE
checkElbows <- FALSE
redoKMeans <- FALSE

if (rereadData) {
print("Reading in data...")
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
}

# helper function for the elbow method
SSexplained <- function(k, data) {
  result <- kmeans(data, k, iter.max=20, nstart=4)
  return(result$betweenss / result$totss)
}

if (checkElbows) {
  print("Checking elbows...")
  kValues <- 1:10
  violationK <- vapply(kValues, function(k) SSexplained(k, violationData), numeric(1))
  transitionK <- vapply(kValues, function(k) SSexplained(k, transitionData), numeric(1))
  plot(kValues, violationK, type="b", main="k-Means Clustering of Violation Data",
       xlab="k", ylab="% of variance explained")
  plot(kValues, transitionK, type="b", main="k-Means Clustering of Transition Data",
       xlab="k", ylab="% of variance explained")
}

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
print("Tabulating stats...")
meanTimeSpacing <- mean(inspectionData$SINCE.LAST, na.rm=TRUE)
meanNumberInspections <- mean(restaurantData$N.INSPECTIONS)

# k-means analysis
if (redoKMeans) {
print("Finding several means...")
  tranK5 <- kmeans(transitionData, 5, nstart=6)
  tranK9 <- kmeans(transitionData, 9, nstart=6)
  violK5 <- kmeans(transitionData, 5, nstart=6)
}

tightness <- function(kdata) {
  return (kdata$withinss / kdata$size)
}
cat("\nTightness of transition clusters (k=5)\n")
print(tightness(tranK5), digits=3)
cat("\nTightness of transition clusters (k=9)\n")
print(tightness(tranK9), digits=3)
cat("\nTightness of violation clusters (k=5)\n")
print(tightness(violK5), digits=3)
cat("\n")

# helper functions for plotting and recording data
boroughs <- c("BRONX", "BROOKLYN", "MANHATTAN", "QUEENS", "STATEN ISLAND")
allBoroughs <- factor(restaurantData$BORO, levels=boroughs)
overallBoroFreq <- tabulate(allBoroughs, nbins=5) / length(allBoroughs)
plotBorough <- function(boroData, setName) {
  boro <- factor(boroData, levels=boroughs)
  # scale and center the data based on the total frequency in each borough
  result <- tabulate(boro, nbins=5) / length(boroData) / overallBoroFreq - 1
  names(result) <- levels(boro)
  barplot(result, main=paste0("Boroughs for ", setName), ylim=c(-0.2, 0.2),
          ylab="relative ratio of frequencies")
  return(result)
}
plotTime <- function(sinceLast, setName) {
  hist(sinceLast, main=paste0("Time between inspections for ", setName), xlim=c(0, 730), col="gray")
}
plotScore <- function(scores, setName) {
  breaks <- 7 * (0:(as.integer(max(scores, na.rm=TRUE))/7 + 2)) - 1
  hist(scores, breaks=breaks, main=paste0("Scores for ", setName), xlim=c(0, 60), col="gray")
}
allCuisineTypes <- unique(restaurantData$CUISINE.DESCRIPTION)
allCuisines <- factor(restaurantData$CUISINE.DESCRIPTION, levels=allCuisineTypes)
overallCuisineFreq <- tabulate(allCuisines, nbins=length(allCuisineTypes)) / length(allCuisines)
cuisineFrequency <- function(cuisines) {
  cuisines <- factor(cuisines, levels=allCuisineTypes)
  result <- tabulate(cuisines, nbins=length(allCuisineTypes)) / length(cuisines) / overallCuisineFreq - 1
  names(result) <- levels(cuisines)
  return(result)
}
# time-weighted avg of scores
timeScore <- function(scores, untilNext) {
  # only involve times for scored inspections
  timeSum <- sum(untilNext[!is.na(scores)], na.rm=TRUE)
  timeScoreSum <- sum(untilNext * scores, na.rm=TRUE)
  return(timeScoreSum / timeSum)
}

# helper function for reporting data on some subset of restaurants
reportOnRestaurants <- function(restaurantIDs, setName) {
  result <- list()
  result$name <- setName
  result$n.restaurants <- length(restaurantIDs)
  inspectionIDs <- inspectionData$CAMIS %in% restaurantIDs
  restaurantIDs <- restaurantData$CAMIS %in% restaurantIDs
  result$avg.n.inspections <- mean(restaurantData$N.INSPECTIONS[restaurantIDs], na.rm=TRUE)
  result$var.n.inspections <- sd(restaurantData$N.INSPECTIONS[restaurantIDs], na.rm=TRUE)
  result$avg.score <- mean(inspectionData$SCORE[inspectionIDs], na.rm=TRUE)
  result$time.avg.score <- timeScore(inspectionData$SCORE[inspectionIDs],
                                     inspectionData$UNTIL.NEXT[inspectionIDs])
  result$avg.time <- mean(inspectionData$SINCE.LAST[inspectionIDs], na.rm=TRUE)
  result$cuisines <- cuisineFrequency(restaurantData$CUISINE.DESCRIPTION[restaurantIDs])
  plotBorough(restaurantData$BORO[restaurantIDs], setName)
  plotTime(inspectionData$SINCE.LAST[inspectionIDs], setName)
  plotScore(inspectionData$SCORE[inspectionIDs], setName)
  return(result)
}
printRestaurantReport <- function(clusterData) {
  cat(clusterData$name, "\n")
  cat("Cluster size: ", clusterData$n.restaurants, "\n")
  cat("Avg number of inspections: ", clusterData$avg.n.inspections, "\n")
  cat("SD in number of inspections: ", clusterData$var.n.inspections, "\n")
  cat("Avg score: ", clusterData$avg.score, "\n")
  cat("Time avg score: ", clusterData$time.avg.score, "\n")
  cat("Avg time between inspections: ", clusterData$avg.time, "\n")
  cat("Most frequent cuisines:\n")
  print(sort(clusterData$cuisines, decreasing=TRUE)[1:5], digits=3)
  cat("\n")
}

allRestaurants <- reportOnRestaurants(restaurantData$CAMIS, "all restaurants")
printRestaurantReport(allRestaurants)

chainNames <- unique(restaurantData$DBA[duplicated(restaurantData$DBA)])
chainIDs <- restaurantData$CAMIS[restaurantData$DBA %in% chainNames]
chainRestaurants <- reportOnRestaurants(chainIDs, "restaurants with multiple locations")
printRestaurantReport(chainRestaurants)

starbucksIDs <- restaurantData$CAMIS[restaurantData$DBA == "STARBUCKS" | restaurantData$DBA == "STARBUCKS COFFEE"]
starbucksRestaurants <- reportOnRestaurants(starbucksIDs, "Starbucks")
printRestaurantReport(starbucksRestaurants)

for (b in boroughs) {
  bIDs <- restaurantData$CAMIS[restaurantData$BORO == b]
  printRestaurantReport(reportOnRestaurants(bIDs, b))
}

for (ii in 1:nrow(tranK5$centers)) {
  restaurantIDs <- restaurantData$CAMIS[tranK5$cluster == ii]
  clusterData <- reportOnRestaurants(restaurantIDs, 
                      paste0("transition cluster ",ii,"/",nrow(tranK5$centers)))
  print(tranK5$centers[ii, ])
  printRestaurantReport(clusterData)
}

# principal components for visualizing clusters
pcResults <- prcomp(transitionData, scale=TRUE, center=TRUE)$x
colorSet <- rainbow(5)
for (ii in 1:5) {
  clusterIDs <- tranK5$cluster == ii
  if (ii == 1) {
    plot(pcResults[clusterIDs, 2], pcResults[clusterIDs, 1], col=colorSet[ii],
         xlim=c(-5, 3), ylim=c(-2, 5), xlab="component 2", ylab="component 1",
         main="Transition clusters (k=5) plotted on the first two principal components")
  } else {
    points(pcResults[clusterIDs, 2], pcResults[clusterIDs, 1], col=colorSet[ii])
  }
}
legend("bottomleft", legend=paste0("cluster ", 1:5), fill=colorSet)
for (ii in 1:5) {
  clusterIDs <- tranK5$cluster == ii
  if (ii == 1) {
    plot(pcResults[clusterIDs, 3], pcResults[clusterIDs, 1], col=colorSet[ii],
         xlim=c(-4, 4), ylim=c(-2, 5), xlab="component 3", ylab="component 1",
         main="Transition clusters (k=5) plotted on the first and third principal components")
  } else {
    points(pcResults[clusterIDs, 3], pcResults[clusterIDs, 1], col=colorSet[ii])
  }
}
legend("bottomleft", legend=paste0("cluster ", 1:5), fill=colorSet)


colorSet <- rainbow(9)
for (ii in 1:9) {
  clusterIDs <- tranK9$cluster == ii
  if (ii == 1) {
    plot(pcResults[clusterIDs, 2], pcResults[clusterIDs, 1], col=colorSet[ii],
         xlim=c(-5, 3), ylim=c(-2, 5), xlab="component 2", ylab="component 1",
         main="Transition clusters (k=9) plotted on the first two principal components")
  } else {
    points(pcResults[clusterIDs, 2], pcResults[clusterIDs, 1], col=colorSet[ii])
  }
}
legend("bottomleft", legend=paste0("cluster ", 1:9), fill=colorSet)