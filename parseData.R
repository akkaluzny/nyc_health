dataDirectory <- "data"
dataFile <- paste0(dataDirectory, "/webData.csv")
takeSample <- FALSE  # TRUE to sample the set before parsing
fraction <- 0.4      # the fraction of the number of restaurants for the sample

startTime <- proc.time()[3]

colClasses <- c("integer", rep("character", 4), "integer", 
                rep("character", 7), "integer", rep("character", 4))
print("Reading in data...")
ratingData <- read.csv(dataFile, header=TRUE, sep=",", quote="\"",
                       stringsAsFactors=FALSE,
                       colClasses=colClasses)

# Make sure the dates are understood in the right format
print("Converting dates...")
convertDate <- function(s) { return(as.Date(s, format="%m/%d/%Y")) }
ratingData$INSPECTION.DATE <- convertDate(ratingData$INSPECTION.DATE)
ratingData$GRADE.DATE <- convertDate(ratingData$GRADE.DATE)
ratingData$RECORD.DATE <- convertDate(ratingData$RECORD.DATE)

# Get rid of inspections with a date of 1900-01-01, as these are for 
# restaurants not been inspected yet
ratingData <- ratingData[ratingData$INSPECTION.DATE != "1900-01-01", ]

# create a subset of the data, to make testing go faster
if (takeSample) {
  print("Taking sample...")
  uniqueRestaurants <- unique(ratingData$CAMIS)
  nRestaurants <- length(uniqueRestaurants)
  sampleVector <- sample(uniqueRestaurants, as.integer(fraction * nRestaurants))
  ratingData <- ratingData[ratingData$CAMIS %in% sampleVector, ]
  # create a random testID from the sample, for help with testing/debugging 
  testID <- sample(ratingData$CAMIS, 1)
}
# Check to see that addresses are uniquely associated with the CAMIS ids
# note this is not optimized and will take a while to run
#ii <- 0
#for (id in unique(ratingData$CAMIS)) {
#  restData <- ratingData[ratingData$CAMIS==id, c("BUILDING", "STREET")]
#  address <- paste(restData$BUILDING, restData$STREET)
#  if (length(unique(address)) > 1) {
#    print(id)
#    print(unique(address))
#    print("")
#  }
#  if (ii %% 100 == 0) {
#    print(ii)
#  }
#  ii <- ii+1
#}


restaurantColumns <- c("CAMIS", "DBA", "BORO", "BUILDING", "STREET", "ZIPCODE",
                       "PHONE", "CUISINE.DESCRIPTION")
inspectionColumns <- c("CAMIS", "INSPECTION.DATE", "ACTION", "SCORE",
                       "GRADE", "GRADE.DATE", "RECORD.DATE", "INSPECTION.TYPE")

# create a data frame that contains just the restaurant information
print("Creating restaurant data frame...")
restaurants <- ratingData[!duplicated(ratingData$CAMIS), restaurantColumns]

# create a frame with a row for each inspection
print("Creating inspection data frame...")
inspectionRows <- !duplicated(paste(ratingData$CAMIS, ratingData$INSPECTION.DATE))
inspections <- ratingData[inspectionRows, inspectionColumns]

# add column to restaurant frame for the number of inspections
print("Adding number of inspections column...")
restaurants$N.INSPECTIONS <- vapply(restaurants$CAMIS,
                                    function(id) length(inspections[inspections$CAMIS==id, 1]),
                                    FUN.VALUE=integer(1))

# function to produce a string that contains the violation codes for a given inspection
makeViolationString <- function(id, date) {
  violations <- ratingData[ratingData$CAMIS==id & ratingData$INSPECTION.DATE==date,
                           "VIOLATION.CODE"]
  return(paste(violations, collapse=" "))
}
# create the above string for each inspection and add it as a new column
## UNTESTED
#inspections$VIOLATIONS <- mapply(makeViolationString,
#                                 inspections$CAMIS, inspections$INSPECTION.DATE,
#                                 SIMPLIFY="vector")

print("Creating matrix of restaurant violations...")
violationCodes <- unique(ratingData$VIOLATION.CODE)
violationCodes <- violationCodes[violationCodes != ""] # exclude the empty violation
nCodes <- length(violationCodes)
# we'll construct the transpose of the desired matrix, as column operations are 
# more effecient
violationMatrix <- matrix(numeric(nCodes * nrow(restaurants)), nrow=nCodes)
rownames(violationMatrix) <- violationCodes
colnames(violationMatrix) <- restaurants$CAMIS
for (id in restaurants$CAMIS) {
  violations <- ratingData[ratingData$CAMIS == id, "VIOLATION.CODE"]
  idCode <- as.character(id)
  for (v in violations) {
    if (v != "") {
      violationMatrix[v, idCode] <- violationMatrix[v, idCode] + 1
    }
  }
  n <- restaurants[restaurants$CAMIS == id, "N.INSPECTIONS"]
  if (n > 0) {
    violationMatrix[, idCode] <- violationMatrix[, idCode] / n
  }
  rm(violations)
}
violationMatrix <- t(violationMatrix)
## OLD METHOD -- slow
# Create a matrix where each row corresponds to a "histogram"-like tally of
# violations for a restaurant
# This helper function creates one column of the matrix (i.e. for one violation)
#makeViolationVector <- function(violation) {
#  vRows <- ratingData$VIOLATION.CODE == violation
#  result <- vapply(restaurants$CAMIS, function(id) sum((ratingData$CAMIS == id) & vRows),
#                   FUN.VALUE=integer(1))
#  return(result / restaurants$N.INSPECTIONS)
#}
#violationMatrix <- sapply(violationCodes, makeViolationVector, simplify="matrix")
#rownames(violationMatrix) <- restaurants$CAMIS

# helper function to go from score to grade--A -> 1, etc.
getGrade <- function(score) {
  result <- as.integer(2 + 0 * score)
  result[score <= 13] <- 1
  result[score >= 28] <- 3
  return(result)
}

# Create matrix of transition matrices for each restaurant,
# also create a column in the inspection frame for the time since last inspection
# and time to next inspection
print("Creating transition matrices...")
inspections$SINCE.LAST <- numeric(nrow(inspections))
inspections$UNTIL.NEXT <- numeric(nrow(inspections))
# we'll construct the transpose, as its easier to build up by columns of 
# transition matrices
transitionMatrices <- matrix(, nrow=9, ncol=nrow(restaurants))
rownames(transitionMatrices) <- c("AA", "AB", "AC", 
                                  "BA", "BB", "BC",
                                  "CA", "CB", "CC")
colnames(transitionMatrices) <- restaurants$CAMIS
# sort the inspection table
inspections <- inspections[order(inspections$CAMIS, inspections$INSPECTION.DATE), ]
# loop over each restaurant and fill in the matrices and column
for (id in restaurants$CAMIS) {
  insp <- inspections[inspections$CAMIS==id, c("SCORE", "INSPECTION.DATE")]
  sinceLast <- numeric(nrow(insp))
  untilNext <- numeric(nrow(insp))
  sinceLast[1] <- NA # no time since last for the first inspection
  untilNext[nrow(insp)] <-NA # no time to next for the last
  transMatrix <- numeric(9)
  grades <- getGrade(insp[, "SCORE"])
  n <- length(grades)
  if (n > 1) {
    for (ii in 2:n) {
      lastGrade <- grades[ii-1]
      grade <- grades[ii]
      transMatrix[(lastGrade-1)*3 + grade] <- transMatrix[(lastGrade-1)*3 + grade] + 1
      timeBetween <- difftime(insp[ii, 2], insp[ii-1, 2], units="days")
      sinceLast[ii] <- timeBetween
      untilNext[ii-1] <- timeBetween
    }
    ## do the scaling for each "column" of the transition matrix
    for (ii in 1:3) {
      nGrade <- sum(grades==ii, na.rm=TRUE)
      if (nGrade > 0) {
        transMatrix[(1:3) + (ii-1)*3] <-  transMatrix[(1:3) + (ii-1)*3] / nGrade
      }
    }
  }
  transitionMatrices[, as.character(id)] <- transMatrix
  inspections[inspections$CAMIS==id, "SINCE.LAST"] <- sinceLast
  inspections[inspections$CAMIS==id, "UNTIL.NEXT"] <- untilNext
  rm(insp, grades, transMatrix, sinceLast, untilNext) # might not be needed
}
transitionMatrices <- t(transitionMatrices)

print("Saving data...")
write.table(inspections, paste0(dataDirectory, "/inspectionData.csv"), 
            row.names=FALSE, sep=",")
write.table(restaurants, paste0(dataDirectory, "/restaurantData.csv"), 
            row.names=FALSE, sep=",")
write.table(violationMatrix, paste0(dataDirectory, "/violationData.csv"), 
            row.names=TRUE, sep=",")
write.table(transitionMatrices, paste0(dataDirectory, "/transitionData.csv"), 
            row.names=TRUE, sep=",")

print("Done.")
cat("Time taken: ", proc.time()[3] - startTime, "s\n")