dataFile <- "data/webData.csv"

colClasses <- c("integer", rep("character", 4), "integer", 
                rep("character", 7), "integer", rep("character", 4))
#ratingData <- read.csv(dataFile, header=TRUE, sep=",", quote="\"",
#                       stringsAsFactors=FALSE,
#                       colClasses=colClasses)

# Make sure the dates are understood in the right format
convertDate <- function(s) { return(as.Date(s, format="%m/%d/%Y")) }
ratingData$INSPECTION.DATE <- convertDate(ratingData$INSPECTION.DATE)
ratingData$GRADE.DATE <- convertDate(ratingData$GRADE.DATE)
ratingData$RECORD.DATE <- convertDate(ratingData$RECORD.DATE)

# make empty violation

# Get rid of inspections with a date of 1900-01-01, as these have 
# restaurants not been inspected yet
ratingData <- ratingData[ratingData$INSPECTION.DATE != "1900-01-01", ]

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
restaurants <- ratingData[!duplicated(ratingData$CAMIS), restaurantColumns]

# create a frame with a row for each inspection
inspectionRows <- !duplicated(paste(ratingData$CAMIS, ratingData$INSPECTION.DATE))
inspections <- ratingData[inspectionRows, inspectionColumns]

# add column to restaurant frame for the number of inspections
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

violationCodes <- unique(ratingData$VIOLATION.CODE)
violationCodes <- violationCodes[violationCodes != ""] # exclude the empty violation
nCodes <- length(violationCodes)
# Create a matrix where each row corresponds to a "histogram"-like tally of
# violations for a restaurant
# This helper function creates one column of the matrix (i.e. for one violation)
makeViolationVector <- function(violation) {
  vRows <- ratingData$VIOLATION.CODE == violation
  result <- vapply(restaurants$CAMIS, function(id) sum((ratingData$CAMIS == id) & vRows),
                   FUN.VALUE=integer(1))
  return(result / restaurants$N.INSPECTIONS)
}
violation <- sapply(violationCodes, makeViolationVector, simplify="matrix")
#names(violationVectors) <- restaurants$CAMIS