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
inspectionColumns <- c("CAMIS", "INSPECTION.DATE", "ACTION", "VIOLATION.CODE",
                       "VIOLATION.DESCRIPTION", "CRITICAL.FLAG", "SCORE",
                       "GRADE", "GRADE.DATE", "RECORD.DATE", "INSPECTION.TYPE")

# create a data frame that contains just the restaurant information
restaurants <- ratingData[!duplicated(ratingData$CAMIS), restaurantColumns]

# create a frame with a row for each inspection
inspectionRows <- !duplicated(paste(ratingData$CAMIS, ratingData$INSPECTION.DATE))
inspections <- ratingData[inspectionRows, inspectionColumns]

