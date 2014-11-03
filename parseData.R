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

