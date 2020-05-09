library(tidyverse)
library(ggmap)
library(geosphere)

geocode <- ggmap::geocode

# Functions End

library(readr)
places <- read_csv("places.csv")
View(places)
city <- as.character(places[1,2])
register_google(key = "AIzaSyAfnLNZjvYdMx-cyga_qA1oJ6P36dRGalA")

places2 <- apply(places[,2],1,geocode)

# Select the file from the file chooser
fileToLoad <- file.choose(new = TRUE)
fileToLoad <- places


# Read in the CSV data and store it in a variable 
origAddress <- read.csv(fileToLoad, stringsAsFactors = FALSE)

origAddress <- places[1:20,]
# Initialize the data frame
geocoded <- data.frame(stringsAsFactors = FALSE)

# Loop through the addresses to get the latitude and longitude of each address and add it to the
# origAddress data frame in new columns lat and lon
for(i in 1:nrow(origAddress))
{
  # Print("Working...")
  result <- geocode(origAddress$Title[i], output = "latlona", source = "google")
  origAddress$lon[i] <- as.numeric(result[1])
  origAddress$lat[i] <- as.numeric(result[2])
  origAddress$geoAddress[i] <- as.character(result[3])
}
# Write a CSV file containing origAddress to the working directory
write.csv(origAddress, "geocoded.csv", row.names=FALSE)

