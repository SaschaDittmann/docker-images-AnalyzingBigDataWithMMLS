## ----chap03chunk01, include=FALSE----------------------------------------
source('scripts/01-Getting-started.R')

## ----chap03chunk02-------------------------------------------------------
rxGetInfo(nyc_xdf, getVarInfo = TRUE, numRows = 5)

## ----chap03chunk03-------------------------------------------------------
rxDataStep(nyc_xdf, nyc_xdf,
  transforms = list(
    tip_percent = ifelse(fare_amount > 0 & tip_amount < fare_amount, 
                         round(tip_amount * 100 / fare_amount, 0), 
                         NA)),
  overwrite = TRUE)
rxSummary( ~ tip_percent, nyc_xdf)

## ----chap03chunk04-------------------------------------------------------
rxSummary( ~ tip_percent2, nyc_xdf,
           transforms = list(
            tip_percent2 = ifelse(fare_amount > 0 & tip_amount < fare_amount, 
                                  round(tip_amount * 100 / fare_amount, 0), 
                                  NA)))

## ----chap03chunk05-------------------------------------------------------
rxCrossTabs( ~ month:year, nyc_xdf,
             transforms = list(
               year = as.integer(substr(pickup_datetime, 1, 4)),
               month = as.integer(substr(pickup_datetime, 6, 7)),
               year = factor(year, levels = 2014:2016),
               month = factor(month, levels = 1:12)))

## ----chap03chunk06-------------------------------------------------------
rxCrossTabs( ~ month:year, nyc_xdf,
             transforms = list(
               date = ymd_hms(pickup_datetime),
               year = factor(year(date), levels = 2014:2016),
               month = factor(month(date), levels = 1:12)),
             transformPackages = "lubridate")

## ----chap03chunk07-------------------------------------------------------
# transformation function for extracting some date and time features
xforms <- function(data) {
  
  wlabels <- c('Sun', 'Mon', 'Tue', 'Wed', 'Thu', 'Fri', 'Sat')
  cut_levels <- c(1, 5, 9, 12, 16, 18, 22)
  hour_labels <- c('1AM-5AM', '5AM-9AM', '9AM-12PM', '12PM-4PM', 
                   '4PM-6PM', '6PM-10PM', '10PM-1AM')
  
  pickup_datetime <- ymd_hms(data$pickup_datetime, tz = "UTC")
  pickup_hour <- addNA(cut(hour(pickup_datetime), cut_levels))
  pickup_dow <- factor(wday(pickup_datetime), levels = 1:7, labels = wlabels)
  levels(pickup_hour) <- hour_labels
  
  dropoff_datetime <- ymd_hms(data$dropoff_datetime, tz = "UTC")
  dropoff_hour <- addNA(cut(hour(dropoff_datetime), cut_levels))
  dropoff_dow <- factor(wday(dropoff_datetime), levels = 1:7, labels = wlabels)
  levels(dropoff_hour) <- hour_labels
  
  data$pickup_hour <- pickup_hour
  data$pickup_dow <- pickup_dow
  data$dropoff_hour <- dropoff_hour
  data$dropoff_dow <- dropoff_dow
  data$trip_duration <- as.integer(as.duration(dropoff_datetime - pickup_datetime))
  
  data
}

## ----chap03chunk08-------------------------------------------------------
library(lubridate)
Sys.setenv(TZ = "US/Eastern") # not important for this dataset
head(xforms(nyc_sample)) # test the function on a data.frame

## ----chap03chunk09-------------------------------------------------------
head(rxDataStep(nyc_sample, transformFunc = xforms, transformPackages = "lubridate"))

## ----chap03chunk10-------------------------------------------------------
st <- Sys.time()
rxDataStep(nyc_xdf, nyc_xdf, overwrite = TRUE, transformFunc = xforms, 
           transformPackages = "lubridate")
Sys.time() - st

## ----chap03chunk11-------------------------------------------------------
rxs1 <- rxSummary( ~ pickup_hour + pickup_dow + trip_duration, nyc_xdf)
# we can add a column for proportions next to the counts
rxs1$categorical <- lapply(rxs1$categorical, 
  function(x) cbind(x, prop = round(prop.table(x$Counts), 2)))
rxs1

## ----chap03chunk12-------------------------------------------------------
rxs2 <- rxSummary( ~ pickup_dow:pickup_hour, nyc_xdf)
rxs2 <- tidyr::spread(rxs2$categorical[[1]], key = 'pickup_hour', value = 'Counts')
row.names(rxs2) <- rxs2[ , 1]
rxs2 <- as.matrix(rxs2[ , -1])
rxs2

## ----chap03chunk13-------------------------------------------------------
levelplot(prop.table(rxs2, 2), cuts = 10, xlab = "", ylab = "", 
          main = "Distribution of taxis by day of week")

## ----chap03chunk14-------------------------------------------------------
library(rgeos)
library(maptools)

nyc_shapefile <- readShapePoly(file.path(data_dir, '/ZillowNeighborhoods-NY/ZillowNeighborhoods-NY.shp'))
mht_shapefile <- subset(nyc_shapefile, City == 'New York' & County == 'New York')

mht_shapefile@data$id <- as.character(mht_shapefile@data$Name)
library(ggplot2)
mht_points <- fortify(gBuffer(mht_shapefile, byid = TRUE, width = 0), region = "Name")
library(dplyr)
mht_df <- inner_join(mht_points, mht_shapefile@data, by = "id")
mht_cent <- cbind(mht_shapefile@data, as.data.frame(gCentroid(mht_shapefile, byid = TRUE)))

library(ggrepel)
ggplot(mht_df, aes(long, lat, fill = id)) +
  geom_polygon() +
  geom_path(color = "white") +
  coord_equal() +
  theme(legend.position = "none") +
  geom_text_repel(aes(label = id, x = x, y = y), data = mht_cent, size = 3)

## ----chap03chunk15-------------------------------------------------------
# take only the coordinate columns, and replace NAs with 0
data_coords <- data.frame(
  long = ifelse(is.na(nyc_sample$pickup_longitude), 0, nyc_sample$pickup_longitude),
  lat = ifelse(is.na(nyc_sample$pickup_latitude), 0, nyc_sample$pickup_latitude)
  )
# we specify the columns that correspond to the coordinates
coordinates(data_coords) <- c('long', 'lat')
# returns the neighborhoods based on coordinates
nhoods <- over(data_coords, nyc_shapefile)
# rename the column names in nhoods
names(nhoods) <- paste('pickup', tolower(names(nhoods)), sep = '_')
# combine the neighborhood information with the original data
nyc_sample <- cbind(nyc_sample, nhoods)
head(nyc_sample)

## ----chap03chunk16, eval=FALSE-------------------------------------------
## find_nhoods <- function(data) {
##   # extract pick-up lat and long and find their neighborhoods
##   # add only the pick-up neighborhood and city columns to the data
##   # extract drop-off lat and long and find their neighborhoods
##   # add only the drop-off neighborhood and city columns to the data
##   # return the data with the new columns added in
## }

## ----chap03chunk17, eval=FALSE-------------------------------------------
## # test the function on a data.frame using rxDataStep
## head(rxDataStep(nyc_sample,
##                 transformFunc = find_nhoods,
##                 transformPackages = c("sp", "maptools"),
##                 transformObjects = list(shapefile = nyc_shapefile)))

## ----chap03chunk18-------------------------------------------------------
rxDataStep(nyc_xdf, nyc_xdf,
  transforms = list(
    rate_code_id = as.factor(rate_code_id),
    rate_code_id = factor(rate_code_id, 
                          levels = 1:6, 
                          labels = c('standard', 'JFK', 'Newark', 'Nassau or Westchester', 
                                     'negotiated', 'group ride')),
    payment_type = factor(payment_type, levels = 1:2, labels = c('card', 'cash'))
  ),
  overwrite = TRUE)

## ----chap03chunk19-------------------------------------------------------
nhoods <- over(data_coords, mht_shapefile)
str(nhoods)

## ----chap03chunk20-------------------------------------------------------
find_nhoods <- function(data) {
  # extract pick-up lat and long and find their neighborhoods
  pickup_longitude <- ifelse(is.na(data$pickup_longitude), 0, data$pickup_longitude)
  pickup_latitude <- ifelse(is.na(data$pickup_latitude), 0, data$pickup_latitude)
  data_coords <- data.frame(long = pickup_longitude, lat = pickup_latitude)
  coordinates(data_coords) <- c('long', 'lat')
  nhoods <- over(data_coords, shapefile)
  # add only the pick-up neighborhood and city columns to the data
  data$pickup_nhood <- nhoods$Name
  data$pickup_borough <- nhoods$County
  # extract drop-off lat and long and find their neighborhoods
  dropoff_longitude <- ifelse(is.na(data$dropoff_longitude), 0, data$dropoff_longitude)
  dropoff_latitude <- ifelse(is.na(data$dropoff_latitude), 0, data$dropoff_latitude)
  data_coords <- data.frame(long = dropoff_longitude, lat = dropoff_latitude)
  coordinates(data_coords) <- c('long', 'lat')
  nhoods <- over(data_coords, shapefile)
  # add only the drop-off neighborhood and city columns to the data
  data$dropoff_nhood <- nhoods$Name
  data$dropoff_borough <- nhoods$County
  # return the data with the new columns added in
  data
}

## ----chap03chunk21-------------------------------------------------------
# test the function on a data.frame using rxDataStep
head(rxDataStep(nyc_sample, transformFunc = find_nhoods, 
                transformPackages = c("sp", "maptools"), 
                transformObjects = list(shapefile = nyc_shapefile)))

## ----chap03chunk22-------------------------------------------------------
st <- Sys.time()
rxDataStep(nyc_xdf, nyc_xdf, overwrite = TRUE, 
           transformFunc = find_nhoods, 
           transformPackages = c("sp", "maptools", "rgeos"), 
           transformObjects = list(shapefile = nyc_shapefile))

Sys.time() - st
rxGetInfo(nyc_xdf, numRows = 5)

