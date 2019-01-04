## ----chap05chunk01, include=FALSE----------------------------------------
source('scripts/01-Getting-started.R')

## ----chap05chunk02-------------------------------------------------------
input_xdf <- file.path(data_dir, 'yellow_tripdata_2016_clean.xdf')
mht_xdf <- RxXdfData(input_xdf)

rxDataStep(nyc_xdf, mht_xdf,
  rowSelection = (passenger_count > 0 &
                  trip_distance >= 0 & trip_distance < 30 &
                  trip_duration > 0 & trip_duration < 60*60*24 &
                  !is.na(pickup_nb) &
                  !is.na(dropoff_nb) &
                  fare_amount > 0),
  transformPackages = "stringr",
  varsToDrop = c('extra', 'mta_tax', 'improvement_surcharge', 'total_amount', 
                 'pickup_borough', 'dropoff_borough', 'pickup_nhood', 'dropoff_nhood'),
  overwrite = TRUE)

## ----chap05chunk03-------------------------------------------------------
mht_sample <- rxDataStep(mht_xdf, rowSelection = (u < .01), transforms = list(u = runif(.rxNumRows)))

dim(mht_sample)

## ----chap05chunk04-------------------------------------------------------
library(ggmap)

# Note: The Maps Static API Usage Limits have changed. 
# Creating an API key and including it in your request allows you to track usage 
# in the Google Cloud Platform Console, and to purchase additional quota if required.
# https://developers.google.com/maps/documentation/maps-static/get-api-key
register_google(key = "<ADD YOUR API KEY HERE>")

map_13 <- get_map(location = c(lon = -73.98, lat = 40.76), zoom = 13)
map_14 <- get_map(location = c(lon = -73.98, lat = 40.76), zoom = 14)
map_15 <- get_map(location = c(lon = -73.98, lat = 40.76), zoom = 15)

q1 <- ggmap(map_14) +
  geom_point(aes(x = dropoff_longitude, y = dropoff_latitude), data = mht_sample, 
             alpha = 0.15, na.rm = TRUE, col = "red", size = .5) +
  theme_nothing(legend = TRUE)

q2 <- ggmap(map_15) +
  geom_point(aes(x = dropoff_longitude, y = dropoff_latitude), data = mht_sample, 
             alpha = 0.15, na.rm = TRUE, col = "red", size = .5) +
  theme_nothing(legend = TRUE)

library(gridExtra)
grid.arrange(q1, q2, ncol = 2)

## ----chap05chunk05-------------------------------------------------------
library(dplyr)
xydata <- transmute(mht_sample, 
                    long_std = dropoff_longitude / -74, 
                    lat_std = dropoff_latitude / 40)

start_time <- Sys.time()
rxkm_sample <- kmeans(xydata, centers = 300, iter.max = 2000, nstart = 50)
Sys.time() - start_time

# we need to put the centroids back into the original scale for coordinates
centroids_sample <- rxkm_sample$centers %>%
  as.data.frame %>%
  transmute(long = long_std*(-74), lat = lat_std*40, size = rxkm_sample$size)

head(centroids_sample)

## ----chap05chunk06-------------------------------------------------------
start_time <- Sys.time()
rxkm <- rxKmeans( ~ long_std + lat_std, data = mht_xdf, outFile = mht_xdf, 
                 outColName = "dropoff_cluster", centers = rxkm_sample$centers, 
                 transforms = list(long_std = dropoff_longitude / -74, 
                                   lat_std = dropoff_latitude / 40), 
                 blocksPerRead = 1, overwrite = TRUE, maxIterations = 100, 
                 reportProgress = -1)
Sys.time() - start_time

clsdf <- cbind(
transmute(as.data.frame(rxkm$centers), long = long_std*(-74), lat = lat_std*40),
size = rxkm$size, withinss = rxkm$withinss)

head(clsdf)

## ----chap05chunk07-------------------------------------------------------
centroids_whole <- cbind(transmute(as.data.frame(rxkm$centers), 
                                   long = long_std*(-74), lat = lat_std*40), 
                         size = rxkm$size, 
                         withinss = rxkm$withinss)

q1 <- ggmap(map_15) +
  geom_point(data = centroids_sample, aes(x = long, y = lat, alpha = size), 
             na.rm = TRUE, size = 3, col = 'red') +
  theme_nothing(legend = TRUE) +
  labs(title = "centroids using sample data")

q2 <- ggmap(map_15) +
  geom_point(data = centroids_whole, aes(x = long, y = lat, alpha = size), 
             na.rm = TRUE, size = 3, col = 'red') +
  theme_nothing(legend = TRUE) +
  labs(title = "centroids using whole data")

library(gridExtra)
grid.arrange(q1, q2, ncol = 2)

## ----chap05chunk08-------------------------------------------------------
nclus <- 50
kmeans_nclus <- kmeans(xydata, centers = nclus, iter.max = 2000, nstart = 1)
sum(kmeans_nclus$withinss)

## ----chap05chunk09-------------------------------------------------------
nclus_seq <- seq(20, 1000, by = 50)

## ----chap05chunk10-------------------------------------------------------
find_wss <- function(nclus, ...) {
st <- Sys.time()
res <- sum(kmeans(centers = nclus, ...)$withinss)
print(sprintf("nclus = %d, runtime = %3.2f seconds", nclus, Sys.time() - st))
res
}

find_wss(nclus = 10, x = xydata, iter.max = 500, nstart = 1)

## ----chap05chunk11-------------------------------------------------------
wss <- sapply(nclus_seq, find_wss, x = xydata, iter.max = 500, nstart = 1)

library(ggplot2)
ggplot(aes(x = x, y = y), data = data.frame(x = nclus_seq, y = wss)) +
  geom_line() +
  xlab("number of clusters") +
  ylab("within clusters sum of squares")

