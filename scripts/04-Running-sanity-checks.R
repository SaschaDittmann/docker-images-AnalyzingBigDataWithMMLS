## ----chap04chunk01, include=FALSE----------------------------------------
source('scripts/01-Getting-started.R')

## ----chap04chunk02-------------------------------------------------------
system.time(rxs_all <- rxSummary( ~ ., nyc_xdf) )

## ----chap04chunk03-------------------------------------------------------
head(rxs_all$sDataFrame)

## ----chap04chunk04-------------------------------------------------------
nhoods_by_borough <- rxCube( ~ pickup_nhood:pickup_borough, nyc_xdf, returnDataFrame = TRUE)
library(dplyr)
nhoods_by_borough %>%
  select(pickup_borough, pickup_nhood, Counts) %>%
  filter(Counts > 0) %>%
  arrange(pickup_borough, desc(Counts)) %>%
  group_by(pickup_borough) %>%
  top_n(5)

## ----chap04chunk05-------------------------------------------------------
nhoods_by_borough <- rxCrossTabs( ~ pickup_nhood:pickup_borough, nyc_xdf)
nhoods_by_borough <- nhoods_by_borough$counts[[1]]
nb_cnt <- apply(nhoods_by_borough, 1, function(x) sum(x > 0))
nb_cnt[nb_cnt > 1]

## ----chap04chunk06-------------------------------------------------------
manhattan_nhoods <- subset(nyc_shapefile@data, County == 'New York', select = "Name", drop = TRUE)
# manhattan_nhoods <- manhattan_nhoods[-grep('Island', manhattan_nhoods)]
manhattan_nhoods <- as.character(manhattan_nhoods)
bad_nhoods <- c('Brooklyn Heights', 'Marble Hill', 'Ellis Island', 'Liberty Island',
                'Mill Rock Park', 'Governors Island', 'Vinegar Hill')
manhattan_nhoods <- setdiff(manhattan_nhoods, bad_nhoods)

refactor_columns <- function(data) {
  data$pickup_nb = factor(data$pickup_nhood, levels = nhoods_levels)
  data$dropoff_nb = factor(data$dropoff_nhood, levels = nhoods_levels)
  data
}

rxDataStep(nyc_xdf, nyc_xdf, overwrite = TRUE, 
           transformFunc = refactor_columns, 
           transformObjects = list(nhoods_levels = manhattan_nhoods))

rxs_pickdrop <- rxSummary( ~ pickup_nb:dropoff_nb, nyc_xdf)
head(rxs_pickdrop$categorical[[1]])

## ----chap04chunk07-------------------------------------------------------
rxHistogram( ~ trip_distance, nyc_xdf, startVal = 0, endVal = 25, 
            histType = "Percent", numBreaks = 20)

## ----chap04chunk08-------------------------------------------------------
rxs <- rxSummary( ~ pickup_nhood:dropoff_nhood, nyc_xdf, 
                 rowSelection = (trip_distance > 15 & trip_distance < 22))
library(dplyr)
head(arrange(rxs$categorical[[1]], desc(Counts)), 10)

## ----chap04chunk09-------------------------------------------------------
# outFile argument missing means we output to data.frame
odd_trips <- rxDataStep(nyc_xdf, 
  rowSelection = (u < .05 & ( # we can adjust this if the data gets too big
                  (trip_distance > 20 | trip_distance <= 0) |
                  (passenger_count > 5 | passenger_count == 0) |
                  (fare_amount > 1000 | fare_amount <= 0))), 
  transforms = list(u = runif(.rxNumRows)))

print(dim(odd_trips))

## ----chap04chunk10-------------------------------------------------------
library(ggplot2)
odd_trips %>%
filter(trip_distance > 20) %>%
ggplot() -> p

p + geom_histogram(aes(x = fare_amount, fill = trip_duration <= 10*60), binwidth = 10) +
  xlim(0, 500) + 
  coord_fixed(ratio = .5)

## ----chap04chunk11-------------------------------------------------------
rxHistogram( ~ trip_distance, nyc_xdf, startVal = 0, endVal = 25, 
            histType = "Percent", numBreaks = 20)

## ----chap04chunk12-------------------------------------------------------
cut(8.9, breaks = c(-Inf, 0, 5, 10, Inf), labels = c("0", "<5", "5-10", "10+"))

## ----chap04chunk13-------------------------------------------------------
rxHistogram( ~ trip_distance | pickup_hour + payment_type, nyc_xdf, startVal = 0, 
            endVal = 25, numBreaks = 20)

## ----chap04chunk14-------------------------------------------------------
rxHistogram( ~ trip_dist | pickup_hour + payment_type, nyc_xdf, 
            transforms = list(trip_dist = cut(trip_distance, 
                                              breaks = c(-Inf, 0, 5, 10, Inf), 
                                              labels = c("0", "<5", "5-10", "10+"))))

