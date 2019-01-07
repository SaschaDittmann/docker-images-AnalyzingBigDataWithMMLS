## ----chap10chunk01, include=FALSE----------------------------------------
source('scripts/01-Getting-started.R')

## ---- eval=FALSE, engine="bash"------------------------------------------
## wget http://mrsactionscripts.blob.core.windows.net/rstudio-server-community-v01/testhdi.r

## ---- eval=FALSE, engine="bash"------------------------------------------
## mkdir data
## cat raw_urls.txt | xargs -n 1 -p 6 wget -c -P data/

## ---- eval=FALSE, engine="bash"------------------------------------------
## hadoop fs -mkdir /user/RevoShare/sethmott/nyctaxi
## hadoop fs -copyFromLocal data/* /user/RevoShare/sethmott/nyctaxi
## hadoop fs -ls /user/RevoShare/sethmott/nyctaxi

## ---- eval=FALSE, engine="bash"------------------------------------------
## wget http://www.zillow.com/static/shp/ZillowNeighborhoods-NY.zip
## unzip ZillowNeighborhoods-NY.zip -d ZillowNeighborhoods-NY/

## ---- eval=FALSE, engine="bash"------------------------------------------
## sudo apt-get install libgeos-dev -y -f

## ---- eval=FALSE, engine="bash"------------------------------------------
## sudo R

## ----chap10chunk02, eval=FALSE-------------------------------------------
## .libPaths("/usr/lib64/microsoft-r/3.3/lib64/R/library")

## ----chap10chunk03, eval=FALSE-------------------------------------------
## options("repos" = c(CRAN = "http://cran.r-project.org/"))
## install.packages('dplyr')
## install.packages('lubridate')
## install.packages('stringr')
## install.packages('tidyr')
## install.packages('rgeos')
## install.packages('maptools')
## install.packages('ggplot2')
## install.packages('ggrepel')
## install.packages('ggmap')
## install.packages('gridExtra')
## install.packages('seriation')
## install.packages('circlize')

## ---- eval=FALSE, engine="bash"------------------------------------------
## apt-get install libgeos-dev -y -f

## ----chap10chunk04, eval=FALSE-------------------------------------------
## install.packages('lubridate')
## install.packages('stringr')
## install.packages('rgeos')
## install.packages('maptools')

## ----chap10chunk05-------------------------------------------------------
myNameNode <- "default"
myPort <- 0
hdfsFS <- RxHdfsFileSystem(hostName = myNameNode, port = myPort)

data_path <- file.path("/user/RevoShare/sparkuser")
taxi_path <- file.path(data_path, "nyctaxi")

payment_levels <- c("card", "cash", "no charge", "dispute", "unknown", "voided trip")
ccColInfo <- list(payment_type = list(type = "factor", 
                                      levels = as.character(1:6),
                                      newLevels = payment_levels))

taxi_text <- RxTextData(taxi_path, colInfo = ccColInfo, fileSystem = hdfsFS)
rxGetInfo(taxi_text, getVarInfo = TRUE, numRows = 10)

## ----chap10chunk06-------------------------------------------------------
spark_cc <- RxSpark(nameNode = myNameNode,
                    port = myPort,
                    persistentRun = TRUE, 
                    executorOverheadMem = "4G", 
                    executorMem = "16G", 
                    executorCores = 4,
                    extraSparkConfig = "--conf spark.speculation=true",
                    consoleOutput = TRUE)

rxSetComputeContext(spark_cc)

## ----chap10chunk07-------------------------------------------------------
system.time(
  rxsum <- rxSummary( ~ fare_amount + payment_type, taxi_text)
)

rxsum

## ----chap10chunk08-------------------------------------------------------
taxi_old <- taxi_text
taxi_new <- RxXdfData(file.path(data_path, "nyctaxiXDF01"), fileSystem = hdfsFS)
rxHadoopRemoveDir(taxi_new@file)

## ----chap10chunk09-------------------------------------------------------
rxDataStep(taxi_old, taxi_new,
           transforms = list(tip_percent = ifelse(fare_amount > 0, 
                                                  tip_amount/fare_amount,
                                                  NA)))

## ----chap10chunk10-------------------------------------------------------
system.time(
  rxs1 <- rxSummary( ~ tip_percent, taxi_new)
)

## ----chap10chunk11-------------------------------------------------------
system.time(
  rxs2 <- rxSummary( ~ tip_percent, taxi_old,
                    transforms = list(tip_percent = ifelse(fare_amount > 0, 
                                                           tip_amount/fare_amount,
                                                           NA)))
)

## ----chap10chunk12-------------------------------------------------------
xforms <- function(data) {

  data$tip_percent <- ifelse(data$fare_amount > 0, data$tip_amount/data$fare_amount, NA)

  weekday_labels <- c('Sun', 'Mon', 'Tue', 'Wed', 'Thu', 'Fri', 'Sat')
  cut_levels <- c(1, 5, 9, 12, 16, 18, 22)
  hour_labels <- c('1AM-5AM', '5AM-9AM', '9AM-12PM', '12PM-4PM', '4PM-6PM', '6PM-10PM', '10PM-1AM')

  # extract pick-up hour and day of week
  pickup_datetime <- lubridate::ymd_hms(data$tpep_pickup_datetime, tz = "UTC")
  pickup_hour <- addNA(cut(hour(pickup_datetime), cut_levels))
  pickup_dow <- factor(wday(pickup_datetime), levels = 1:7, labels = weekday_labels)
  levels(pickup_hour) <- hour_labels
  # extract drop-off hour and day of week
  dropoff_datetime <- lubridate::ymd_hms(data$tpep_dropoff_datetime, tz = "UTC")
  dropoff_hour <- addNA(cut(hour(dropoff_datetime), cut_levels))
  dropoff_dow <- factor(wday(dropoff_datetime), levels = 1:7, labels = weekday_labels)
  levels(dropoff_hour) <- hour_labels
  data$pickup_hour <- pickup_hour
  data$pickup_dow <- pickup_dow
  data$dropoff_hour <- dropoff_hour
  data$dropoff_dow <- dropoff_dow
  # extract trip duration
  data$trip_duration <- as.integer(lubridate::interval(pickup_datetime, dropoff_datetime))
  
  # extract pick-up lat and long and find their neighborhoods
  pickup_longitude <- ifelse(is.na(data$pickup_longitude), 0, data$pickup_longitude)
  pickup_latitude <- ifelse(is.na(data$pickup_latitude), 0, data$pickup_latitude)
  data_coords <- data.frame(long = pickup_longitude, lat = pickup_latitude)
  coordinates(data_coords) <- c('long', 'lat')
  nhoods <- over(data_coords, shapefile)
  # add only the pick-up neighborhood and borough columns to the data
  data$pickup_nhood <- nhoods$Name
  data$pickup_borough <- nhoods$County

  # extract drop-off lat and long and find their neighborhoods
  dropoff_longitude <- ifelse(is.na(data$dropoff_longitude), 0, data$dropoff_longitude)
  dropoff_latitude <- ifelse(is.na(data$dropoff_latitude), 0, data$dropoff_latitude)
  data_coords <- data.frame(long = dropoff_longitude, lat = dropoff_latitude)
  coordinates(data_coords) <- c('long', 'lat')
  nhoods <- over(data_coords, shapefile)
  # add only the drop-off neighborhood and borough columns to the data
  data$dropoff_nhood <- nhoods$Name
  data$dropoff_borough <- nhoods$County

  # reduce pick-up and drop-off neighborhoods to manhattan only
  data$pickup_nb = factor(data$pickup_nhood, levels = nhoods_levels)
  data$dropoff_nb = factor(data$dropoff_nhood, levels = nhoods_levels)

  return(data)
}

## ----chap10chunk13-------------------------------------------------------
library(rgeos)
library(maptools)

nyc_shapefile <- readShapePoly('ZillowNeighborhoods-NY/ZillowNeighborhoods-NY.shp')
mht_shapefile <- subset(nyc_shapefile, City == 'New York' & County == 'New York')

manhattan_nhoods <- subset(nyc_shapefile@data, County == 'New York', select = "Name", drop = TRUE)
manhattan_nhoods <- as.character(manhattan_nhoods)
bad_nhoods <- c('Brooklyn Heights', 'Marble Hill', 'Mill Rock Park','Vinegar Hill')
bad_nhoods <- c(bad_nhoods, grep('Island', manhattan_nhoods, value = TRUE))
manhattan_nhoods <- setdiff(manhattan_nhoods, bad_nhoods)

## ----chap10chunk14-------------------------------------------------------
x <- head(taxi_new)
rxSetComputeContext("local")

rxDataStep(inData = x, 
           outFile = NULL, 
           transformFunc = xforms, 
           transformPackages = c("lubridate", "sp", "maptools"),
           transformObjects = list(nhoods_levels = manhattan_nhoods,
                                   shapefile = nyc_shapefile))

## ----chap10chunk15-------------------------------------------------------
rxSetComputeContext(spark_cc)

taxi_old <- taxi_text
taxi_new <- RxXdfData(file.path(data_path, "nyctaxiXDF01"), fileSystem = hdfsFS)
rxHadoopRemoveDir(taxi_new@file)

system.time(
  rxDataStep(inData = taxi_old, 
             outFile = taxi_new, 
             transformFunc = xforms, 
             transformPackages = c("lubridate", "sp", "maptools"),
             transformObjects = list(nhoods_levels = manhattan_nhoods,
                                     shapefile = nyc_shapefile))
)

## ----chap10chunk16-------------------------------------------------------
rxGetInfo(taxi_new, numRows = 5, getVarInfo = TRUE)

## ----chap10chunk17-------------------------------------------------------
rxs1 <- rxSummary( ~ pickup_hour + pickup_dow + trip_duration, taxi_new)
# we can add a column for proportions next to the counts
rxs1$categorical <- lapply(rxs1$categorical, 
                           function(x) cbind(x, prop =round(prop.table(x$Counts), 2)))
rxs1

## ----chap10chunk18-------------------------------------------------------
rxs2 <- rxSummary( ~ pickup_dow:pickup_hour, taxi_new)
rxs2 <- tidyr::spread(rxs2$categorical[[1]], key = 'pickup_hour', value = 'Counts')
row.names(rxs2) <- rxs2[ , 1]
rxs2 <- as.matrix(rxs2[ , -1])
rxs2

## ----chap10chunk19-------------------------------------------------------
levelplot(prop.table(rxs2, 2), cuts = 10, xlab = "", ylab = "", 
          main = "Distribution of taxis by day of week")

## ----chap10chunk20-------------------------------------------------------
system.time(
  rxs_all <- rxSummary( ~ ., taxi_new)
)
rxs_all$sDataFrame

## ----chap10chunk21-------------------------------------------------------
nhoods_by_borough <- rxCube( ~ pickup_nhood:pickup_borough, taxi_new, returnDataFrame = TRUE)
library(dplyr)
nhoods_by_borough %>%
  select(pickup_borough, pickup_nhood, Counts) %>%
  filter(Counts > 0) %>%
  arrange(pickup_borough, desc(Counts)) %>%
  group_by(pickup_borough) %>%
  top_n(10) %>%
  print(n = 50)

## ----chap10chunk22-------------------------------------------------------
system.time(
  rxHistogram( ~ trip_distance, taxi_new,
               startVal = 0, endVal = 25, histType = "Percent", numBreaks = 20)
)

## ----chap10chunk23-------------------------------------------------------
system.time(
  rxs <- rxSummary( ~ pickup_nhood:dropoff_nhood, taxi_new, 
                    rowSelection = (trip_distance > 15 & trip_distance < 22))
)

rxs <- rxs$categorical[[1]]
head(arrange(rxs, desc(Counts)), 10)

## ----chap10chunk24-------------------------------------------------------
system.time(
  rxs <- rxSummary( ~ pickup_nb:dropoff_nb, taxi_new, 
                    rowSelection = (trip_distance > 15 & trip_distance < 22))
)

rxs <- rxs$categorical[[1]]
head(arrange(rxs, desc(Counts)), 10)

## ----chap10chunk25-------------------------------------------------------
newlevs <- c("Financial District", "Battery Park", "Tribeca", "Chinatown",
             "Lower East Side", "Little Italy", "SoHo", "West Village", 
             "Greenwich Village", "NoHo", "Stuyvesant Town", "East Village", 
             "Gramercy", "Flatiron District", "Chelsea", "Clinton", 
             "Garment District", "Murray Hill", "Tudor City", "Turtle Bay", 
             "Sutton Place", "Midtown", "Columbus Circle", "Upper East Side", 
             "Central Park", "Carnegie Hill", "Upper West Side", "East Harlem", 
             "Harlem", "Morningside Heights", "Manhattanville", 
             "Hamilton Heights", "Washington Heights", "Inwood")

## ----chap10chunk26-------------------------------------------------------
taxi_old <- RxXdfData(file.path(data_path, "nyctaxiXDF01"), fileSystem = hdfsFS)
taxi_new <- RxXdfData(file.path(data_path, "nyctaxiXDF02"), fileSystem = hdfsFS)
rxHadoopRemoveDir(taxi_new@file)

st <- Sys.time()
rxDataStep(taxi_old, taxi_new,
           transforms = list(pickup_nb = factor(pickup_nb, levels = newlevels),
                             dropoff_nb = factor(dropoff_nb, levels = newlevels),
                             split = factor(ifelse(rbinom(.rxNumRows, size = 1, prob = 0.75), "train", "test")),
                             good_tip = as.factor(ifelse(tip_percent > 0.3, 1, 0))),
           transformObjects = list(newlevels = unique(newlevs)),
           rowSelection = (
             passenger_count > 0 & 
             payment_type %in% c("card", "cash") & 
             trip_distance >= 0 & trip_distance < 30 & 
             trip_duration > 0 & trip_duration < 60*60*24 & 
             !is.na(pickup_nb) & 
             !is.na(dropoff_nb) & 
             fare_amount > 0), 
           varsToDrop = c('extra', 'mta_tax', 'improvement_surcharge', 'total_amount', 
                          'pickup_borough', 'dropoff_borough', 
                          'pickup_nhood', 'dropoff_nhood'))

Sys.time() - st

## ----chap10chunk27-------------------------------------------------------
rxc1 <- rxCube(trip_distance ~ pickup_nb:dropoff_nb, taxi_new)

rxc2 <- rxCube(minutes_per_mile ~ pickup_nb:dropoff_nb, taxi_new, 
               transforms = list(minutes_per_mile = (trip_duration / 60) / trip_distance))

rxc3 <- rxCube(tip_percent ~ pickup_nb:dropoff_nb, taxi_new,
               rowSelection = (payment_type == "card"))

library(dplyr)
res <- bind_cols(list(rxc1, rxc2, rxc3))
res <- res[, c('pickup_nb', 'dropoff_nb', 
               'trip_distance', 'minutes_per_mile', 'tip_percent')]
head(res)

## ----chap10chunk28-------------------------------------------------------
library(ggplot2)
ggplot(res, aes(pickup_nb, dropoff_nb)) +
  geom_tile(aes(fill = trip_distance), colour = "white") +
  theme(axis.text.x = element_text(angle = 60, hjust = 1)) +
  scale_fill_gradient(low = "white", high = "steelblue") +
  coord_fixed(ratio = .9)

## ----chap10chunk29-------------------------------------------------------
ggplot(res, aes(pickup_nb, dropoff_nb)) +
  geom_tile(aes(fill = minutes_per_mile), colour = "white") +
  theme(axis.text.x = element_text(angle = 60, hjust = 1)) +
  scale_fill_gradient(low = "white", high = "steelblue") +
  coord_fixed(ratio = .9)

## ----chap10chunk30-------------------------------------------------------
ggplot(res, aes(pickup_nb, dropoff_nb)) +
  geom_tile(aes(fill = tip_percent), colour = "white") +
  theme(axis.text.x = element_text(angle = 60, hjust = 1)) +
  scale_fill_gradient(low = "white", high = "steelblue") +
  coord_fixed(ratio = .9)

## ----chap10chunk31-------------------------------------------------------
res %>%
  mutate(tip_color = cut(tip_percent, c(15, 20, 25, 100)/100)) %>%
  ggplot(aes(pickup_nb, dropoff_nb)) +
  geom_tile(aes(fill = tip_color)) +
  theme(axis.text.x = element_text(angle = 60, hjust = 1)) +
  coord_fixed(ratio = .9)

## ----chap10chunk32-------------------------------------------------------
res %>%
  mutate(tip_color = cut(tip_percent, quantile(tip_percent, na.rm = TRUE))) %>%
  # mutate(tip_color = ntile(tip_percent, 5)) %>%
  ggplot(aes(pickup_nb, dropoff_nb)) +
  geom_tile(aes(fill = tip_color)) +
  theme(axis.text.x = element_text(angle = 60, hjust = 1)) +
  coord_fixed(ratio = .9)

## ----chap10chunk33-------------------------------------------------------
rxSummary( ~ split + good_tip, taxi_new)

## ----chap10chunk34-------------------------------------------------------
list_models <- list(dtree = rxDTree, dforest = rxDForest, btrees = rxBTrees)

train_model <- function(model = rxDTree, xdf_data = taxi_new) {
  form <- formula(good_tip ~ payment_type + pickup_nb + dropoff_nb + pickup_hour + pickup_dow)
  rx_model <- model(form, data = xdf_data, 
                    rowSelection = (split == "train"),
                    method = "class")
  return(rx_model)  
}

## ----chap10chunk35-------------------------------------------------------
system.time(
  trained_models <- rxExec(train_model, model = rxElemArg(list_models), xdf_data = taxi_new)
)

## ----chap10chunk36-------------------------------------------------------
taxi_score <- RxXdfData(file.path(data_path, "nyctaxiXDF03"), fileSystem = hdfsFS)
rxHadoopRemoveDir(taxi_score@file)

rxPredict(trained_models$btree, data = taxi_new, outData = taxi_score,
          predVarNames = "good_tip_pred", 
          extraVarsToWrite = c("pickup_datetime", "dropoff_datetime"))

