## ----chap02chunk01, include=FALSE----------------------------------------
source('scripts/01-Getting-started.R')

## ----chap02chunk02-------------------------------------------------------
col_classes <- c(
  'pickup_datetime'       = "character",
  'dropoff_datetime'      = "character",
  'passenger_count'       = "integer",
  'trip_distance'         = "numeric",
  'pickup_longitude'      = "numeric",
  'pickup_latitude'       = "numeric",
  'rate_code_id'          = "factor",
  'dropoff_longitude'     = "numeric",
  'dropoff_latitude'      = "numeric",
  'payment_type'          = "factor",
  'fare_amount'           = "numeric",
  'extra'                 = "numeric",
  'mta_tax'               = "numeric",
  'tip_amount'            = "numeric",
  'tolls_amount'          = "numeric",
  'improvement_surcharge' = "numeric",
  'total_amount'          = "numeric")

## ----chap02chunk03-------------------------------------------------------
data_dir <- "data/NYC_taxi"
input_csv <- file.path(data_dir, 'yellow_tripsample_2016-01.csv')
# we take a chunk of the data and load it as a data.frame (good for testing things)
nyc_sample <- read.csv(input_csv, nrows = 1000, colClasses = col_classes)
head(nyc_sample)

## ----chap02chunk04-------------------------------------------------------
input_xdf <- file.path(data_dir, 'yellow_tripsample_2016.xdf')
library(lubridate)
most_recent_date <- ymd("2016-07-01") # the day of the months is irrelevant

# because we keep appending to the same file, we can't run this in parallel
st <- Sys.time()
for(ii in 1:6) { # get each month's data and append it to the first month's data
  file_date <- most_recent_date - months(ii)
  input_csv <- sprintf('yellow_tripsample_%s.csv', substr(file_date, 1, 7))
  input_csv <- file.path(data_dir, input_csv)
  append <- if (ii == 1) "none" else "rows"
  rxImport(input_csv, input_xdf, colClasses = col_classes, overwrite = TRUE, 
    append = append)
  print(input_csv)
}
Sys.time() - st # stores the time it took to import

## ----chap02chunk05-------------------------------------------------------
input_xdf <- file.path(data_dir, 'yellow_tripsample_2016.xdf')
nyc_xdf <- RxXdfData(input_xdf)
system.time(rxsum_xdf <- rxSummary( ~ fare_amount, nyc_xdf))
rxsum_xdf

## ----chap02chunk06-------------------------------------------------------
# we can only use one month's data unless we join the CSVs
input_csv <- file.path(data_dir, 'yellow_tripsample_2016-01.csv')
# point to CSV file and provide column info
nyc_csv <- RxTextData(input_csv, colClasses = col_classes)
system.time(rxsum_csv <- rxSummary( ~ fare_amount, nyc_csv))
rxsum_csv

## ----chap02chunk07, eval=FALSE-------------------------------------------
## input_xdf <- file.path(data_dir, 'yellow_tripsample_2016-01.xdf')
## input_csv <- file.path(data_dir, 'yellow_tripsample_2016-01.csv')
## 
## st <- Sys.time()
## ## convert CSV to XDF here
## jan_2016_xdf <- RxXdfData(input_xdf)
## ## summarize XDF file here
## rt_xdf <- Sys.time() - st
## 
## st <- Sys.time()
## jan_2016_csv <- RxTextData(input_csv, colClasses = col_classes)
## ## summarize CSV file here
## rt_csv <- Sys.time() - st
## 
## file.remove(input_xdf) # remove the file to keep folder clean
## 
## ## compare runtimes rt_xdf and rt_csv
## ## compare results sum_xdf and sum_csv

## ----chap02chunk08-------------------------------------------------------
input_xdf <- file.path(data_dir, 'yellow_tripsample_2016-01.xdf')
input_csv <- file.path(data_dir, 'yellow_tripsample_2016-01.csv')

st <- Sys.time()
rxImport(input_csv, input_xdf, colClasses = col_classes, overwrite = TRUE)
jan_2016_xdf <- RxXdfData(input_xdf)
sum_xdf <- rxSummary( ~ ., jan_2016_xdf)
rt_xdf <- Sys.time() - st # runtime for XDF file

file.remove(input_xdf) # remove the file to keep folder clean

## ----chap02chunk09-------------------------------------------------------
st <- Sys.time()
jan_2016_csv <- RxTextData(input_csv, colClasses = col_classes)
sum_csv <- rxSummary( ~ ., jan_2016_csv)
rt_csv <- Sys.time() - st # runtime for CSV file

## ----chap02chunk10-------------------------------------------------------
rt_xdf - rt_csv

## ----chap02chunk11-------------------------------------------------------
sum_xdf$categorical[[2]]
sum_csv$categorical[[2]]

## ----chap02chunk12-------------------------------------------------------
sum_xdf$sDataFrame[5, ]
sum_csv$sDataFrame[5, ]
