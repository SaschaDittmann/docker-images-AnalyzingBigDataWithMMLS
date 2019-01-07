## ----chap09chunk01, include=FALSE----------------------------------------
source('scripts/01-Getting-started.R')

## ---- eval=FALSE, engine="sql"-------------------------------------------
## -- let's enable external scripts so that SQL Server can make calls to the R server
## EXEC sp_configure  'external scripts enabled', 1
## Reconfigure  with  override

## ---- eval=FALSE, engine="sql"-------------------------------------------
## -- you need to restart SQL Server at this point then run this to double check
## EXEC sp_configure  'external scripts enabled'

## ---- eval=FALSE, engine="sql"-------------------------------------------
## USE master;
## GO
## CREATE DATABASE RDB;
## GO
## 
## USE [master]
## GO
## CREATE LOGIN [ruser] WITH PASSWORD=N'ruser', DEFAULT_DATABASE=[master], CHECK_EXPIRATION=OFF, CHECK_POLICY=OFF
## GO
## USE [RDB]
## GO
## CREATE USER [ruser] FOR LOGIN [ruser]
## ALTER ROLE [db_datareader] ADD MEMBER [ruser]
## ALTER ROLE [db_datawriter] ADD MEMBER [ruser]
## ALTER ROLE [db_ddladmin] ADD MEMBER [ruser]
## GO
## 
## USE RDB
## GO
## GRANT EXECUTE ANY EXTERNAL SCRIPT  TO [ruser]
## GO
## 
## use RDB
## GO
## GRANT EXECUTE TO [ruser]
## GO

## ---- eval=FALSE, engine="sql"-------------------------------------------
## -- here's a very basic example we can run to make sure everything worked
## EXEC sp_execute_external_script  @language =N'R',
## @script=N'OutputDataSet <- InputDataSet',
## @input_data_1 =N'select 1 as hello'
## with result sets (([hello] int not null));
## GO

## ---- eval=FALSE, engine="sql"-------------------------------------------
## -- this could be useful for debugging purposes
## EXEC sp_execute_external_script  @language =N'R',
## @script=N'print(.libPaths())
##           print(R.home())'
## GO

## ---- eval=FALSE, engine="sql"-------------------------------------------
## USE RDB
## GO
## 
## DROP TABLE NYCTaxiSmall;
## 
## CREATE TABLE NYCTaxiSmall(
##   pickup_datetime       char(19),
##   dropoff_datetime      char(19),
##   passenger_count       tinyint,
##   trip_distance         float(24),
##   pickup_longitude      float(24),
##   pickup_latitude       float(24),
##   rate_code_id          char(1),
##   dropoff_longitude     float(24),
##   dropoff_latitude      float(24),
##   payment_type          char(1),
##   fare_amount           float(24),
##   extra                 float(24),
##   mta_tax               float(24),
##   tip_amount            float(24),
##   tolls_amount          float(24),
##   improvement_surcharge float(24),
##   total_amount          float(24))
## GO
## 
## USE RDB
## GO
## 
## DECLARE @cnt INT = 1;
## WHILE @cnt < 7
## BEGIN
##     DECLARE @CSVfile nvarchar(255);
##     SET @CSVfile = N'C:\Data\NYC_taxi\yellow_tripsample_2016-0'+cast(@cnt as char(1))+'.csv';
##     PRINT @CSVfile;
##     DECLARE @q nvarchar(MAX);
##     SET @q=
##        'BULK INSERT NYCTaxiSmall
##         FROM '+char(39)+@CSVfile+char(39)+'
##         WITH (FIRSTROW = 2, FIELDTERMINATOR = '','', ROWTERMINATOR = ''\n'')'
##         -- WITH (FIRSTROW = 2, LASTROW = 1000000, FIELDTERMINATOR = '','', ROWTERMINATOR = ''\n'')'
##     EXEC(@q)
##     SET @cnt = @cnt + 1;
## END;
## GO

## ----chap09chunk02-------------------------------------------------------
sqlConnString <- "Driver=SQL Server;Server=.;Database=RDB;Uid=ruser;Pwd=ruser"
sqlRowsPerRead <- 100000
sqlTable <- "NYCTaxiSmall"

nyc_raw <- RxSqlServerData(connectionString = sqlConnString,
                           rowsPerRead = sqlRowsPerRead, 
                           table = sqlTable)

rxGetInfo(nyc_raw, getVarInfo = TRUE, numRows = 5)

## ----chap09chunk03-------------------------------------------------------
rate_levels <- c("standard", "JFK", "Newark", "Nassau or Westchester", "negotiated", "group ride")

ccColInfo <- list(
pickup_datetime    = list(type = "character"),
dropoff_datetime   = list(type = "character"),
passenger_count    = list(type = "integer"),
rate_code_id       = list(type = "factor", levels = as.character(1:6), newLevels = rate_levels),
store_and_fwd_flag = list(type = "factor", levels = c("Y", "N")),
payment_type       = list(type = "factor", levels = as.character(1:2), newLevels = c("card", "cash"))
)

nyc_raw <- RxSqlServerData(connectionString = sqlConnString,
                           rowsPerRead = sqlRowsPerRead, 
                           table = sqlTable,
                           colInfo = ccColInfo)

## ----chap09chunk04-------------------------------------------------------
rxGetInfo(nyc_raw, getVarInfo = TRUE, numRows = 5)

## ----chap09chunk05-------------------------------------------------------
system.time(
  rxsum_sql <- rxSummary( ~ fare_amount, nyc_raw, reportProgress = 0)
)

## ----chap09chunk06-------------------------------------------------------
# Set ComputeContext. Needs a temp directory path to serialize R objects back and forth
sqlShareDir <- paste("C:/AllShare/", Sys.getenv("USERNAME"), sep = "")
sqlWait <- TRUE
sqlConsoleOutput <- FALSE
sqlCC <- RxInSqlServer(connectionString = sqlConnString,
                       shareDir = sqlShareDir, 
                       wait = sqlWait, 
                       consoleOutput = sqlConsoleOutput)

## ----chap09chunk07-------------------------------------------------------
rxSetComputeContext(sqlCC)
system.time(
  rxsum_sql <- rxSummary( ~ fare_amount, nyc_raw)
)

## ----chap09chunk08-------------------------------------------------------
xforms <- function(data) {

  data$tip_percent <- ifelse(data$fare_amount > 0, data$tip_amount/data$fare_amount, NA)

  weekday_labels <- c('Sun', 'Mon', 'Tue', 'Wed', 'Thu', 'Fri', 'Sat')
  cut_levels <- c(1, 5, 9, 12, 16, 18, 22)
  hour_labels <- c('1AM-5AM', '5AM-9AM', '9AM-12PM', '12PM-4PM', '4PM-6PM', '6PM-10PM', '10PM-1AM')

  # extract pick-up hour and day of week
  pickup_datetime <- lubridate::ymd_hms(data$pickup_datetime, tz = "UTC")
  pickup_hour <- addNA(cut(hour(pickup_datetime), cut_levels))
  pickup_dow <- factor(wday(pickup_datetime), levels = 1:7, labels = weekday_labels)
  levels(pickup_hour) <- hour_labels
  # extract drop-off hour and day of week
  dropoff_datetime <- lubridate::ymd_hms(data$dropoff_datetime, tz = "UTC")
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

## ----chap09chunk09-------------------------------------------------------
library(rgeos)
library(maptools)

nyc_shapefile <- readShapePoly('../ZillowNeighborhoods-NY/ZillowNeighborhoods-NY.shp')
mht_shapefile <- subset(nyc_shapefile, City == 'New York' & County == 'New York')

manhattan_nhoods <- subset(nyc_shapefile@data, County == 'New York', select = "Name", drop = TRUE)
manhattan_nhoods <- as.character(manhattan_nhoods)
bad_nhoods <- c('Brooklyn Heights', 'Marble Hill', 'Mill Rock Park','Vinegar Hill')
bad_nhoods <- c(bad_nhoods, grep('Island', manhattan_nhoods, value = TRUE))
manhattan_nhoods <- setdiff(manhattan_nhoods, bad_nhoods)

## ---- eval=FALSE, engine="sql"-------------------------------------------
## CREATE TABLE NYCTaxiSmallFeaturized(
##   pickup_datetime       char(19),
##   dropoff_datetime      char(19),
##   passenger_count       tinyint,
##   trip_distance         float(24),
##   pickup_longitude      float(24),
##   pickup_latitude       float(24),
##   rate_code_id          varchar(24),
##   dropoff_longitude     float(24),
##   dropoff_latitude      float(24),
##   payment_type          char(5),
##   fare_amount           float(24),
##   extra                 float(24),
##   mta_tax               float(24),
##   tip_amount            float(24),
##   tolls_amount          float(24),
##   improvement_surcharge float(24),
##   total_amount          float(24),
##   tip_percent           float(24),
##   pickup_hour           char(8),
##   pickup_dow            char(3),
##   dropoff_hour          char(8),
##   dropoff_dow           char(3),
##   trip_duration         int,
##   pickup_nhood          varchar(30),
##   pickup_borough        varchar(30),
##   dropoff_nhood         varchar(30),
##   dropoff_borough       varchar(30),
##   pickup_nb             varchar(30),
##   dropoff_nb            varchar(30))
## GO

## ----chap09chunk10-------------------------------------------------------
nyc_sql <- RxSqlServerData(connectionString = sqlConnString,
                           rowsPerRead = sqlRowsPerRead, 
                           table = "NYCTaxiSmallFeaturized")

rxDataStep(nyc_raw, nyc_sql, transformFunc = xforms, overwrite = TRUE, 
           transformPackages = c("lubridate", "sp", "maptools"),
           transformObjects = list(nhoods_levels = manhattan_nhoods,
                                   shapefile = nyc_shapefile))

## ----chap09chunk11-------------------------------------------------------
weekday_labels <- c('Sun', 'Mon', 'Tue', 'Wed', 'Thu', 'Fri', 'Sat')
hour_labels <- c('1AM-5AM', '5AM-9AM', '9AM-12PM', '12PM-4PM', '4PM-6PM', '6PM-10PM', '10PM-1AM')

ccColInfo$pickup_dow <- list(type = "factor", levels = weekday_labels)
ccColInfo$pickup_hour <- list(type = "factor", levels = hour_labels)
ccColInfo$dropoff_dow <- list(type = "factor", levels = weekday_labels)
ccColInfo$dropoff_hour <- list(type = "factor", levels = hour_labels)

## ----chap09chunk12-------------------------------------------------------
library(rgeos)
library(maptools)

nyc_shapefile <- readShapePoly('../ZillowNeighborhoods-NY/ZillowNeighborhoods-NY.shp')
mht_shapefile <- subset(nyc_shapefile, City == 'New York' & County == 'New York')

manhattan_nhoods <- subset(nyc_shapefile@data, County == 'New York', select = "Name", drop = TRUE)
manhattan_nhoods <- as.character(manhattan_nhoods)
bad_nhoods <- c('Brooklyn Heights', 'Marble Hill', 'Ellis Island', 'Liberty Island', 'Mill Rock Park', 'Governors Island', 'Vinegar Hill')
manhattan_nhoods <- setdiff(manhattan_nhoods, bad_nhoods)

ccColInfo$pickup_nb <- list(type = "factor", levels = manhattan_nhoods)
ccColInfo$dropoff_nb <- list(type = "factor", levels = manhattan_nhoods)

## ----chap09chunk13-------------------------------------------------------
nyc_sql@colInfo <- ccColInfo
rxGetInfo(nyc_sql, getVarInfo = TRUE, numRows = 5)

## ----chap09chunk14-------------------------------------------------------
system.time(linmod <- rxLinMod(tip_percent ~ pickup_nb:dropoff_nb + pickup_dow:pickup_hour,
                               data = nyc_sql, reportProgress = 0, 
                               rowSelection = (split == "train")))

## ----chap09chunk15-------------------------------------------------------
sqlTable <- "NYCTaxiScore"
nyc_score <- RxSqlServerData(connectionString = sqlConnString,
                             rowsPerRead = sqlRowsPerRead, 
                             table = sqlTable)

rxPredict(linmod, data = nyc_sql, outData = nyc_score,
          predVarNames = "tip_percent_pred_linmod", overwrite = TRUE)

## ----chap09chunk16-------------------------------------------------------
nyc_score <- nyc_sql
nyc_score@table <- "NYCTaxiScore"

## ----chap09chunk17-------------------------------------------------------
rxPredict(linmod, data = nyc_sql, outData = nyc_score,
          predVarNames = "tip_percent_pred_linmod", overwrite = TRUE, 
          extraVarsToWrite = c("pickup_datetime", "dropoff_datetime"))

## ---- eval=FALSE, engine="SQL"-------------------------------------------
## CREATE TABLE models
## (model varbinary(max))
## GO
## 
## CREATE PROCEDURE [dbo].[PersistModel]
## @m nvarchar(max)
## AS
## BEGIN
## -- SET NOCOUNT ON added to prevent extra result sets from interfering with SELECT statements.
## SET NOCOUNT ON;
## insert into models (model) values (convert(varbinary(max),@m,2))
## END

## ----chap09chunk18-------------------------------------------------------
modelbin <- serialize(linmod, NULL)
modelbinstr <- paste(modelbin, collapse = "")

library(RODBC)
odbcCloseAll()
conn <- odbcDriverConnect(sqlConnString)
q <- paste("EXEC PersistModel @m='", modelbinstr,"'", sep = "")
sqlQuery(conn, q)

## ---- eval=FALSE, engine="sql"-------------------------------------------
## -- We can run this to show that we can successfully retrieve the model
## DECLARE @lmodel2 varbinary(max) = (SELECT TOP 1 model FROM RDB.dbo.models);
## EXEC sp_execute_external_script @language = N'R',
## @script = N'
##             mod <- unserialize(as.raw(model))
##             print(summary(mod))',
## @params = N'@model varbinary(max)',
## @model = @lmodel2;
## GO

## ---- eval=FALSE, engine="sql"-------------------------------------------
## -- Create prediction stored procedure
## CREATE PROCEDURE [dbo].[PredictTipBatchMode] @inquery nvarchar(max)
## AS
## BEGIN
## DECLARE @lmodel2 varbinary(max) = (SELECT TOP 1 model FROM models);
## EXEC sp_execute_external_script @language = N'R',
##   @script = N'
##               weekday_labels <- c("Sun", "Mon", "Tue", "Wed", "Thu", "Fri", "Sat")
##               hour_labels <- c("1AM-5AM", "5AM-9AM", "9AM-12PM", "12PM-4PM", "4PM-6PM", "6PM-10PM", "10PM-1AM")
## 
##               library(rgeos)
##               library(maptools)
## 
##               nyc_shapefile <- readShapePoly("../ZillowNeighborhoods-NY/ZillowNeighborhoods-NY.shp")
##               mht_shapefile <- subset(nyc_shapefile, City == "New York" & County == "New York")
## 
##               manhattan_nhoods <- subset(nyc_shapefile@data, County == "New York", select = "Name", drop = TRUE)
##               manhattan_nhoods <- as.character(manhattan_nhoods)
##               bad_nhoods <- c("Brooklyn Heights", "Marble Hill", "Ellis Island", "Liberty Island", "Mill Rock Park", "Governors Island", "Vinegar Hill")
##               manhattan_nhoods <- setdiff(manhattan_nhoods, bad_nhoods)
## 
##               mod <- unserialize(as.raw(model))
## 
##               InputDataSet <- transform(InputDataSet,
##                   pickup_dow = factor(pickup_dow, levels = weekday_labels),
##                   pickup_hour = factor(pickup_hour, levels = hour_labels),
##                   pickup_nb = factor(pickup_nb, levels = manhattan_nhoods),
##                   dropoff_nb = factor(dropoff_nb, levels = manhattan_nhoods))
## 
##               OutputDataSet <- rxPredict(modelObject = mod, data = InputDataSet,
##                                          outData = NULL, predVarNames = "Score",
##                                          type = "response", writeModelVars = FALSE,
##                                          overwrite = TRUE)
##               str(OutputDataSet)
##               print(OutputDataSet)',
##   @input_data_1 = @inquery,
##   @params = N'@model varbinary(max)',
##   @model = @lmodel2
## WITH RESULT SETS ((Score float));
## END

## ---- eval=FALSE, engine="sql"-------------------------------------------
## DECLARE @query_string nvarchar(max)
## SET @query_string='SELECT top 100 * FROM NYCTaxiSmall'
## EXEC [dbo].[PredictTipBatchMode] @inquery = @query_string;

## ----chap09chunk19-------------------------------------------------------
input <- "N' SELECT top 1000 * FROM NYCTaxiSmall'"
q <- paste("EXEC PredictTipBatchMode @inquery = ", input, sep = "")
scoredData <- sqlQuery(conn, q)
head(scoredData)

## ----chap09chunk20-------------------------------------------------------
qt <- data.frame(percentile = seq(1, 99, by = 1))
num_vars <- c('fare_amount', 'tip_percent')
qt[ , num_vars] <- lapply(num_vars, function(var) rxQuantile(var, nyc_sql, probs = qt$percentile / 100))
library(ggplot2)
q1 <- ggplot(aes(x = percentile, y = fare_amount), data = qt) + geom_line()
q2 <- ggplot(aes(x = percentile, y = tip_percent), data = qt) + geom_line()

library(gridExtra)
grid.arrange(q1, q2, ncol = 2)

## ----chap09chunk21-------------------------------------------------------
nyc_sample <- rxDataStep(nyc_sql, rowSelection = (u < .01),
                         transforms = list(u = runif(.rxNumRows)))

library(ggplot2)
ggplot(data = nyc_sample, aes(x = log(trip_distance), y = log(trip_duration))) + 
  geom_point()

## ----chap09chunk22-------------------------------------------------------
nyc_sample_sql <- nyc_sql
nyc_sample_sql@table <- NULL
nyc_sample_sql@sqlQuery <- 'select * from RDB.dbo.NYCTaxiSmall tablesample (1 percent)'
nyc_sample <- rxImport(nyc_sample_sql)

library(ggplot2)
ggplot(data = nyc_sample, aes(x = log(trip_distance), y = log(trip_duration))) + 
  geom_point()

## ----chap09chunk23-------------------------------------------------------
scatterPlot <- function(inDataSource) {
  ds <- rxImport(inDataSource)
  require(ggplot2)
  pl <- ggplot(data = ds, aes(x = log(trip_distance), y = log(trip_duration)))
  pl <- pl + geom_point()
  return(list(myplot = pl))
}

scatterPlot(nyc_sample_sql) # this works, but it's not in-database

## ----chap09chunk24-------------------------------------------------------
rxSetComputeContext(sqlCC)
myplots <- rxExec(scatterPlot, nyc_sample_sql, timesToRun = 1, packagesToLoad = 'ggplot2')
plot(myplots[[1]][["myplot"]]) # only the plot object is returned to us for display

## ---- eval=FALSE, engine="sql"-------------------------------------------
## USE RDB;
## GO
## CREATE TABLE plots(plot varbinary(max));
## GO
## 
## INSERT INTO plots(plot)
## EXEC  sp_execute_external_script
##    @language = N'R'
##   ,@script = N'
##         image_file = tempfile()
##         jpeg(filename = image_file, width = 500, height = 500)
##         hist(data$fare_amount, col = "light blue")
##         dev.off()
##         outds <- data.frame(data = readBin(file(image_file, "rb"), what = raw(), n = 1e6))'
##   ,@input_data_1 = N'select fare_amount from rdb.dbo.NYCTaxiSmall tablesample (1 percent);'
##   ,@input_data_1_name = N'data'
##   ,@output_data_1_name = N'outds';
## --WITH RESULT SETS ((plot varbinary(max)));

