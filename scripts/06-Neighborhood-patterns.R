## ----chap06chunk01, include=FALSE----------------------------------------
source('scripts/01-Getting-started.R')

## ----chap06chunk02-------------------------------------------------------
rxct <- rxCrossTabs(trip_distance ~ pickup_nb:dropoff_nb, mht_xdf)
res <- rxct$sums$trip_distance / rxct$counts$trip_distance

library(seriation)
res[which(is.nan(res))] <- mean(res, na.rm = TRUE)
nb_order <- seriate(res)

## ----chap06chunk03-------------------------------------------------------
rxc1 <- rxCube(trip_distance ~ pickup_nb:dropoff_nb, mht_xdf)
rxc2 <- rxCube(minutes_per_mile ~ pickup_nb:dropoff_nb, mht_xdf, 
               transforms = list(minutes_per_mile = (trip_duration/60)/trip_distance))
rxc3 <- rxCube(tip_percent ~ pickup_nb:dropoff_nb, mht_xdf)
library(dplyr)
res <- bind_cols(list(rxc1, rxc2, rxc3))
res <- res[ , c('pickup_nb', 'dropoff_nb', 'trip_distance', 'minutes_per_mile', 'tip_percent')]
head(res)

## ----chap06chunk04-------------------------------------------------------
library(ggplot2)
ggplot(res, aes(pickup_nb, dropoff_nb)) +
  geom_tile(aes(fill = trip_distance), colour = "white") +
  theme(axis.text.x = element_text(angle = 60, hjust = 1)) +
  scale_fill_gradient(low = "white", high = "steelblue") +
  coord_fixed(ratio = .9)

## ----chap06chunk05-------------------------------------------------------
newlevs <- levels(res$pickup_nb)[unlist(nb_order)]
res$pickup_nb <- factor(res$pickup_nb, levels = unique(newlevs))
res$dropoff_nb <- factor(res$dropoff_nb, levels = unique(newlevs))

ggplot(res, aes(pickup_nb, dropoff_nb)) +
  geom_tile(aes(fill = trip_distance), colour = "white") +
  theme(axis.text.x = element_text(angle = 60, hjust = 1)) +
  scale_fill_gradient(low = "white", high = "steelblue") +
  coord_fixed(ratio = .9)

## ----chap06chunk06-------------------------------------------------------
ggplot(res, aes(pickup_nb, dropoff_nb)) +
  geom_tile(aes(fill = minutes_per_mile), colour = "white") +
  theme(axis.text.x = element_text(angle = 60, hjust = 1)) +
  scale_fill_gradient(low = "white", high = "steelblue") +
  coord_fixed(ratio = .9)

## ----chap06chunk07-------------------------------------------------------
res %>%
  mutate(tip_color = cut(tip_percent, c(0, 8, 12, 15, 100))) %>%
  ggplot(aes(pickup_nb, dropoff_nb)) +
  geom_tile(aes(fill = tip_color)) +
  theme(axis.text.x = element_text(angle = 60, hjust = 1)) +
  coord_fixed(ratio = .9)

## ----chap06chunk08-------------------------------------------------------
# first way of reordering the factor levels
rxDataStep(inData = mht_xdf, outFile = mht_xdf, 
	transforms = list(
		pickup_nb = factor(pickup_nb, levels = newlevels), 
		dropoff_nb = factor(dropoff_nb, levels = newlevels)), 
	transformObjects = list(newlevels = unique(newlevs)), overwrite = TRUE)

## ----chap06chunk09, eval=FALSE-------------------------------------------
## # second way of reordering the factor levels
## rxFactors(mht_xdf, outFile = mht_xdf,
## 	factorInfo = list(
## 		pickup_nb = list(newLevels = unique(newlevs)),
## 		dropoff_nb = list(newLevels = unique(newlevs))),
## 	overwrite = TRUE)

## ----chap06chunk10-------------------------------------------------------
rxc <- rxCube( ~ pickup_nb:dropoff_nb, mht_xdf)
rxc <- as.data.frame(rxc)

rxc %>%
  filter(Counts > 0) %>%
  mutate(pct_all = Counts/sum(Counts) * 100) %>%
  group_by(pickup_nb) %>%
  mutate(pct_by_pickup_nb = Counts/sum(Counts) * 100) %>%
  group_by(dropoff_nb) %>%
  mutate(pct_by_dropoff_nb = Counts/sum(Counts) * 100) %>%
  group_by() %>%
  arrange(desc(Counts)) -> rxcs

head(rxcs)

## ----chap06chunk11-------------------------------------------------------
ggplot(rxcs, aes(pickup_nb, dropoff_nb)) +
  geom_tile(aes(fill = pct_all), colour = "white") +
  theme(axis.text.x = element_text(angle = 60, hjust = 1)) +
  scale_fill_gradient(low = "white", high = "black") +
  coord_fixed(ratio = .9)

## ----chap06chunk12-------------------------------------------------------
ggplot(rxcs, aes(pickup_nb, dropoff_nb)) +
  geom_tile(aes(fill = pct_by_pickup_nb), colour = "white") +
  theme(axis.text.x = element_text(angle = 60, hjust = 1)) +
  scale_fill_gradient(low = "white", high = "steelblue") +
  coord_fixed(ratio = .9)

## ----chap06chunk13-------------------------------------------------------
ggplot(rxcs, aes(pickup_nb, dropoff_nb)) +
  geom_tile(aes(fill = pct_by_dropoff_nb), colour = "white") +
  theme(axis.text.x = element_text(angle = 60, hjust = 1)) +
  scale_fill_gradient(low = "white", high = "red") +
  coord_fixed(ratio = .9)

## ----chap06chunk14-------------------------------------------------------
rxcs %>%
  select(pickup_nb, dropoff_nb, pct = pct_by_pickup_nb) %>%
  arrange(pickup_nb, desc(pct))

## ----chap06chunk15, eval=FALSE-------------------------------------------
## nb_name <- "West Village" # a neighborhood of our choosing
## nb_drop <- ## pull the most common destinations for this neighborhood from `rxcs_tops`
## 
## pickup_df <- rxDataStep(mht_xdf, # we leave out outFile and store results in pickup_df
##   rowSelection = ## select the relevant subset of the data
##   varsToKeep = c("dropoff_nb", "pickup_datetime"),
##   transformObjects = ## a list, used to pass `nb_name` and `nb_drop` to rowSelection
##   )

## ----chap06chunk16, eval=FALSE-------------------------------------------
## library(lubridate)
## pickup_df %>%
##   mutate(pickup_hour = hour(ymd_hms(pickup_datetime, tz = "UTC"))) %>%
##   ggplot(aes(x = pickup_hour, fill = dropoff_nb)) +
##   geom_bar(position = "stack", stat = "count") +
##   scale_fill_discrete(guide = guide_legend(reverse = TRUE))

## ----chap06chunk17-------------------------------------------------------
rxcs %>%
  select(pickup_nb, dropoff_nb, pct = pct_by_pickup_nb) %>%
  arrange(pickup_nb, desc(pct)) %>%
  group_by(pickup_nb) %>%
  mutate(cumpct = cumsum(pct)) %>%
  filter(pct > 5 & (cumpct <= 50 | (cumpct > 50 & lag(cumpct) <= 50))) %>%
  as.data.frame -> rxcs_tops

## ----chap06chunk18-------------------------------------------------------
nb_name <- "West Village"
nb_drop <- subset(rxcs_tops, pickup_nb == nb_name, select = "dropoff_nb", drop = TRUE)

pickup_df <- rxDataStep(mht_xdf,
  rowSelection = pickup_nb == nb & dropoff_nb %in% top_drop_for_nb,
  varsToKeep = c("dropoff_nb", "pickup_datetime"),
  transformObjects = list(nb = nb_name, top_drop_for_nb = nb_drop))

## ----chap06chunk19-------------------------------------------------------
library(scales)
library(lubridate)
pickup_df %>%
  mutate(pickup_hour = hour(ymd_hms(pickup_datetime, tz = "UTC"))) %>%
  ggplot(aes(x = pickup_hour, fill = dropoff_nb)) +
  geom_bar(position = "fill", stat = "count") +
  scale_fill_discrete(guide = guide_legend(reverse = TRUE)) +
  scale_y_continuous(labels = percent_format()) +
  ylab("percent")

## ----chap06chunk20-------------------------------------------------------
res1 <- rxCube(tip_percent ~ pickup_dow:pickup_hour, mht_xdf)
res2 <- rxCube(fare_amount/(trip_duration/60) ~ pickup_dow:pickup_hour, mht_xdf)
names(res2)[3] <- 'fare_per_minute'
res <- bind_cols(list(res1, res2))
res <- res[ , c('pickup_dow', 'pickup_hour', 'fare_per_minute', 'tip_percent', 'Counts')]

ggplot(res, aes(pickup_dow, pickup_hour)) +
  geom_tile(aes(fill = fare_per_minute), colour = "white") +
  theme(axis.text.x = element_text(angle = 60, hjust = 1)) +
  scale_fill_gradient(low = "white", high = "steelblue") +
  geom_text(aes(label = sprintf('%dK riders\n (%d%% tip)', 
                                signif(Counts/1000, 2), round(tip_percent, 0))), 
            size = 2.5) +
  coord_fixed(ratio = .9)

