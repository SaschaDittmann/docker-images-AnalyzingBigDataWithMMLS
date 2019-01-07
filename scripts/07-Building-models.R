## ----chap07chunk01, include=FALSE----------------------------------------
source('scripts/01-Getting-started.R')

## ----chap07chunk02-------------------------------------------------------
form_1 <- as.formula(tip_percent ~ pickup_nb:dropoff_nb + pickup_dow:pickup_hour)
rxlm_1 <- rxLinMod(form_1, data = mht_xdf, dropFirst = TRUE, covCoef = TRUE)

## ----chap07chunk03-------------------------------------------------------
rxs <- rxSummary( ~ pickup_nb + dropoff_nb + pickup_hour + pickup_dow, mht_xdf)
ll <- lapply(rxs$categorical, function(x) x[ , 1])
names(ll) <- c('pickup_nb', 'dropoff_nb', 'pickup_hour', 'pickup_dow')
pred_df_1 <- expand.grid(ll)
pred_df_1 <- rxPredict(rxlm_1, data = pred_df_1, computeStdErrors = TRUE, writeModelVars = TRUE)
names(pred_df_1)[1:2] <- paste(c('tip_pred', 'tip_stderr'), 1, sep = "_")
head(pred_df_1, 10)

## ----chap07chunk04-------------------------------------------------------
library(ggplot2)
ggplot(pred_df_1, aes(x = pickup_nb, y = dropoff_nb)) +
  geom_tile(aes(fill = tip_pred_1), colour = "white") +
  theme(axis.text.x = element_text(angle = 60, hjust = 1)) +
  scale_fill_gradient(low = "white", high = "red") +
  coord_fixed(ratio = .9)

## ----chap07chunk05-------------------------------------------------------
ggplot(pred_df_1, aes(x = pickup_dow, y = pickup_hour)) +
  geom_tile(aes(fill = tip_pred_1), colour = "white") +
  theme(axis.text.x = element_text(angle = 60, hjust = 1)) +
  scale_fill_gradient(low = "white", high = "red") +
  coord_fixed(ratio = .9)

## ----chap07chunk06-------------------------------------------------------
form_2 <- as.formula(tip_percent ~ pickup_nb:dropoff_nb)
rxlm_2 <- rxLinMod(form_2, data = mht_xdf, dropFirst = TRUE, covCoef = TRUE)
pred_df_2 <- rxPredict(rxlm_2, data = pred_df_1, computeStdErrors = TRUE, writeModelVars = TRUE)
names(pred_df_2)[1:2] <- paste(c('tip_pred', 'tip_stderr'), 2, sep = "_")

library(dplyr)
pred_df <- pred_df_2 %>%
  select(starts_with('tip_')) %>%
  cbind(pred_df_1) %>%
  arrange(pickup_nb, dropoff_nb, pickup_dow, pickup_hour) %>%
  select(pickup_dow, pickup_hour, pickup_nb, dropoff_nb, starts_with('tip_pred_'))

head(pred_df)

## ----chap07chunk07-------------------------------------------------------
ggplot(data = pred_df) +
  geom_density(aes(x = tip_pred_1, col = "complex")) +
  geom_density(aes(x = tip_pred_2, col = "simple")) +
  facet_grid(pickup_hour ~ pickup_dow)

## ----chap07chunk08-------------------------------------------------------
dfq <- data.frame(probs = seq(0, 1, by = .05))
dfq$tip_percent <- rxQuantile("tip_percent", data = mht_xdf, probs = dfq$probs)

ggplot(aes(x = tip_percent, y = probs), data = dfq) +
  geom_line()

## ----chap07chunk09-------------------------------------------------------
pred_df %>%
  mutate_each(funs(cut(., c(-Inf, 8, 12, 15, 18, Inf))), tip_pred_1, tip_pred_2) %>%
  ggplot() +
  geom_bar(aes(x = tip_pred_1, fill = "complex"), alpha = .5) +
  geom_bar(aes(x = tip_pred_2, fill = "simple"), alpha = .5) +
  facet_grid(pickup_hour ~ pickup_dow) +
  xlab('tip percent prediction') +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

## ----chap07chunk10, eval=FALSE-------------------------------------------
## form_3 <- ## formula described above goes here
## rxlm_3 <- ## build a linear model based on the above formula

## ----chap07chunk11, eval=FALSE-------------------------------------------
## rxs <- rxSummary( ~ payment_type + pickup_nb + dropoff_nb + pickup_hour + pickup_dow, mht_xdf)
## ll <- lapply(rxs$categorical, function(x) x[ , 1])
## names(ll) <- c('payment_type', 'pickup_nb', 'dropoff_nb', 'pickup_hour', 'pickup_dow')
## pred_df <- expand.grid(ll)

## ----chap07chunk12, eval=FALSE-------------------------------------------
## ggplot(data = pred_all) +
##   geom_bar(aes(x = p1, fill = "model 1", group = payment_type), alpha = .5) +
##   geom_bar(aes(x = p3, fill = "model 3", group = payment_type), alpha = .5) +
##   facet_grid(pickup_hour ~ pickup_dow) +
##   xlab('tip percent prediction') +
##   theme(axis.text.x = element_text(angle = 90, hjust = 1))

## ----chap07chunk13-------------------------------------------------------
form_3 <- as.formula(tip_percent ~ payment_type + pickup_nb:dropoff_nb + pickup_dow:pickup_hour)
rxlm_3 <- rxLinMod(form_3, data = mht_xdf, dropFirst = TRUE, covCoef = TRUE)

## ----chap07chunk14-------------------------------------------------------
rxs <- rxSummary( ~ payment_type + pickup_nb + dropoff_nb + pickup_hour + pickup_dow, mht_xdf)
ll <- lapply(rxs$categorical, function(x) x[ , 1])
names(ll) <- c('payment_type', 'pickup_nb', 'dropoff_nb', 'pickup_hour', 'pickup_dow')
pred_df <- expand.grid(ll)

pred_df_1 <- rxPredict(rxlm_1, data = pred_df, computeStdErrors = TRUE, writeModelVars = TRUE)
pred_df_3 <- rxPredict(rxlm_3, data = pred_df, computeStdErrors = TRUE, writeModelVars = TRUE)

pred_df %>%
  cbind(select(rename(pred_df_1, p1 = tip_percent_Pred), p1)) %>%
  cbind(select(rename(pred_df_3, p3 = tip_percent_Pred), p3)) %>%
  mutate_at(vars(p1, p3), funs(cut(., c(-Inf, 8, 12, 15, 18, Inf)))) -> pred_all

## ----chap07chunk15-------------------------------------------------------
ggplot(data = pred_all) +
  geom_bar(aes(x = p1, fill = "model 1"), alpha = .5) +
  geom_bar(aes(x = p3, fill = "model 3"), alpha = .5) +
  facet_grid(~ payment_type) +
  xlab('tip percent prediction') +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

## ----chap07chunk16-------------------------------------------------------
library(scales)
pred_df %>%
  cbind(select(rename(pred_df_1, p1 = tip_percent_Pred), p1)) %>%
  cbind(select(rename(pred_df_3, p3 = tip_percent_Pred), p3)) %>%
  mutate_at(vars(p1, p3), funs(rescale(., to = c(0, 20)))) %>%
  mutate_at(vars(p1, p3), funs(cut(., c(-Inf, 8, 12, 15, 18, Inf)))) -> pred_all

ggplot(data = pred_all) +
  geom_bar(aes(x = p1, fill = "model 1"), alpha = .5) +
  geom_bar(aes(x = p3, fill = "model 3"), alpha = .5) +
  facet_grid(~ payment_type) +
  xlab('tip percent prediction') +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

## ----chap07chunk17-------------------------------------------------------
dir.create(file.path(data_dir, 'output'), showWarnings = FALSE)
rx_split_xdf <- function(xdf = mht_xdf, split_perc = 0.75,
                         output_path = file.path(data_dir, "output"), ...) {
  # first create a column to split by
  outFile <- tempfile(fileext = 'xdf')
  rxDataStep(inData = xdf, outFile = xdf, transforms = list(
  	split = factor(ifelse(rbinom(.rxNumRows, size = 1, prob = splitperc), "train", "test"))),
  	transformObjects = list(splitperc = split_perc), overwrite = TRUE, ...)

  # then split the data in two based on the column we just created
  splitDS <- rxSplit(inData = xdf,
  outFilesBase = file.path(output_path, "train"),
  splitByFactor = "split",
  overwrite = TRUE)
  return(splitDS)
}

# we can now split to data in two
mht_split <- rx_split_xdf(xdf = mht_xdf, 
	varsToKeep = c('payment_type', 'fare_amount', 'tip_amount', 'tip_percent', 
								 'pickup_hour', 'pickup_dow', 'pickup_nb', 'dropoff_nb'))
names(mht_split) <- c("train", "test")

## ----chap07chunk18-------------------------------------------------------
system.time(linmod <- rxLinMod(tip_percent ~ pickup_nb:dropoff_nb + pickup_dow:pickup_hour, data = mht_split$train, reportProgress = 0))
system.time(dtree <- rxDTree(tip_percent ~ pickup_nb + dropoff_nb + pickup_dow + pickup_hour, data = mht_split$train, pruneCp = "auto", reportProgress = 0))
system.time(dforest <- rxDForest(tip_percent ~ pickup_nb + dropoff_nb + pickup_dow + pickup_hour, data = mht_split$train, nTree = 30, importance = TRUE, useSparseCube = TRUE, reportProgress = 0))

## ----chap07chunk19-------------------------------------------------------
trained.models <- list(linmod = linmod, dtree = dtree, dforest = dforest)
save(trained.models, file = 'trained_models.Rdata')

## ----chap07chunk20-------------------------------------------------------
pred_df <- expand.grid(ll[2:5])
pred_df_1 <- rxPredict(trained.models$linmod, data = pred_df, predVarNames = "pred_linmod")
pred_df_2 <- rxPredict(trained.models$dtree, data = pred_df, predVarNames = "pred_dtree")
pred_df_3 <- rxPredict(trained.models$dforest, data = pred_df, predVarNames = "pred_dforest")
pred_df <- do.call(cbind, list(pred_df, pred_df_1, pred_df_2, pred_df_3))

observed_df <- rxSummary(tip_percent ~ pickup_nb:dropoff_nb:pickup_dow:pickup_hour, mht_xdf)
observed_df <- observed_df$categorical[[1]][ , c(2:6)]
pred_df <- inner_join(pred_df, observed_df, by = names(pred_df)[1:4])

ggplot(data = pred_df) +
  geom_density(aes(x = Means, col = "observed average"), size = 1) +
  geom_density(aes(x = pred_linmod, col = "linmod"), size = 1) +
  geom_density(aes(x = pred_dtree, col = "dtree"), size = 1) +
  geom_density(aes(x = pred_dforest, col = "dforest"), size = 1) +
  xlim(-1, 30) +
  xlab("tip percent")

## ----chap07chunk21-------------------------------------------------------
rxPredict(trained.models$linmod, data = mht_split$test, outData = mht_split$test, predVarNames = "tip_percent_pred_linmod", overwrite = TRUE)
rxPredict(trained.models$dtree, data = mht_split$test, outData = mht_split$test, predVarNames = "tip_percent_pred_dtree", overwrite = TRUE)
rxPredict(trained.models$dforest, data = mht_split$test, outData = mht_split$test, predVarNames = "tip_percent_pred_dforest", overwrite = TRUE)

rxSummary(~ SSE_linmod + SSE_dtree + SSE_dforest, data = mht_split$test,
  transforms = list(
    SSE_linmod = (tip_percent - tip_percent_pred_linmod)^2,
    SSE_dtree = (tip_percent - tip_percent_pred_dtree)^2,
    SSE_dforest = (tip_percent - tip_percent_pred_dforest)^2))

## ----chap07chunk22-------------------------------------------------------
rxc <- rxCor( ~ tip_percent + tip_percent_pred_linmod + tip_percent_pred_dtree + tip_percent_pred_dforest, data = mht_split$test)
print(rxc)

## ----chap07chunk23, eval=FALSE-------------------------------------------
## # we re-build the linear model without payment_type for comparison
## linmod_1 <- rxLinMod(## formula goes here
##                      data = mht_split$train, reportProgress = 0)
## # we build a linear model with payment_type
## linmod_2 <- rxLinMod(## formula goes here
##                      data = mht_split$train, reportProgress = 0)
## # we build a decision tree with payment_type
## dtree <- rxDTree(## formula goes here
##                  data = mht_split$train, pruneCp = "auto", reportProgress = 0)
## 
## trained.models <- list(linmod_1 = linmod_1, linmod_2 = linmod_2, dtree = dtree)

## ----chap07chunk24, eval=FALSE-------------------------------------------
## pred_df <- expand.grid(ll)
## ## predict payment_type on pred_df by each of three models
## # pred_df_1 stores predictions made by linmod_1 into a column called pred_linmod_1
## # pred_df_2 stores predictions made by linmod_2 into a column called pred_linmod_2
## # pred_df_3 stores predictions made by dtree into a column called pred_dtree
## pred_df <- bind_cols(pred_df, pred_df_1, pred_df_2, pred_df_3)
## head(pred_df)

## ----chap07chunk25, eval=FALSE-------------------------------------------
## ggplot(data = pred_df) +
##   geom_density(aes(x = pred_linmod_1, col = "linmod_1"), size = 1) +
##   geom_density(aes(x = pred_linmod_2, col = "linmod_2"), size = 1) +
##   geom_density(aes(x = pred_dtree, col = "dtree"), size = 1) +
##   xlim(-10, 40) +
##   xlab("tip percent") +
##   facet_grid(payment_type ~ ., scales = "free")

## ----chap07chunk26, eval=FALSE-------------------------------------------
## test_df <- rxDataStep(mht_split$test,
##   varsToKeep = c('tip_percent', 'payment_type', 'pickup_nb', 'dropoff_nb', 'pickup_hour', 'pickup_dow'),
##   maxRowsByCols = 10^9)
## test_df_1 <- ## predict for the first model, call the predictions `tip_pred_linmod`
## test_df_2 <- ## predict for the second model, call the predictions `tip_pred_dtree`
## test_df_3 <- ## predict for the third model, call the predictions `tip_pred_dforest`

## ----chap07chunk27, eval=FALSE-------------------------------------------
## test_df <- do.call(cbind, list(test_df, test_df_1, test_df_2, test_df_3))
## 
## rxSummary(~ SSE_linmod + SSE_dtree + SSE_dforest, data = test_df,
##   transforms = list(
##     SSE_linmod = (tip_percent - tip_pred_linmod)^2,
##     SSE_dtree = (tip_percent - tip_pred_dtree)^2,
##     SSE_dforest = (tip_percent - tip_pred_dforest)^2))

## ----chap07chunk28-------------------------------------------------------
linmod_1 <- rxLinMod(tip_percent ~ pickup_nb:dropoff_nb + pickup_dow:pickup_hour, data = mht_split$train, reportProgress = 0)
linmod_2 <- rxLinMod(tip_percent ~ payment_type + pickup_nb:dropoff_nb + pickup_dow:pickup_hour, data = mht_split$train, reportProgress = 0)
dtree <- rxDTree(tip_percent ~ payment_type + pickup_nb + dropoff_nb + pickup_dow + pickup_hour, data = mht_split$train, pruneCp = "auto", reportProgress = 0)

trained.models <- list(linmod_1 = linmod_1, linmod_2 = linmod_2, dtree = dtree)

## ----chap07chunk29-------------------------------------------------------
pred_df <- expand.grid(ll)
pred_df_1 <- rxPredict(trained.models$linmod_1, data = pred_df, predVarNames = "pred_linmod_1")
pred_df_2 <- rxPredict(trained.models$linmod_2, data = pred_df, predVarNames = "pred_linmod_2")
pred_df_3 <- rxPredict(trained.models$dtree, data = pred_df, predVarNames = "pred_dtree")
pred_df <- bind_cols(pred_df, pred_df_1, pred_df_2, pred_df_3)
head(pred_df)

## ----chap07chunk30-------------------------------------------------------
ggplot(data = pred_df) +
  geom_density(aes(x = pred_linmod_1, col = "linmod_1"), size = 1) +
  geom_density(aes(x = pred_linmod_2, col = "linmod_2"), size = 1) +
  geom_density(aes(x = pred_dtree, col = "dtree"), size = 1) +
  xlim(-10, 40) +
  xlab("tip percent") +
  facet_grid(payment_type ~ ., scales = "free")

## ----chap07chunk31-------------------------------------------------------
test_df <- rxDataStep(mht_split$test, varsToKeep = c('tip_percent', 'payment_type', 'pickup_nb', 'dropoff_nb', 'pickup_hour', 'pickup_dow'), maxRowsByCols = 10^9)
test_df_1 <- rxPredict(trained.models$linmod_1, data = test_df, predVarNames = "tip_pred_linmod_1")
test_df_2 <- rxPredict(trained.models$linmod_2, data = test_df, predVarNames = "tip_pred_linmod_2")
test_df_3 <- rxPredict(trained.models$dtree, data = test_df, predVarNames = "tip_pred_dtree")
test_df <- do.call(cbind, list(test_df, test_df_1, test_df_2, test_df_3))
head(test_df)

## ----chap07chunk32-------------------------------------------------------
ggplot(data = test_df) +
  geom_density(aes(x = tip_pred_linmod_1, col = "linmod w/o payment type")) +
  geom_density(aes(x = tip_pred_linmod_2, col = "linmod w/ payment type")) +
  geom_density(aes(x = tip_pred_dtree, col = "dtree with payment type")) +
  xlab("tip percent") # + facet_grid(pickup_hour ~ pickup_dow)

## ----chap07chunk33-------------------------------------------------------
rxSummary(~ SSE_linmod_1 + SSE_linmod_2 + SSE_dtree, data = test_df,
  transforms = list(
    SSE_linmod_1 = (tip_percent - tip_pred_linmod_1)^2,
    SSE_linmod_2 = (tip_percent - tip_pred_linmod_2)^2,
    SSE_dtree = (tip_percent - tip_pred_dtree)^2))

## ----chap07chunk34-------------------------------------------------------
rxc <- rxCor( ~ tip_percent + tip_pred_linmod_1 + tip_pred_linmod_2 + tip_pred_dtree, data = test_df)
print(rxc)

