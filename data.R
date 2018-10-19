library(readr)
library(tidyverse)
library(xgboost)
library(lubridate)
library(magrittr)
set.seed(12345)

cold_start_test <-
  read_csv("cold_start_test.csv") %>% as.data.frame()
consumption_train <-
  read_csv("consumption_train.csv") %>% as.data.frame()
meta <- read_csv("meta.csv") %>% as.data.frame()
submission_format <-
  read_csv("submission_format.csv") %>% as.data.frame()


test <- cold_start_test %>% 
  left_join(meta, by = "series_id") %>%
  mutate(
    weekday = lubridate::wday(timestamp),
    hour = lubridate::hour(timestamp),
    month = lubridate::month(timestamp),
    day = lubridate::day(timestamp),
    surface = as.factor(surface),
    base_temperature = as.factor(base_temperature),
    offday = case_when(
      monday_is_day_off == "True" & weekday == 2 ~ 1,
      tuesday_is_day_off == "True" & weekday == 3 ~ 1,
      wednesday_is_day_off == "True" & weekday == 4 ~ 1,
      thursday_is_day_off == "True" & weekday == 5 ~ 1,
      friday_is_day_off == "True" & weekday == 6 ~ 1,
      saturday_is_day_off == "True" & weekday == 7 ~ 1,
      sunday_is_day_off == "True" & weekday == 1 ~ 1,
      TRUE ~ 0    ),
    is_holiday = ifelse(month == 1 & day == 1, 1 ,0),
    is_holiday = ifelse(month == 12 & day == 24, 1 ,is_holiday),
    is_holiday = ifelse(month == 12 & day == 25, 1 ,is_holiday)
  )


subs <- submission_format %>% 
  left_join(meta, by = "series_id") %>%
  mutate(
    # weekday = lubridate::wday(timestamp),
    # hour = lubridate::hour(timestamp),
    # month = lubridate::month(timestamp),
    # day = lubridate::day(timestamp),
    surface = as.factor(surface),
    base_temperature = as.factor(base_temperature)
  )



expand_time <- function(data, idx) {
  xxx <- data[idx, ]
  
  if(xxx$prediction_window == "daily"){
    res <- xxx %>%
      # select(-timestamp) %>%
      left_join(data_frame(
        pred_id = xxx$pred_id[1],
        timestamp2 = seq(
          from = xxx$timestamp[1],
          to = xxx$timestamp[1] + hours(23),
          by = "hour"
        )
      ),
      by = "pred_id")
  }else if(xxx$prediction_window == "weekly"){
    res <- xxx %>%
      # select(-timestamp) %>%
      left_join(data_frame(
        pred_id = xxx$pred_id[1],
        timestamp2 = seq(
          from = xxx$timestamp[1],
          to = xxx$timestamp[1] + weeks(1) - hours(1),
          by = "hour"
        )
      ),
      by = "pred_id")
    
  }else if(xxx$prediction_window == "hourly"){
    res <- xxx %>% 
      mutate(timestamp2 = timestamp)
  
    }

  
  return(res)
  
}


subs_hourly <- 1:nrow(subs) %>% map_df(~ expand_time(subs, .x)) %>% 
  mutate(
    weekday = lubridate::wday(timestamp2),
    hour = lubridate::hour(timestamp2),
    month = lubridate::month(timestamp2),
    day = lubridate::day(timestamp2),
    surface = as.factor(surface),
    base_temperature = as.factor(base_temperature),
    offday = case_when(
      monday_is_day_off == "True" & weekday == 2 ~ 1,
      tuesday_is_day_off == "True" & weekday == 3 ~ 1,
      wednesday_is_day_off == "True" & weekday == 4 ~ 1,
      thursday_is_day_off == "True" & weekday == 5 ~ 1,
      friday_is_day_off == "True" & weekday == 6 ~ 1,
      saturday_is_day_off == "True" & weekday == 7 ~ 1,
      sunday_is_day_off == "True" & weekday == 1 ~ 1,
      TRUE ~ 0    ),
    is_holiday = ifelse(month == 1 & day == 1, 1 ,0),
    is_holiday = ifelse(month == 12 & day == 24, 1 ,is_holiday),
    is_holiday = ifelse(month == 12 & day == 25, 1 ,is_holiday)
  )


test %>% 
  group_by(surface, offday) %>% 
  summarise(mean = mean(consumption),
            n = n()
            )


vals <- test %>% 
  group_by(series_id, weekday, hour) %>% 
  summarise(mean_consum_wh = mean(consumption, na.rm = TRUE)) 


vals_2 <- test %>% 
  group_by(series_id, hour) %>% 
  summarise(mean_consum_h = mean(consumption, na.rm = TRUE)) 

vals_3 <- test %>% 
  group_by(series_id, offday, hour) %>% 
  summarise(mean_consum_offh = mean(consumption, na.rm = TRUE)) 


vals_alloff <- test %>% 
  group_by(surface, offday) %>% 
  summarise(mean_consum_sf = mean(consumption, na.rm = TRUE)) 

vals_holi <- test %>% 
  group_by(is_holiday, hour) %>% 
  summarise(mean_consum_holi = mean(consumption, na.rm = TRUE)) 

vals_low <- test %>%
  filter(offday == 0) %>% 
  group_by(series_id) %>% 
  summarise(min_consum = min(consumption, na.rm = TRUE)) 



##### Model best 
final <- subs_hourly %>%
  left_join(vals, by = c("series_id", "weekday", "hour")) %>%
  left_join(vals_3, by = c("series_id", "offday", "hour")) %>%
  left_join(vals_2, by = c("series_id", "hour")) %>%
  left_join(vals_alloff, by = c("surface", "offday")) %>%
  mutate(consumption = ifelse(is.na(mean_consum_wh),mean_consum_offh,mean_consum_wh),
         consumption = ifelse(is.na(consumption),mean_consum_h,consumption),
         consumption = ifelse(is.na(consumption),mean_consum_sf,consumption)
         # consumption = ifelse(is_holiday == 1,mean_consum_holi,consumption)
         )

##### Model 1 
final <- subs_hourly %>% 
  left_join(vals, by = c("series_id", "weekday", "hour")) %>% 
  left_join(vals_3, by = c("series_id", "offday", "hour")) %>% 
  left_join(vals_2, by = c("series_id", "hour")) %>% 
  left_join(vals_alloff, by = c("surface", "offday")) %>%
  left_join(vals_low, by = c("series_id")) %>% 
  # left_join(vals_holi, by = c("is_holiday", "hour")) %>% 
  mutate(consumption = ifelse(is.na(mean_consum_wh),mean_consum_offh,mean_consum_wh),
         consumption = ifelse(is.na(consumption) & offday == 1,min_consum*1.3,consumption),
         consumption = ifelse(is.na(consumption) & offday == 0,mean_consum_h*1.2,consumption),
         consumption = ifelse(is.na(consumption),mean_consum_h,consumption),
         consumption = ifelse(is.na(consumption),mean_consum_sf,consumption)
         # consumption = ifelse(,mean_consum_sf,consumption)
         
         # consumption = ifelse(is_holiday == 1,mean_consum_holi,consumption)
  ) 


final_sum <-  final %>% 
  group_by(pred_id) %>% 
  summarise(consumption = sum(consumption))



submission_format %>% 
  select(-consumption) %>% 
  left_join(final_sum, by = "pred_id") %>% 
  select(pred_id, 
         series_id, 
         timestamp, 
         temperature,
         consumption,
         prediction_window ) %>% 
  write_csv("detail_3.csv")


test %>% 
  ggplot(aes(x = temperature, y = consumption, color = surface)) +
  geom_point(alpha = 0.5)+
  geom_smooth()


library(scales)

ids <- final %>% 
  # filter(offday == 1) %>% 
  pull(series_id) %>% 
  unique()
id  <-  sample(ids, 1) #100148

meta <- final %>% filter(series_id == id) %>% 
  select(prediction_window, surface, base_temperature) 
  
p1 <- test %>% filter(series_id == id) %>% 
  ggplot(aes(x = timestamp, y = consumption)) +
  geom_point()+
  geom_line() + 
  geom_point(data = final %>% filter(series_id == id), 
             aes(x = timestamp2, y = consumption), color = "blue")+
  geom_line(data = final %>% filter(series_id == id), 
            aes(x = timestamp2, y = consumption), color = "blue") +
  ggtitle(paste0(id,",",meta$prediction_window ,",",meta$surface,",",meta$base_temperature))+
  scale_x_datetime(date_breaks = "1 day",
                   date_minor_breaks = "1 hour",
                   date_labels = "%y-%b-%d:%a"
                   ) +
  theme(text = element_text(size=16),
        axis.text.x = element_text(angle=45, hjust=1)
        )

p2 <- test %>% filter(series_id == id) %>% 
  ggplot(aes(x = timestamp, y = consumption)) +
  geom_point()+
  geom_line() + 
  geom_point(data = final2 %>% filter(series_id == id), 
             aes(x = timestamp2, y = consumption), color = "blue")+
  geom_line(data = final2 %>% filter(series_id == id), 
            aes(x = timestamp2, y = consumption), color = "blue") +
  ggtitle(paste0(id,",",meta$prediction_window ,",",meta$surface,",",meta$base_temperature))+
  scale_x_datetime(date_breaks = "1 day",
                   date_minor_breaks = "1 hour",
                   date_labels = "%y-%b-%d:%a"
  ) +
  theme(text = element_text(size=16),
        axis.text.x = element_text(angle=45, hjust=1)
  )

gridExtra::grid.arrange(p1,p2,ncol = 1)






test %>% filter(series_id == id) %>% select(monday_is_day_off:sunday_is_day_off) %>% distinct() %>% dplyr::slice(1)



test %>% 
  filter(series_id == 100491) %>% 
  group_by(weekday) %>% 
  summarise(con = mean(consumption))

# 103464 --- only given off day but need to predict weekly (x2)
# 100128 --- given one day not off need to predict off day


# is there any holidays?


final %>% 
  mutate(
    is_holiday = ifelse(month == 1 & day == 1, 1 ,0),
    is_holiday = ifelse(month == 12 & day == 24, 1 ,is_holiday),
    is_holiday = ifelse(month == 12 & day == 25, 1 ,is_holiday)
    # is_holiday = ifelse(month == 12 & day == 25, 1 ,is_holiday)
    ) %>% 
  group_by(hour, is_holiday) %>% 
  summarise(mean = mean(consumption, na.rm = TRUE),
            n = n())


final %>% 
  group_by(hour, is_holiday) %>% 
  summarise(mean = mean(consumption, na.rm = TRUE),
            n = n()) %>% View()



test %>% 
  mutate(
    is_newyear = ifelse(month == 1 & day == 1, 1 ,0)
  ) %>% 
  group_by(hour) %>% 
  summarise(mean = mean(consumption, na.rm = TRUE),
            n = n())



tri <- 1:nrow(consumption_train)

tr_te <- bind_rows(consumption_train, cold_start_test)
# consumption_train %>% filter(series_id == 100712)
#
consumption_train %>%
  group_by(series_id) %>%
  summarise(range = max(timestamp) - min(timestamp))

y <- tr_te$consumption

tr_te_new <- tr_te %>%
  left_join(meta, by = "series_id") %>%
  mutate(
    weekday = lubridate::wday(timestamp),
    hour = lubridate::hour(timestamp),
    month = lubridate::month(timestamp),
    day = lubridate::day(timestamp),
    surface = as.factor(surface),
    base_temperature = as.factor(base_temperature)
  ) %>%
  mutate_if(is.character, as.logical) %>%
  select(-X1,-series_id,-consumption, -timestamp) %>%
  data.matrix()

futile.logger::flog.info("Preparing data for model (separate train and test, etc)...")

dtrain <- xgb.DMatrix(data = tr_te_new[tri,], label = y[tri])
dtest <- xgb.DMatrix(data = tr_te_new[-tri,], label = y[-tri])
cols <- colnames(dtrain)

dsub <- submission_format %>%
  left_join(meta, by = "series_id") %>%
  mutate(
    weekday = lubridate::wday(timestamp),
    hour = lubridate::hour(timestamp),
    month = lubridate::month(timestamp),
    day = lubridate::day(timestamp),
    surface = as.factor(surface),
    base_temperature = as.factor(base_temperature)
  ) %>%
  mutate_if(is.character, as.logical) %>%
  select(-pred_id, -series_id,-consumption, -timestamp, -prediction_window) %>%
  data.matrix()


# tr_new <- tr_te_new[tri, ]
# tri2 <- caret::createDataPartition(y, p = 0.9, list = F) %>% c()
# dtrain <- xgb.DMatrix(data = tr_new[tri2, ], label = y[tri2])
# dval <- xgb.DMatrix(data = tr_new[-tri2, ], label = y[-tri2])

#---------------------------
cat("Training model...\n")
p <- list(
  objective = "reg:linear",
  booster = "gbtree",
  eval_metric = "mae",
  nthread = 10,
  eta = 0.1,
  max_depth = 7,
  min_child_weight = 1,
  # gamma = 0,
  # subsample = 0.85,
  # colsample_bytree = 0.7,
  # colsample_bylevel = 0.6,
  # alpha = 0,
  # lambda = 0,
  #scale_pos_weight = 2,
  nrounds = 2000
)
# cv <- xgb.cv(params = p, data = dtrain, nrounds = p$nrounds, nfold = 5,verbose = T)

m_xgb <-
  xgb.train(
    p,
    dtrain,
    p$nrounds,
    list(val = dtest),
    print_every_n = 1,
    early_stopping_rounds = 10
  )

xgb.importance(cols, model = m_xgb) %>% write_csv(., path = "feature_importance_1.csv")

submission_format %>%
  mutate(
    consumption = predict(m_xgb, dsub)
  ) %>%
  mutate(consumption = ifelse(prediction_window == "weekly", consumption*24*6,consumption),
         consumption = ifelse(prediction_window == "daily", consumption*24,consumption)
         ) %>%
  write_csv(paste0("xgb_", round(m_xgb$best_score, 4), ".csv"))


# random search -----------------------------------------------------------


best_param <- list()
best_rmse <- Inf
best_rmse_index <- 0

res <- data.frame(
  max_depth = double(),
  eta = double(),
  subsample = double(),
  colsample_bytree = double(),
  min_child_weight = integer(),
  max_delta_step = integer(),
  best_iteration = integer(),
  min_rmse = double()
)


for (iter in 1:100) {
  futile.logger::flog.info(iter)
  param <- list(
    objective = "reg:linear",
    booster = "gbtree",
    eval_metric = "rmse",
    nthread = 15,
    max_depth = sample(6:10, 1),
    eta = runif(1, 0.01, 0.4),
    # Learning rate, default: 0.3
    subsample = runif(1, .8, 1),
    colsample_bytree = runif(1, .8, 1),
    min_child_weight = sample(1:40, 1),
    max_delta_step = sample(1:10, 1)
  )
  cv.nround <- 1000
  cv.nfold <-  5 # 5-fold cross-validation
  # seed.number  <-  sample.int(10000, 1) # set seed for the cv
  # set.seed(seed.number)
  mdcv <- xgb.cv(
    data = dtrain,
    params = param,
    nfold = cv.nfold,
    nrounds = cv.nround,
    verbose = F,
    early_stopping_rounds = 100,
    maximize = FALSE
  )
  
  min_rmse_index  <-  mdcv$best_iteration
  min_rmse <-  mdcv$evaluation_log[min_rmse_index]$test_rmse_mean
  
  data.frame(
    max_depth = param$max_depth,
    eta = param$eta,
    subsample = param$subsample,
    colsample_bytree = param$colsample_bytree,
    min_child_weight = param$min_child_weight,
    max_delta_step = param$max_delta_step,
    best_iteration = min_rmse_index,
    min_rmse = min_rmse
  ) %>% bind_rows(res) -> res
  
  # if (min_rmse < best_rmse) {
  #   best_rmse <- min_rmse
  #   best_rmse_index <- min_rmse_index
  #   # best_seednumber <- seed.number
  #   best_param <- param
  # }
}





# The best index (min_rmse_index) is the best "nround" in the model
nround = best_rmse_index
set.seed(best_seednumber)
xg_mod <-
  xgb.train(
    best_param,
    dtrain,
    nrounds = 500,
    list(val = dval),
    print_every_n = 50,
    early_stopping_rounds = 50
  )


# bayesian ----------------------------------------------------------------



library(rBayesianOptimization)

bayesian_eval <- function(nrounds, eta, max_depth) {
  params <- list(
    objective = "reg:linear",
    nrounds = nrounds,
    eta = eta,
    max_depth = max_depth
  )
  
  mdl <- xgboost(
    data = Dtrain,
    params = params,
    nrounds = params[["nrounds"]],
    verbose = 0
  )
  
  preds <- predict(mdl, Dtest)
  val_data <- copy(random_train[40001:63700])
  
  val_data[, predictions := preds]
  val_data[, m := mean(consumption), by = series_id]
  
  nmae <- mean(val_data[, abs(predictions - consumption) / m])
  
  list(Score = -nmae,
       Pred = preds)
}

BayesianOptimization(
  bayesian_eval,
  bounds = list(
    nrounds = c(30L, 500L),
    eta = c(0.05, 0.7),
    max_depth = c(1L, 20L)
  ),
  init_points = 10,
  n_iter = 50
)
