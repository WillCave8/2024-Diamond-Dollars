library(tidyverse)
library(caret)
library(xgboost)
library(data.table)

model_data <- read_csv("PBP_W_OBP.csv") 

model_data <- model_data %>% mutate(
  favorable_bat = ifelse(stand != p_throws, 1, 0)
)

model_binding_data <- model_data %>%
  mutate(prev_pitch_type = ifelse(prev_pitch_type == "FA", "FF", prev_pitch_type)) %>%
  filter(pitch_number != 1)
model_binding_data$pitch_type <- ifelse(is.na(model_binding_data$pitch_type), "FF", model_binding_data$pitch_type)
model_binding_data <- model_binding_data %>% na.omit()

xgb_model_data <- model_binding_data %>%
  select(-c(...1, game_pk, at_bat_number, game_year, pitch_type, zone, description,
            stf, loc, plate_x, plate_z, sz_top, sz_bot, avg_p_stf, avg_p_loc, vertical_miss, horizontal_miss, batter, pitcher,
            match_top_zone, prev_zone, prev_sz_top, prev_sz_bot, stand, p_throws)) %>%
  mutate(prev_pitch_type = ifelse(prev_pitch_type == "FA", "FF", prev_pitch_type)) %>%
  filter(pitch_number != 1) %>%
  na.omit()


xgb_model_data$on_base <- as.factor(xgb_model_data$on_base)
xgb_model_data$prev_pitch_type <- as.factor(xgb_model_data$prev_pitch_type)
xgb_model_data$prev_description <- as.factor(xgb_model_data$prev_description)

model_binding_data$on_base <- as.factor(model_binding_data$on_base)
model_binding_data$prev_pitch_type <- as.factor(model_binding_data$prev_pitch_type)
model_binding_data$prev_description <- as.factor(model_binding_data$prev_description)

levels(xgb_model_data$on_base) <- c("not_ob", "ob")


set.seed(123)

tune_grid <- expand.grid(
  nrounds = 150,           
  max_depth = 6,           
  eta = 0.3,                     
  gamma = 1.5,               
  colsample_bytree = 0.7,
  min_child_weight = 5,
  subsample = 0.7
)

ctrl <- trainControl(method = "cv", 
                     number = 5, 
                     verboseIter = TRUE,
                     classProbs = TRUE)

xgb_mid_ab_model <- train(
  on_base ~ .,                    
  data = xgb_model_data,
  method = "xgbTree",             
  trControl = ctrl,            
  tuneGrid = tune_grid,    
  verbose = TRUE,
  metric = "Accuracy",
  maximize = TRUE
)

saveRDS(xgb_mid_ab_model, "new_xgb.rds")

readRDS("xgb_model_file.rds") -> xgb_mid_ab_model


predictions <- predict(xgb_mid_ab_model, model_binding_data, type = "prob")

colnames(predictions) <- c("not_on_base_prob", "on_base_prob")

g_test <- cbind(model_binding_data, predictions)

predicted_obp_by_count_xgb <- data.frame(
  "1-0" = g_test %>%
    filter(balls == 1 & strikes == 0) %>%
    summarize(obp = mean(on_base_prob, na.rm = TRUE)) %>%
    pull(),
  "2-0" = g_test %>%
    filter(balls == 2 & strikes == 0) %>%
    summarize(obp = mean(on_base_prob, na.rm = TRUE)) %>%
    pull(),
  "3-0" = g_test %>%
    filter(balls == 3 & strikes == 0) %>%
    summarize(obp = mean(on_base_prob, na.rm = TRUE)) %>%
    pull(),
  "0-1" = g_test %>%
    filter(balls == 0 & strikes == 1) %>%
    summarize(obp = mean(on_base_prob, na.rm = TRUE)) %>%
    pull(),
  "1-1" = g_test %>%
    filter(balls == 1 & strikes == 1) %>%
    summarize(obp = mean(on_base_prob, na.rm = TRUE)) %>%
    pull(),
  "2-1" = g_test %>%
    filter(balls == 2 & strikes == 1) %>%
    summarize(obp = mean(on_base_prob, na.rm = TRUE)) %>%
    pull(),
  "3-1" = g_test %>%
    filter(balls == 3 & strikes == 1) %>%
    summarize(obp = mean(on_base_prob, na.rm = TRUE)) %>%
    pull(),
  "0-2" = g_test %>%
    filter(balls == 0 & strikes == 2) %>%
    summarize(obp = mean(on_base_prob, na.rm = TRUE)) %>%
    pull(),
  "1-2" = g_test %>%
    filter(balls == 1 & strikes == 2) %>%
    summarize(obp = mean(on_base_prob, na.rm = TRUE)) %>%
    pull(),
  "2-2" = g_test %>%
    filter(balls == 2 & strikes == 2) %>%
    summarize(obp = mean(on_base_prob, na.rm = TRUE)) %>%
    pull(),
  "3-2" = g_test %>%
    filter(balls == 3 & strikes == 2) %>%
    summarize(obp = mean(on_base_prob, na.rm = TRUE)) %>%
    pull()
) %>%
  t()

xgb_model <- xgb_mid_ab_model$finalModel

# Now you can pass this xgb_model to xgb.importance
importance_scores <- xgb.importance(model = xgb_model)

importance_matrix <- as.data.table(importance_scores) %>% head(10)

xgb.plot.importance(importance_matrix = importance_matrix,
                    xlab = "Relative Importance") +
  title("Feature Importance Plot")

xgb.ggplot.importance(importance_matrix)


g_test %>%
  select(-not_on_base_prob) -> pbp_mid_ab

first_pitch <- read_csv("first_pitch_obp.csv")
not_fp <- pbp_mid_ab %>%
  select(-c(...1))

str(first_pitch$on_base)
not_fp$on_base <- as.numeric(not_fp$on_base)
pbp_pr <- bind_rows(not_fp, first_pitch) %>%
  arrange(game_pk, at_bat_number, pitch_number) %>%
  group_by(game_pk, at_bat_number) %>%
  mutate(change_in_obp = on_base_prob - lag(on_base_prob, default = first(on_base_prob)))


bichette_1 <- pbp_pr %>% filter(game_pk == 716492, at_bat_number == 24)
bichette_2 <- pbp_pr %>% filter(game_pk == 718134, at_bat_number == 39)

fouls_model_data <- pbp_pr %>%
  mutate(on_base_change = lead(change_in_obp, default = NA)) %>%
  filter(description %in% c("foul", "foul_tip")) %>%
  mutate(in_zone = ifelse(zone %in% c(11,12,13,14),0,1)) %>%
  mutate(middle_middle = ifelse(plate_x >= -0.3 & 
                                  plate_x <= 0.3 & 
                                  plate_z >= 1.8 & 
                                  plate_z <= 3.0, 1, 0)) %>%
  mutate(not_middle_middle_but_strike = ifelse(middle_middle == 0 & in_zone == 1, 1, 0)) %>%
  mutate(miss_by = case_when(
    plate_z > sz_top ~ abs((plate_z - sz_top)),
    plate_z < sz_bot ~ abs((plate_z - sz_bot)),
    plate_x > 0.85 ~ abs(plate_x - 0.85),
    plate_x < -0.85 ~ abs(plate_x + 0.85),
    TRUE ~ 0
  )) %>%
  mutate(borderline = case_when(
    miss_by > 0.294 ~ 1,
    TRUE ~ 0
  )) %>% ungroup() %>%
  select(c(pitch_number, balls, strikes, match_top_zone, plate_x, plate_z, 
           stf, loc, middle_middle, in_zone, miss_by, not_middle_middle_but_strike, change = on_base_change)) %>%
  na.omit()

str(fouls_model_data)

write.csv(pbp_pr, "final_dataset.csv")

foul_tune_grid <- expand.grid(
  nrounds = 50,           
  max_depth = 6,           
  eta = 0.3,                     
  gamma = 1.5,               
  colsample_bytree = 0.7,
  min_child_weight = 1,
  subsample = 0.7
)

foul_ctrl <- trainControl(method = "cv", 
                     number = 5, 
                     verboseIter = TRUE,
                     classProbs = FALSE)

foul_xgb <- train(
  change ~ .,                    
  data = fouls_model_data,
  method = "xgbTree",             
  trControl =foul_ctrl,            
  tuneGrid = foul_tune_grid,    
  verbose = TRUE,
  metric = "RMSE",
  maximize = TRUE
)

xgbfoul <- xgb.importance(model = foul_xgb$finalModel)


foul_importance_matrix <- as.data.table(xgbfoul)

xgb.plot.importance(importance_matrix = foul_importance_matrix,
                    xlab = "Relative Importance") +
  title("Feature Importance Plot")


lm(change ~., data = fouls_model_data) %>% 
  summary()

write.csv(fouls_model_data, "fouls_model_data.csv")
