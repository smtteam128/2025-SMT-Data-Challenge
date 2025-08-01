#----------------NOTE-----------------------------------------------
#' This contains our modelling and application section. Please run our 
#' CF/LF/RF scripts to get the data and run our Python script to get hitter times
library(randomForest)
library(pROC)
library(vip)
library(caret)
#Merge the data
play_data <- bind_rows(final_overall_everything_merge_cf, final_overall_everything_merge_rf, final_overall_everything_merge_lf)

play_data <- play_data %>%
  mutate(
    distance_to_target = sqrt((ball_landing_location_x - field_x)^2 + 
                                (ball_landing_location_y - field_y)^2)
  )
#Hitter times-------------------------------------------
#Load in hitter times
sprint_times <- read.csv("all_games_projected_60_yard_dash.csv")

#Some hitters may be jogging on most plays. Lets get the top 95 percent of their runs
hitter_times <- sprint_times %>%
  group_by(Hitter_ID) %>%
  summarize(
    mean_time = mean(Projected_60_Yard_Time_s, na.rm = TRUE),
    total = n(),
    #Speed is decreasing
    sprint_time_95th = quantile(Projected_60_Yard_Time_s, probs = 0.05, na.rm = TRUE), 
    max_speed_basepath = quantile(Max_Speed_Range, probs = 0.95, na.rm = TRUE),
    time_to_max_95th = quantile(Time_to_Max_Speed_s, probs = 0.05, na.rm = TRUE)
    
  )

hitter_times <- select(hitter_times, total, player_code = Hitter_ID, sprint_time_95th, time_to_max_95th, max_speed_basepath)
hitter_times <- filter(hitter_times, total > 5) #We want players with at least 5 baserunning attempts

#Join the play data with the hitter sprint times
plays_sprints <- left_join(play_data, hitter_times)
sum(is.na(plays_sprints$max_speed_basepath)) 
#Lot of plays where we don't have the outfielder's sprint time. Let's filter them out

#Filter out plays where there is a player with no speed associated with them
plays_sprints <- filter(plays_sprints, !is.na(max_speed_basepath))
plays_sprints <- filter(plays_sprints, !is.na(player_code))

#Any sprint speed above 35 is extremely unlikely to be seen
range(plays_sprints$max_speed_basepath, na.rm = T)
plays_sprints <- filter(plays_sprints, max_speed_basepath < 35)

#Lets get player direction as well
plays_sprints <- plays_sprints %>%
  mutate(
    dx = ball_landing_location_x - field_x,
    dy = ball_landing_location_y - field_y,
    direction_angle = atan2(dy, dx), 
    direction_deg = (atan2(dy, dx) * 180 / pi) %% 360  #Going forward = 0, going back = 180
  )

#Convert to polar coordinates
plays_sprints <- plays_sprints %>%
  mutate(r = sqrt(field_x^2 + field_y^2),
         theta = atan2(field_y, field_x))
plays_sprints <- filter(plays_sprints, hang_time > 0.75)

#Model preparation - stratified random sampling----------------------------
# Extract day from the game string
plays_sprints$day <- sub(".*d(\\d{3}).*", "\\1", plays_sprints$game_str)
plays_sprints$day <- as.numeric(plays_sprints$day)

# Breaking into quarters for stratified sampling
plays_sprints <- plays_sprints %>%
  mutate(
    quarter = case_when(
      day <= 25 ~ 1,
      day <= 50 ~ 2,
      day <= 75 ~ 3,
      TRUE      ~ 4
    )
  )

# 80/20 train/test split, stratified by quarter
set.seed(617)
train_play_ids <- plays_sprints %>%
  distinct(game_str, play_id) %>%
  slice_sample(prop = 0.8)

train_df <- plays_sprints %>%
  semi_join(train_play_ids, by = c("game_str", "play_id"))

test_df <- plays_sprints %>%
  anti_join(train_play_ids, by = c("game_str", "play_id"))

#Random forest - Model 1 ----
colnames(train_df)


#Feature variables
features_no_speed <- c(
  "r",
  "theta",
  "distance_to_target",
  "hang_time",
  "direction_deg"
)

#RF data preparations
train_df$is_out <- as.factor(train_df$is_out)
test_df$is_out  <- as.factor(test_df$is_out)

#Cross validation---------------------------------------------------------

set.seed(617) #Protect the parquet

train_control <- trainControl(
  method = "cv",
  number = 5,
  classProbs = TRUE,
  summaryFunction = twoClassSummary, 
  savePredictions = "final"
)
rf_grid <- expand.grid(
  mtry = c(2, 3, 4) #We are running 3 Random Forests to see which sampling is best (hypertuning)
)

str(train_df)
train_cv <- train_df
train_cv$is_out <- factor(train_cv$is_out, levels = c("0", "1"), labels = c("no", "yes"))

cv_no_speed <- train(
  x = train_cv[, features_no_speed],
  y = train_cv$is_out,
  method = "rf",
  metric = "ROC", 
  trControl = train_control,
  tuneGrid = rf_grid,
  ntree = 500
)

print(cv_no_speed)
plot(cv_no_speed)
varImp(cv_no_speed)
print(cv_no_speed$resample)

#Model proper------------------------------------------------------
set.seed(617)
naive_rf <- randomForest(
  x = train_df[, features_no_speed],
  y = train_df$is_out,
  ntree = 500, #Trial and error - This value is best
  mtry = 2,    
  classwt = c("0" = 1, "1" = 5.37), #Cause outs/no outs is imbalenced
  importance = TRUE 
)

#Prediction
test_df$naive_catch_prob <- predict(naive_rf, newdata = test_df, type = "prob")[, "1"]
test_df$naive_catch_prob <- predict(naive_rf, newdata = test_df, type = "prob")[, "1"]

#Evaluating the model---------------------------------------------------

#AUC/ROC
roc_obj <- roc(test_df$is_out, test_df$naive_catch_prob)
auc_val <- auc(roc_obj)
cat("AUC (Random Forest):", auc_val, "\n")


#Log loss
preds <- test_df$naive_catch_prob
test_label <- as.numeric(as.character(test_df$is_out)) 
logloss <- -mean(test_label * log(preds + 1e-15) + (1 - test_label) * log(1 - preds + 1e-15))
cat("Log Loss (Random Forest):", logloss, "\n")


#Feature importance
importance <- importance(naive_rf)
print(importance)
vip(naive_rf)


#Confusion matrix
threshold <- 0.5
predicted_class <- ifelse(test_df$naive_catch_prob >= threshold, 1, 0)


confusion_matrix <- table(
  Actual = test_df$is_out,
  Predicted = predicted_class
) 
print(confusion_matrix)

#Looking at precision and recall
TP <- confusion_matrix["1", "1"]
FP <- confusion_matrix["0", "1"]
FN <- confusion_matrix["1", "0"]
precision <- TP / (TP + FP)
recall <- TP / (TP + FN)

cat("Precision:", precision, "\n")
cat("Recall:", recall, "\n")

#Joining the data with the OG
plays_sprints$naive_catch_prob <- predict(naive_rf, newdata = plays_sprints, type = "prob")[, "1"]

summary(plays_sprints$naive_catch_prob)

#Model 2 - GAM-----------------------------------------------------------

#GAM model
set.seed(617)  
#Train/test split
train_idx <- sample(nrow(plays_sprints), 0.8 * nrow(plays_sprints))
gam_train_data <- plays_sprints[train_idx, ]
gam_test_data <- plays_sprints[-train_idx, ]

#Running the model - With interaction term
gam_model <- gam(is_out ~ s(naive_catch_prob) + s(sprint_time_95th) + 
                   s(naive_catch_prob, sprint_time_95th), 
                 data = gam_train_data, 
                 family = binomial)

#Adding to test df
gam_predictions <- predict(gam_model, gam_test_data, type = "response")
test_df$adjusted_prob <- predict(gam_model, newdata = test_df, type = "response")

test_df$residual <- test_df$adjusted_prob - test_df$naive_catch_prob
summary(test_df$residual)

#Let's get the confusion matrix for this model
predicted_gam <- ifelse(test_df$adjusted_prob >= threshold, 1, 0)

confusion_matrix_gam <- table(
  Actual = test_df$is_out,
  Predicted = predicted_gam
)
confusion_matrix_gam
plays_sprints$adjusted_prob <- predict(gam_model, 
                                       newdata = plays_sprints, 
                                       type = "response")

plays_sprints$residual <- plays_sprints$adjusted_prob - plays_sprints$naive_catch_prob


#Applications--------------------------------------------------------------
#Get the player with the highest catch probability on a given play
#Player-level data - Way to compare both metrics
players <- plays_sprints %>%
  group_by(player_code) %>%
  summarize(total = n(), exp_naive_prob = mean(naive_catch_prob), 
            exp_adj_prob = mean(adjusted_prob), 
            catch_perc = mean(is_out), 
            naive_oaa = sum(is_out - naive_catch_prob), 
            sixty_yard_dash = mean(sprint_time_95th), 
            residual = mean(residual))

plays_sprints <- plays_sprints %>%
  group_by(game_str, play_id) %>%
  mutate(highest_prob = ifelse(adjusted_prob == max(adjusted_prob), 1, 0)) %>%
  ungroup()

#Let us actually get the value of the player with the highest probability on the play
high_prob <- filter(plays_sprints, highest_prob == 1)
high_prob <- select(high_prob, game_str, play_id, highest_prob_on_play =  adjusted_prob)
plays_sprints <- left_join(plays_sprints, high_prob)


#Calculate Kelly Leak score
plays_sprints$leak_score <- ifelse(plays_sprints$is_out == 1 & plays_sprints$highest_prob == 0, 
                                   plays_sprints$highest_prob_on_play - plays_sprints$adjusted_prob, 0)
leak_score_agg <- plays_sprints %>%
  group_by(player_code) %>%
  summarize(total = sum(leak_score > 0), leak_score = sum(leak_score))

#Player responsibility score - RRS + TRS
responsibility_scores <- plays_sprints %>%
  group_by(game_str, play_id) %>%
  filter(n() == 3) %>%
  mutate(raw_responsibility_score = round(adjusted_prob/(sum(adjusted_prob))*adjusted_prob, 4)) %>%
  mutate(true_responsibility_score = is_out - raw_responsibility_score) %>%
  ungroup()

#Aggregate to the player level
player_responsibility_score <- responsibility_scores %>%
  group_by(player_code) %>%
  summarize(trs = sum(true_responsibility_score), oaa = sum(is_out - naive_catch_prob), total = n())

#Save models and values
saveRDS(naive_rf, "random_forest_model.rds")
saveRDS(gam_model, "gam_model.rds")
write.csv(plays_sprints, "final_dataset.csv")


