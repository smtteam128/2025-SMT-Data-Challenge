#----------------NOTE----------------------------------------------
#' We are basically doing the same thing here for left fielders as we did for
#' center fielders. As a result, this section will be commented out minimally.
#' Please run the center fielders script first before you run this script. This
#' script can be run before the right fielders script or after. Order does not 
#' matter.
library(tidyverse)
library(arrow)
#Loading in data---------------------------------------------------
data_directory <- "~/Desktop/SMT-Data-Challenge-2025-Updated"

game_info_input <- arrow::open_csv_dataset(paste0(data_directory,"/game_info"), 
                                           partitioning = c("HomeTeam", "AwayTeam", "Season", "Day"), 
                                           hive_style = F, 
                                           unify_schemas = T, 
                                           na = c("", "NA", "NULL", NA, "\\N"))

ball_pos_input <- arrow::open_csv_dataset(paste0(data_directory,"/ball_pos"), 
                                          partitioning = c("HomeTeam", "AwayTeam", "Season", "Day"), 
                                          col_names = c("game_str", "play_id", "timestamp",
                                                        "ball_position_x", "ball_position_y", "ball_position_z"),
                                          hive_style = F, 
                                          unify_schemas = T, 
                                          na = c("", "NA", "NULL", NA, "\\N"))

game_events_input <- arrow::open_csv_dataset(paste0(data_directory,"/game_events"), 
                                             partitioning = c("HomeTeam", "AwayTeam", "Season", "Day"),  
                                             hive_style = F, 
                                             unify_schemas = T, 
                                             na = c("", "NA", "NULL", NA, "\\N"))

player_pos_input <- arrow::open_csv_dataset(paste0(data_directory,"/player_pos"), 
                                            partitioning = c("HomeTeam", "AwayTeam", "Season", "Day"), 
                                            col_names = c("game_str", "play_id", "timestamp",
                                                          "player_position", "field_x", "field_y"),
                                            hive_style = F, 
                                            unify_schemas = T, 
                                            na = c("", "NA", "NULL", NA, "\\N"))

rosters_input <- arrow::open_csv_dataset(paste0(data_directory,"/rosters.csv"), 
                                         hive_style = F, 
                                         unify_schemas = T, 
                                         na = c("", "NA", "NULL", NA, "\\N"))
#Load in all of game info
game_info <- game_info_input %>%
  collect()

#Load in all of game events
game_events <- game_events_input %>%
  collect()

#Basic data wrangling---------------------------------------------
game_events_bbip_rf <- game_events %>%
  filter(player_position == 9, event_code == 2) %>%
  distinct(game_str, play_id, .keep_all = TRUE)

#We only want game events where the center fielder picked up the ball
game_events_rf <- game_events %>%
  semi_join(game_events_bbip_rf, by = c("game_str", "play_id"))

#Getting plays where the center fielder caught the ball (out)
game_events_rf <- game_events_rf %>%
  arrange(game_str, play_per_game, timestamp) %>%
  group_by(game_str, play_per_game) %>%
    mutate(
      is_out = as.integer(
        any(player_position == 9 & event_code == 2 &
              lag(player_position) == 10 & lag(event_code) == 4, na.rm = TRUE)
      )
    ) %>%
  ungroup()

#Load in game info
game_info <- as.data.frame(game_info)

#Join game info with game events to get player names
players_rf_fly_balls <- game_events_rf %>%
  
  left_join(game_info, by = join_by("game_str", "at_bat", "play_per_game", 
                                    "Season", "HomeTeam","AwayTeam", "Day")) %>%
  select(game_str, play_id, at_bat, play_per_game, timestamp, 
         player_position, event_code, right_field, is_out) 

players_rf_touched <- players_rf_fly_balls %>%
  filter(!is.na(right_field))


#Get hang time
players_rf_touched <- players_rf_touched %>%
  arrange(game_str, play_id, timestamp) %>%
  group_by(game_str, play_id) %>%
  mutate(
    hang_time = ifelse(
      lag(player_position) == 10 & lag(event_code) == 4 & player_position == 9 & event_code == 2,
      round((timestamp - lag(timestamp)) / 1000, 2),
      ifelse(
        lag(player_position) == 10 & lag(event_code) == 4 & player_position == 255 & event_code == 16,
        round((timestamp - lag(timestamp)) / 1000, 2),
        ifelse(
          lag(player_position) == 10 & lag(event_code) == 4 & event_code %in% c(9, 10),
          round((timestamp - lag(timestamp)) / 1000, 2),
          NA_real_
        )
      )
    )
  ) %>%
  fill(hang_time, .direction = "downup") %>%  # Fill hang_time within each play group
  ungroup()


players_rf_touched <- players_rf_touched %>%
  filter(!is.na(hang_time))

#A sequence of code that only keeps the timestamps where the ball hit the ground or 
players_rf_transitions <- players_rf_touched %>%
  arrange(game_str, play_id, timestamp) %>%
  group_by(game_str, play_id) %>%
    mutate(
      valid_followup = lag(player_position) == 10 & lag(event_code) == 4 &
        (
          (player_position == 9 & event_code == 2) |
            (player_position == 255 & event_code == 16) |
            (event_code %in% c(9, 10))
        )
    ) %>%
    mutate(
      keep_row = valid_followup | lead(valid_followup)
    ) %>%
  ungroup() %>%
  filter(keep_row)

players_rf_transitions <- players_rf_transitions %>%
  mutate(game_str_play_id = paste(game_str, play_id, sep = "_"))

#Player position wrangling -------------------------------------------------
players_rf_pos <- player_pos_input %>%
  filter(player_position == 9) %>%
  select(-c("Season", "HomeTeam", "AwayTeam", "Day")) %>%
  collect()

players_rf_pos <- players_rf_pos %>%
  mutate(game_str_play_id = paste(game_str, play_id, sep = "_"))

players_rf_pos <- filter(players_rf_pos, game_str_play_id %in% players_rf_transitions$game_str_play_id)

table(players_rf_pos$game_str)
players_rf_pos$play_id <- as.integer(players_rf_pos$play_id)
players_rf_pos$timestamp <- as.integer(players_rf_pos$timestamp)
players_rf_pos$player_position <- as.integer(players_rf_pos$player_position)

#Bringing it all together

#Get the player codes

player_pos_game_events_rf <- left_join(players_rf_pos, players_rf_transitions, by = c("game_str", "play_id", "timestamp"))
player_pos_game_events_rf$start_stop <- ifelse(!(is.na(player_pos_game_events_rf$player_position.y)), 1, 0)

#Getting the start/stop times -------------------------------------------
start_stop_times_rf <- player_pos_game_events_rf %>%
  group_by(game_str, play_id) %>%
    filter(start_stop == 1) %>%
    summarise(
      start_time = min(timestamp),
      stop_time  = max(timestamp),
      .groups = "drop"
    )

rf_interpolated <- player_pos_game_events_rf %>%
  left_join(start_stop_times_rf, by = c("game_str", "play_id")) %>%
  mutate(
    between_flag = if_else(timestamp >= start_time & timestamp <= stop_time, 1L, NA_integer_)
  ) %>%
  select(-start_time, -stop_time)



final_df_rf <- filter(rf_interpolated, between_flag == 1)

#Getting hangtime and other finalization -----------------------------------
final_df_rf$field_x <- as.numeric(final_df_rf$field_x)
final_df_rf$field_y <- as.numeric(final_df_rf$field_y)

final_df_rf <- final_df_rf %>%
  arrange(game_str, play_id, timestamp) %>%
  group_by(game_str, play_id) %>%
    mutate(
      dtime = (timestamp - lag(timestamp)) / 1000,
      dtime = replace_na(dtime, 0)
    ) %>%
  ungroup()

table(final_df_rf$dtime)

final_df_rf <- final_df_rf %>%
  arrange(game_str, play_id, timestamp) %>%
  group_by(game_str, play_id) %>%
    fill(hang_time, .direction = "downup") %>%  
    mutate(
      cumulative_hangtime = cumsum(dtime),
      total_hangtime = sum(dtime, na.rm = TRUE),
      time_left = total_hangtime - cumulative_hangtime
    ) %>%
  ungroup()


final_df_rf <- final_df_rf %>%
  arrange(game_str, play_id, timestamp) %>%
  group_by(game_str, play_id) %>%
    fill(right_field, .direction = "downup") %>%
    fill(is_out, .direction = "downup") %>%
    fill(player_position.y, .direction = "downup")





final_df_rf <- final_df_rf %>%
  select(
    -at_bat,
    -play_per_game,
    -player_position.y,
    -game_str_play_id.x,
    -game_str_play_id.y,
    -valid_followup,
    -keep_row,
    -event_code  )

#Playing around with balltracking data -------------------------------------
ball_pos <- ball_pos_input %>%
  collect()

ball_pos <- select(ball_pos, game_str, play_id, timestamp, ball_position_x, ball_position_y, ball_position_z)
ball_pos$play_id <- as.integer(ball_pos$play_id)
ball_pos$timestamp <- as.integer(ball_pos$timestamp)
final_df_rf1 <- left_join(final_df_rf, ball_pos)

final_df_rf1 <- final_df_rf1 %>%
  group_by(game_str, play_id) %>%
    arrange(game_str, play_id, timestamp) %>%
    mutate(ball_landing_location_x = last(ball_position_x)) %>%
    mutate(ball_landing_location_y = last(ball_position_y)) %>%
    mutate(ball_landing_location_z = last(ball_position_z))

final_df_rf1$ball_landing_location_x <- as.numeric(final_df_rf1$ball_landing_location_x)
final_df_rf1$ball_landing_location_y <- as.numeric(final_df_rf1$ball_landing_location_y)
final_df_rf1$ball_landing_location_z <- as.numeric(final_df_rf1$ball_landing_location_z)

final_df_rf1 <- final_df_rf1 %>%
  mutate(
    distance_to_target = sqrt((ball_landing_location_x - field_x)^2 + 
                                (ball_landing_location_y - field_y)^2)
  )

final_df_rf1 <- final_df_rf1 %>%
  rename(player_code = right_field)



# Next loading in ---------------------------------------------------------


#reloading fresh copy in
game_events_loading_finished <- game_events_input %>%
  collect()

game_info_loading_finished <- game_info_input %>%
  collect()


#full process for player_pos, weirdness with earlier call:
player_pos_loading <- arrow::open_csv_dataset(paste0(data_directory,"/player_pos"),
                                              partitioning = c("HomeTeam", "AwayTeam", "Season", "Day"), 
                                              hive_style = FALSE, 
                                              unify_schemas = TRUE, 
                                              na = c("", "NA", "NULL", NA, "\\N"))



player_pos_loading_finished <- player_pos_loading %>%
  collect()



# Next section ------------------------------------------------------------



#GIVEN: RF data frame
#FIND: CF and LF initial positions 
#7 : LEFT FIELD # 8 : CENTER FIELD # 9: RIGHT FIELD

#KEEP 7s, replace 9s with 8s

#so I should have 7s 8s (LF and CF)


new_final_df_rf <- final_df_rf1 %>%
  group_by(game_str, play_id) %>%
    mutate(first_frame = ifelse(timestamp == min(timestamp), 1, 0 )) %>% #identifying first frame
    ungroup() %>%
    filter(first_frame == 1) %>%
    mutate(game_play = paste0(game_str, "_", play_id)) %>% #help me with my model_list since I'm bad at coding
    select(1:3)

other_new_final_df_rf <- final_df_rf1 %>%
  select(game_str, play_id, timestamp, player_position.x, field_x, field_y, player_code, is_out, hang_time, distance_to_target,
         ball_landing_location_x, ball_landing_location_y, ball_landing_location_z) %>%
  group_by(game_str, play_id) %>%
    mutate(first_frame = ifelse(timestamp == min(timestamp), 1, 0 )) %>% #identifying first frame
    ungroup() %>%
    filter(first_frame == 1) %>%
    select(-first_frame)



last_new_final_df_rf <- final_df_rf1 %>%
  select(game_str, play_id, timestamp, hang_time, 
         ball_landing_location_x, ball_landing_location_y, ball_landing_location_z) %>%
  group_by(game_str, play_id) %>%
    mutate(first_frame = ifelse(timestamp == min(timestamp), 1, 0 )) %>% #identifying first frame
    ungroup() %>%
    filter(first_frame == 1) %>%
    select(-first_frame)



###Game Info:
# - game_str, play_per_game, LF / CF player_id

game_info_1 <- game_info_loading_finished %>%
  select(game_str, play_per_game, center_field, left_field)


###Game Events:
# - game_str, play_per_game, play_id, player_position 

game_events_1 <- game_events_loading_finished %>%
  select(game_str, play_per_game, play_id, player_position)



#MERGE: Info and Events

info_events_merge_1 <- game_events_1 %>%
  left_join(game_info_1, join_by(game_str, play_per_game))


player_pos_1 <- player_pos_loading_finished %>%
  select(1:6) %>%
  filter(player_position == 7 | player_position == 8)


final_df_rf_playerpos_merge <- new_final_df_rf %>%
  left_join(player_pos_1, join_by(game_str, play_id, timestamp))

info_events_playerpos_merge_1 <- final_df_rf_playerpos_merge %>%
  left_join(info_events_merge_1, join_by(game_str, play_id)) %>%
  select(-`player_position.y`) %>%
  distinct() %>%
  mutate(player_code = ifelse(`player_position.x` == 7, left_field, center_field )) %>%
  select(-center_field, -left_field, -play_per_game) %>%
  mutate(is_out = 0) %>%
  left_join(last_new_final_df_rf, join_by(game_str, play_id, timestamp))


final_overall_everything_merge_rf <- bind_rows(
  
  info_events_playerpos_merge_1,
  other_new_final_df_rf
) %>%
  arrange(game_str, play_id, `player_position.x`)


write.csv(final_overall_everything_merge_rf, "final_merged_rf.csv", row.names = FALSE)


