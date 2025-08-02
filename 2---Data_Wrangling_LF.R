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
#Basic data wrangling-----------------------------------------------

game_events_bbip_lf <- game_events %>%
  filter(player_position == 7, event_code == 2) %>%
  distinct(game_str, play_id, .keep_all = TRUE)

#We only want game events where the center fielder picked up the ball
game_events_lf <- game_events %>%
  semi_join(game_events_bbip_lf, by = c("game_str", "play_id"))

#Getting plays where the center fielder caught the ball (out)
game_events_lf <- game_events_lf %>%
  arrange(game_str, play_per_game, timestamp) %>%
  group_by(game_str, play_per_game) %>%
    mutate(
      is_out = as.integer(
        any(player_position == 7 & event_code == 2 &
              lag(player_position) == 10 & lag(event_code) == 4, na.rm = TRUE)
      )
    ) %>%
  ungroup()

#Load in game info
game_info <- as.data.frame(game_info)

#Join game info with game events to get player names
players_lf_fly_balls <- game_events_lf %>%
  
  left_join(game_info, by = join_by("game_str", "at_bat", "play_per_game", 
                                    "Season", "HomeTeam","AwayTeam", "Day")) %>%
  select(game_str, play_id, at_bat, play_per_game, timestamp, 
         player_position, event_code, left_field, is_out) 

players_lf_touched <- players_lf_fly_balls %>%
  filter(!is.na(left_field))


#Get hang time
players_lf_touched <- players_lf_touched %>%
  arrange(game_str, play_id, timestamp) %>%
  group_by(game_str, play_id) %>%
    mutate(
      hang_time = ifelse(
        lag(player_position) == 10 & lag(event_code) == 4 & player_position == 7 & event_code == 2,
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


players_lf_touched <- players_lf_touched %>%
  filter(!is.na(hang_time))

#A sequence of code that only keeps the timestamps where the ball hit the ground or 
players_lf_transitions <- players_lf_touched %>%
  arrange(game_str, play_id, timestamp) %>%
  group_by(game_str, play_id) %>%
    mutate(
      valid_followup = lag(player_position) == 10 & lag(event_code) == 4 &
        (
          (player_position == 7 & event_code == 2) |
            (player_position == 255 & event_code == 16) |
            (event_code %in% c(9, 10))
        )
    ) %>%
    mutate(
      keep_row = valid_followup | lead(valid_followup)
    ) %>%
  ungroup() %>%
  filter(keep_row)

players_lf_transitions <- players_lf_transitions %>%
  mutate(game_str_play_id = paste(game_str, play_id, sep = "_"))

#Player position data --------------------------------------------------
players_lf_pos <- player_pos_input %>%
  filter(player_position == 7) %>%
  select(-c("Season", "HomeTeam", "AwayTeam", "Day")) %>%
  collect()

players_lf_pos <- players_lf_pos %>%
  mutate(game_str_play_id = paste(game_str, play_id, sep = "_"))

players_lf_pos <- filter(players_lf_pos, game_str_play_id %in% players_lf_transitions$game_str_play_id)

table(players_lf_pos$game_str)
players_lf_pos$play_id <- as.integer(players_lf_pos$play_id)
players_lf_pos$timestamp <- as.integer(players_lf_pos$timestamp)
players_lf_pos$player_position <- as.integer(players_lf_pos$player_position)


#Start/stop times and interpolation-----------------------------------

player_pos_game_events_lf <- left_join(players_lf_pos, players_lf_transitions, by = c("game_str", "play_id", "timestamp"))
player_pos_game_events_lf$start_stop <- ifelse(!(is.na(player_pos_game_events_lf$player_position.y)), 1, 0)

start_stop_times_lf <- player_pos_game_events_lf %>%
  group_by(game_str, play_id) %>%
    filter(start_stop == 1) %>%
    summarise(
      start_time = min(timestamp),
      stop_time  = max(timestamp),
      .groups = "drop"
    )

lf_interpolated <- player_pos_game_events_lf %>%
  left_join(start_stop_times_lf, by = c("game_str", "play_id")) %>%
  mutate(
    between_flag = if_else(timestamp >= start_time & timestamp <= stop_time, 1L, NA_integer_)
  ) %>%
  select(-start_time, -stop_time)



final_df_lf <- filter(lf_interpolated, between_flag == 1)

#Getting hangtime and more data prep ----------------------------------
final_df_lf$field_x <- as.numeric(final_df_lf$field_x)
final_df_lf$field_y <- as.numeric(final_df_lf$field_y)

final_df_lf <- final_df_lf %>%
  arrange(game_str, play_id, timestamp) %>%
    group_by(game_str, play_id) %>%
    mutate(
      dtime = (timestamp - lag(timestamp)) / 1000,
      dtime = replace_na(dtime, 0)
    ) %>%
  ungroup()

table(final_df_lf$dtime)

final_df_lf <- final_df_lf %>%
  arrange(game_str, play_id, timestamp) %>%
    group_by(game_str, play_id) %>%
      fill(hang_time, .direction = "downup") %>%  # fill missing hang_time both down and up
      mutate(
        cumulative_hangtime = cumsum(dtime),
        total_hangtime = sum(dtime, na.rm = TRUE),
        time_left = total_hangtime - cumulative_hangtime
      ) %>%
  ungroup()


final_df_lf <- final_df_lf %>%
  arrange(game_str, play_id, timestamp) %>%
    group_by(game_str, play_id) %>%
      fill(left_field, .direction = "downup") %>%
      fill(is_out, .direction = "downup") %>%
      fill(player_position.y, .direction = "downup")

final_df_lf <- final_df_lf %>%
  select(
    -at_bat,
    -play_per_game,
    -player_position.y,
    -game_str_play_id.x,
    -game_str_play_id.y,
    -valid_followup,
    -keep_row,
    -event_code  )

#Ball position data--------------------------------------------------
ball_pos <- ball_pos_input %>%
  collect()

ball_pos <- select(ball_pos, game_str, play_id, timestamp, ball_position_x, ball_position_y, ball_position_z)
ball_pos$play_id <- as.integer(ball_pos$play_id)
ball_pos$timestamp <- as.integer(ball_pos$timestamp)
final_df_lf1 <- left_join(final_df_lf, ball_pos)

final_df_lf1 <- final_df_lf1 %>%
  group_by(game_str, play_id) %>%
    arrange(game_str, play_id, timestamp) %>%
    mutate(ball_landing_location_x = last(ball_position_x)) %>%
    mutate(ball_landing_location_y = last(ball_position_y)) %>%
    mutate(ball_landing_location_z = last(ball_position_z))

final_df_lf1$ball_landing_location_x <- as.numeric(final_df_lf1$ball_landing_location_x)
final_df_lf1$ball_landing_location_y <- as.numeric(final_df_lf1$ball_landing_location_y)
final_df_lf1$ball_landing_location_z <- as.numeric(final_df_lf1$ball_landing_location_z)

final_df_lf1 <- final_df_lf1 %>%
  mutate(
    distance_to_target = sqrt((ball_landing_location_x - field_x)^2 + 
                                (ball_landing_location_y - field_y)^2)
  )

final_df_lf1 <- final_df_lf1 %>%
  rename(player_code = left_field)




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

#KEEP 8s, replace 7s with 9s

new_final_df_lf <- final_df_lf1 %>%
  group_by(game_str, play_id) %>%
    mutate(first_frame = ifelse(timestamp == min(timestamp), 1, 0 )) %>% #identifying first frame
    ungroup() %>%
    filter(first_frame == 1) %>%
    mutate(game_play = paste0(game_str, "_", play_id)) %>% #help me with my model_list since I'm bad at coding
    select(1:3)

other_new_final_df_lf <- final_df_lf1 %>%
  select(game_str, play_id, timestamp, player_position.x, field_x, field_y, player_code, is_out, hang_time, distance_to_target,
         ball_landing_location_x, ball_landing_location_y, ball_landing_location_z) %>%
  group_by(game_str, play_id) %>%
    mutate(first_frame = ifelse(timestamp == min(timestamp), 1, 0 )) %>% #identifying first frame
  ungroup() %>%
  filter(first_frame == 1) %>%
  select(-first_frame)



last_new_final_df_lf <- final_df_lf1 %>%
  select(game_str, play_id, timestamp, hang_time, 
         ball_landing_location_x, ball_landing_location_y, ball_landing_location_z) %>%
  group_by(game_str, play_id) %>%
  mutate(first_frame = ifelse(timestamp == min(timestamp), 1, 0 )) %>% #identifying first frame
  ungroup() %>%
  filter(first_frame == 1) %>%
  select(-first_frame)



###Game Info:
# - game_str, play_per_game, CF / RF player_id

game_info_1 <- game_info_loading_finished %>%
  select(game_str, play_per_game, center_field, right_field)


###Game Events:
# - game_str, play_per_game, play_id, player_position 

game_events_1 <- game_events_loading_finished %>%
  select(game_str, play_per_game, play_id, player_position)



#MERGE: Info and Events

info_events_merge_1 <- game_events_1 %>%
  left_join(game_info_1, join_by(game_str, play_per_game))


player_pos_1 <- player_pos_loading_finished %>%
  select(1:6) %>%
  filter(player_position == 8 | player_position == 9)


final_df_lf_playerpos_merge <- new_final_df_lf %>%
  left_join(player_pos_1, join_by(game_str, play_id, timestamp))

info_events_playerpos_merge_1 <- final_df_lf_playerpos_merge %>%
  left_join(info_events_merge_1, join_by(game_str, play_id)) %>%
  select(-`player_position.y`) %>%
  distinct() %>%
  mutate(player_code = ifelse(`player_position.x` == 8, center_field, right_field )) %>%
  select(-center_field, -right_field, -play_per_game) %>%
  mutate(is_out = 0) %>%
  left_join(last_new_final_df_lf, join_by(game_str, play_id, timestamp))


final_overall_everything_merge_lf <- bind_rows(
  
  info_events_playerpos_merge_1,
  other_new_final_df_lf
) %>%
  arrange(game_str, play_id, `player_position.x`)

final_overall_everything_merge_lf <- final_overall_everything_merge_lf %>%
  mutate(attempted_catch = ifelse(player_position.x == 7, 1, 0))
write.csv(final_overall_everything_merge_lf, "final_merged_lf.csv", row.names = FALSE)




