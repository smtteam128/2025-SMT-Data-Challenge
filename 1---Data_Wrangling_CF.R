#---------------THIS IS THE FIRST R SCRIPT---------------------------
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
#Beginning the wrangling process----------------------------------------
# Step 1: Get CF plays (position 8, event_code 2)
game_events_bbip_cf <- game_events %>%
  filter(player_position == 8 & event_code == 2) %>%
  distinct(game_str, play_id, .keep_all = TRUE)

#We only want game events where the center fielder picked up the ball
game_events_cf <- game_events %>%
  semi_join(., game_events_bbip_cf, by = c("game_str", "play_id"))

#Getting plays where the center fielder caught the ball (out)
game_events_cf <- game_events_cf %>%
  arrange(game_str, play_per_game, timestamp) %>%
  group_by(game_str, play_per_game) %>%
   mutate(
      is_out = as.integer(
       any(player_position == 8 & event_code == 2 &
              lag(player_position) == 10 & lag(event_code) == 4, na.rm = TRUE)
      )
    ) %>%
  ungroup()

#Load in game info
game_info <- as.data.frame(game_info)

#Join game info with game events to get player names
players_cf_fly_balls <- game_events_cf %>%
  left_join(., game_info, by = join_by("game_str", "at_bat", "play_per_game", 
                                    "Season", "HomeTeam","AwayTeam", "Day")) %>%
  select(game_str, play_id, at_bat, play_per_game, timestamp, 
         player_position, event_code, center_field, is_out) 

players_cf_touched <- players_cf_fly_balls %>%
  filter(!is.na(center_field))

#Get hang time
players_cf_touched <- players_cf_touched %>%
  arrange(game_str, play_id, timestamp) %>%
  group_by(game_str, play_id) %>%
    mutate(
      hang_time = ifelse(
        lag(player_position) == 10 & lag(event_code) == 4 & player_position == 8 & event_code == 2,
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
    fill(hang_time, .direction = "downup") %>%  
  ungroup()


players_cf_touched <- players_cf_touched %>%
  filter(!is.na(hang_time))

#A sequence of code that only keeps the timestamps where the ball hit the ground or was caught or deflected 
players_cf_transitions <- players_cf_touched %>%
  arrange(game_str, play_id, timestamp) %>%
  group_by(game_str, play_id) %>%
  #We have all plays where the CF touched the ball, but we only want to include the
  #Interval between when the ball was hit off the bat and when the ball bounces.
  #Let's filter the data to only include these two plays
    mutate(
      valid_followup = lag(player_position) == 10 & lag(event_code) == 4 & #Code for ball hit by batter
        (
          (player_position == 8 & event_code == 2) | #CF caught the ball
            (player_position == 255 & event_code == 16) | #Ball bounced
            (event_code %in% c(9, 10)) #Ball deflected off a player or off the wall  
        )
    ) %>%
    mutate(
      keep_row = valid_followup | lead(valid_followup) 
    ) %>%
  ungroup() %>%
  filter(keep_row)

#An easy way for us to join everything together
players_cf_transitions <- players_cf_transitions %>%
  mutate(game_str_play_id = paste(game_str, play_id, sep = "_"))

#Working with player position data-------------------------------------------
#From all CF player position data take the CF data
players_cf_pos <- player_pos_input %>%
  filter(player_position == 8) %>%
  select(-c("Season", "HomeTeam", "AwayTeam", "Day")) %>%
  collect()

#Make a unique game string/player id combo that makes it easier to join data
players_cf_pos <- players_cf_pos %>%
  mutate(game_str_play_id = paste(game_str, play_id, sep = "_"))

#Only get player positions on plays where the CF touches the ball
players_cf_pos <- filter(players_cf_pos, game_str_play_id 
                         %in% players_cf_transitions$game_str_play_id)

#Converting variables to integers
players_cf_pos$play_id <- as.integer(players_cf_pos$play_id)
players_cf_pos$timestamp <- as.integer(players_cf_pos$timestamp)
players_cf_pos$player_position <- as.integer(players_cf_pos$player_position)



#Join player position data with the player events data we were cultivating earlier
player_pos_game_events_cf <- left_join(players_cf_pos, 
                                       players_cf_transitions, 
                                       by = c("game_str", "play_id", "timestamp"))

#Since player position data gets us everything in between the two events, we need
#to identify the start of our play and the end of the play (what we actually care about)
player_pos_game_events_cf$start_stop <- ifelse(!(is.na(player_pos_game_events_cf$player_position.y)), 
                                               1, 0)

start_stop_times_cf <- player_pos_game_events_cf %>%
  group_by(game_str, play_id) %>%
    filter(start_stop == 1) %>%
    summarise(
      start_time = min(timestamp),
      stop_time  = max(timestamp),
      .groups = "drop"
    )

#Now interpolate all values such that every frame between ball hit and ball landing is a 1
cf_interpolated <- player_pos_game_events_cf %>%
  left_join(., start_stop_times_cf, by = c("game_str", "play_id")) %>%
  mutate(
    between_flag = if_else(timestamp >= start_time & timestamp <= stop_time, 1L, NA_integer_)
  ) %>%
  select(-start_time, -stop_time)


#And remove all other values
final_df_cf <- filter(cf_interpolated, between_flag == 1)
#Getting hangtime and preparing the dataframe ----------------------------
#Convert coordinates to numeric
final_df_cf$field_x <- as.numeric(final_df_cf$field_x)
final_df_cf$field_y <- as.numeric(final_df_cf$field_y)

#Get difference in time
final_df_cf <- final_df_cf %>%
  arrange(game_str, play_id, timestamp) %>%
  group_by(game_str, play_id) %>%
  mutate(
    dtime = (timestamp - lag(timestamp)) / 1000,
    dtime = replace_na(dtime, 0)
  ) %>%
  ungroup()

table(final_df_cf$dtime)

#Calculate hangtime
final_df_cf <- final_df_cf %>%
  arrange(game_str, play_id, timestamp) %>%
  group_by(game_str, play_id) %>%
    #Fill missing hang_time both down and up
    fill(hang_time, .direction = "downup") %>%  
    mutate(
      cumulative_hangtime = cumsum(dtime),
      total_hangtime = sum(dtime, na.rm = TRUE),
      time_left = total_hangtime - cumulative_hangtime
    ) %>%
  ungroup()

#Interpolate some values that were missing due to the nature of our join function
final_df_cf <- final_df_cf %>%
  arrange(game_str, play_id, timestamp) %>%
  group_by(game_str, play_id) %>%
    fill(center_field, .direction = "downup") %>%
    fill(is_out, .direction = "downup") %>%
    fill(player_position.y, .direction = "downup")


#Remove variables we don't care about
final_df_cf <- final_df_cf %>%
  select(
    -at_bat,
    -play_per_game,
    -player_position.y,
    -game_str_play_id.x,
    -game_str_play_id.y,
    -valid_followup,
    -keep_row,
    -event_code)

#Loading in ball position data ---------------------------------------------
#Grab our ball position data
ball_pos <- ball_pos_input %>%
  collect()

#Select the variables we want
ball_pos <- select(ball_pos, game_str, play_id, timestamp, ball_position_x, 
                   ball_position_y, ball_position_z)
ball_pos$play_id <- as.integer(ball_pos$play_id)
ball_pos$timestamp <- as.integer(ball_pos$timestamp)

#Join our ball position data with our current data
final_df_cf1 <- left_join(final_df_cf, ball_pos) #TODO - Add by statement

#Get the ball landing location
final_df_cf1 <- final_df_cf1 %>%
  group_by(game_str, play_id) %>%
    arrange(game_str, play_id, timestamp) %>%
    mutate(ball_landing_location_x = as.numeric(last(ball_position_x))) %>%
    mutate(ball_landing_location_y = as.numeric(last(ball_position_y))) %>%
    mutate(ball_landing_location_z = as.numeric(last(ball_position_z)))

#And the distance to target
final_df_cf1 <- final_df_cf1 %>%
  mutate(
    distance_to_target = sqrt((ball_landing_location_x - field_x)^2 + 
                                (ball_landing_location_y - field_y)^2)
  )

#To make this left-joinable
final_df_cf1 <- final_df_cf1 %>%
  rename(player_code = center_field)


# Next loading in ---------------------------------------------------------
#The next part of this script will be joining in the right/left fielder positions 
#on all plays where the center fielder was involved.

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
  collect() #Hefty dataset. Delete when done




# Next section ------------------------------------------------------------ 



#GIVEN: CF data frame
#FIND: LF and RF initial positions 

#Getting the timestamp of the first frame
new_final_df_cf <- final_df_cf1 %>%
  group_by(game_str, play_id) %>%
  mutate(first_frame = ifelse(timestamp == min(timestamp), 1, 0 )) %>% #identifying first frame
  ungroup() %>%
  filter(first_frame == 1) %>%
  mutate(game_play = paste0(game_str, "_", play_id)) %>% #Our easy game_str/play_id identifier
  select(1:3)

other_new_final_df_cf <- final_df_cf1 %>%
  select(game_str, play_id, timestamp, player_position.x, field_x, field_y, player_code, 
         is_out, hang_time, distance_to_target,
         ball_landing_location_x, ball_landing_location_y, ball_landing_location_z) %>%
  group_by(game_str, play_id) %>%
  mutate(first_frame = ifelse(timestamp == min(timestamp), 1, 0 )) %>% #identifying first frame
  ungroup() %>%
  filter(first_frame == 1) %>%
  select(-first_frame)



last_new_final_df_cf <- final_df_cf1 %>%
  select(game_str, play_id, timestamp, hang_time, 
         ball_landing_location_x, ball_landing_location_y, ball_landing_location_z) %>%
  group_by(game_str, play_id) %>%
  mutate(first_frame = ifelse(timestamp == min(timestamp), 1, 0 )) %>% #identifying first frame
  ungroup() %>%
  filter(first_frame == 1) %>%
  select(-first_frame)

#Game Info:
# - game_str, play_per_game, RF / LF player_id

game_info_1 <- game_info_loading_finished %>%
  select(game_str, play_per_game, right_field, left_field)


#Game Events:
# - game_str, play_per_game, play_id, player_position 

game_events_1 <- game_events_loading_finished %>%
  select(game_str, play_per_game, play_id, player_position)



#MERGE: Info and Events

info_events_merge_1 <- game_events_1 %>%
  left_join(game_info_1, join_by(game_str, play_per_game))

#Select only the right and left fielders
player_pos_1 <- player_pos_loading_finished %>%
  select(1:6) %>% 
  filter(player_position == 7 | player_position == 9)

final_df_cf_playerpos_merge <- new_final_df_cf %>%
  left_join(., player_pos_1, join_by(game_str, play_id, timestamp))

#Merge game info with our existing dataset
info_events_playerpos_merge_1 <- final_df_cf_playerpos_merge %>%
  left_join(., info_events_merge_1, join_by(game_str, play_id)) %>%
  select(-`player_position.y`) %>%
  distinct() %>%
  mutate(player_code = ifelse(`player_position.x` == 7, left_field, right_field )) %>%
  select(-right_field, -left_field, -play_per_game) %>%
  mutate(is_out = 0) %>%
  left_join(last_new_final_df_cf, join_by(game_str, play_id, timestamp))


final_overall_everything_merge_cf <- bind_rows(
  info_events_playerpos_merge_1,
  other_new_final_df_cf) %>%
  arrange(game_str, play_id, `player_position.x`)

#Get plays when a player attempted the catch (since this is of CF attempts it's easy)
final_overall_everything_merge_cf <- final_overall_everything_merge_cf %>%
  mutate(attempted_catch = ifelse(player_position.x == 8, 1, 0))
write.csv(final_overall_everything_merge_cf, "final_merged_cf.csv", 
          row.names = FALSE)



