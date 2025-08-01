import numpy as np
import pandas as pd
import matplotlib.pyplot as plt
import math
from pathlib import Path

#Load in our data
def load_data():
    full_ball_pos_df = pd.read_csv("ball_pos_full_data.csv")
    full_player_pos_df = pd.read_csv("player_pos_full_data.csv")
    full_game_events_df = pd.read_csv("game_events_full_data.csv")
    full_game_info_df = pd.read_csv("game_info_full_data.csv")
    return full_ball_pos_df, full_player_pos_df, full_game_events_df, full_game_info_df

def load_data_by_game(full_ball_pos_df, full_player_pos_df, full_game_events_df, full_game_info_df):
    game_strings = full_ball_pos_df["game_str"].unique()
    for game_str in game_strings:
        yield (
            game_str,
            full_ball_pos_df[full_ball_pos_df["game_str"] == game_str],
            full_player_pos_df[full_player_pos_df["game_str"] == game_str],
            full_game_events_df[full_game_events_df["game_str"] == game_str],
            full_game_info_df[full_game_info_df["game_str"] == game_str]
        )

#Look at plays where the ball was hit into play
def find_contact_timestamp(game_events_df):
    bip_df = game_events_df[game_events_df["event_code"] == 4]
    return bip_df["timestamp"].tolist()

#Write a definition for tracking hitter motion
def track_hitter_motion(player_pos_df, game_events_df, game_info_df, current_timestamp, window_ms=5000, speed_threshold=1, starting_speed=3.5):

    ts = current_timestamp
#Using the player position data, let's only take the hitter within that timeframe
    hitter_df = player_pos_df[
            (player_pos_df["player_position"] == 10) &
            (player_pos_df["timestamp"] >= ts) &
            (player_pos_df["timestamp"] <= ts + window_ms)
    ].copy()


    hitter_df = hitter_df.sort_values("timestamp")
    hitter_df = hitter_df.reset_index(drop=True)

    if hitter_df.empty: #Check if we actually have hitter data
        print(f"No hitter data for timestamp {ts}")
        print(f"Yikes")
        return
#Extract game and play id
    game_str = hitter_df["game_str"].iloc[0]
    play_id = hitter_df["play_id"].iloc[0]

#Get play number and match it with game info
    play_per_game = game_events_df[
                        (game_events_df["play_id"] == play_id) & 
                        (game_events_df["game_str"] == game_str)
                    ]["play_per_game"].iloc[0]

    info_match = game_info_df[
                    (game_info_df["play_per_game"] == play_per_game) &
                    (game_info_df["game_str"] == game_str)
                ]

    if info_match.empty:
        print(f"No game_info match for game_str={game_str}, play_per_game={play_per_game}")
        return
 #Get player information           
    hitter_id = game_info_df[
                    (game_info_df["play_per_game"] == play_per_game) & 
                    (game_info_df["game_str"] == game_str)
                ]["batter"].iloc[0]
#Calculate the motion metrics
    hitter_df["dt"] = ((hitter_df["timestamp"].diff()) / 1000)
    hitter_df["dx"] = hitter_df["field_x"].diff()
    hitter_df["dy"] = hitter_df["field_y"].diff()
#Calculate distance and speed
    hitter_df["distance"] = np.sqrt(hitter_df["dx"]**2 + hitter_df["dy"]**2)
    hitter_df["speed"] = hitter_df["distance"] / hitter_df["dt"]
    hitter_df["speed_smooth"] = hitter_df["speed"].rolling(window=3, center=True).mean()


    sprint_start_candidates = hitter_df[hitter_df["speed_smooth"] >= starting_speed]

    if sprint_start_candidates.empty:
        max_ = hitter_df["speed_smooth"].max()
        print(f"Hitter Never Started Sprinting. Top speed was: {max_}")
        return 
#Get the sprint start time
    sprint_start_timestamp = sprint_start_candidates["timestamp"].iloc[0]
    sprint_start_idx = hitter_df[hitter_df["timestamp"] == sprint_start_timestamp].index[0]

    starting_x_pos = hitter_df.loc[sprint_start_idx, "field_x"]
    starting_y_pos = hitter_df.loc[sprint_start_idx, "field_y"]
#Find max speed

    max_speed = hitter_df["speed_smooth"].max()
    max_speed_index = hitter_df["speed_smooth"].idxmax()
    max_speed_timestamp = hitter_df.loc[max_speed_index, "timestamp"]
#Check our work
    prev_index, prev_prev_index = max_speed_index - 1, max_speed_index - 2

    if (prev_index < 0) or (prev_prev_index < 0):
        print("No Indeces Surrounding Max Speed Index")
        return
    
    prev_frame_speed = hitter_df.loc[prev_index, "speed_smooth"]
    prev_prev_frame_speed = hitter_df.loc[prev_prev_index, "speed_smooth"]
#Validate outliers
    if (max_speed > prev_frame_speed + speed_threshold) or (max_speed > prev_prev_frame_speed + speed_threshold):
        print("Max Speed Outside of speed threshold")
        return

    print(f"Max Speed: {max_speed}, Prior Frame Speed: {prev_frame_speed}, Prior Prior Frame Speed: {prev_prev_frame_speed}")
    max_speed_range = (max_speed + prev_frame_speed + prev_prev_frame_speed) / 3
    time_to_max_speed = (max_speed_timestamp - sprint_start_timestamp)/1000
    max_speed_x_pos = hitter_df.loc[max_speed_index, "field_x"]
    max_speed_y_pos = hitter_df.loc[max_speed_index, "field_y"]

    distance_covered = np.sqrt((max_speed_x_pos - starting_x_pos)**2 + (max_speed_y_pos - starting_y_pos)**2)

    sprint_path = hitter_df.loc[sprint_start_idx:max_speed_index].copy()

    path_distance = sprint_path["distance"].sum()

    if distance_covered / path_distance < 0.9:
        print("Hitter did not run in a straight line.")
        return

    return max_speed_range, time_to_max_speed, path_distance, hitter_id, game_str
#Now that we have max speeds, let's get the 60-yard dashes - Use max speed range, time to max speed, and distance covered.
def calc_projected_60_yard_dash(max_speed_range, time_to_max_speed, distance_covered):
    remaining_dist = 180 - distance_covered
    remaining_time = remaining_dist / max_speed_range
    proj_60_time = time_to_max_speed + remaining_time
    return proj_60_time



def main():
    full_ball_pos_df, full_player_pos_df, full_game_events_df, full_game_info_df = load_data()

    full_results = []

    for game_str, ball_pos_df, player_pos_df, game_events_df, game_info_df in load_data_by_game(full_ball_pos_df, full_player_pos_df, full_game_events_df, full_game_info_df):
        print(f"Parsing Game: {game_str}")
        bip_timestamps = find_contact_timestamp(game_events_df)

        all_results_for_game = []
#Process each game
        for i, ts in enumerate(bip_timestamps):
            print(f"\nProcessing Play {i+1} (Timestamp: {ts})")
            result = track_hitter_motion(player_pos_df, game_events_df, game_info_df, ts, window_ms=5000, speed_threshold=1, starting_speed=3.5)

            if result:
                max_speed_range, time_to_max_speed, distance_covered, hitter_id, game_str_other = result
#Sanity check
                if game_str != game_str_other:
                    print("Yikes")
#Calculate 60-yard time
                proj_60_time = calc_projected_60_yard_dash(max_speed_range, time_to_max_speed, distance_covered)
                print(f"Projected 60: {proj_60_time}, Max Speed: {max_speed_range}, Time to Max Speed: {time_to_max_speed}, Distance Covered: {distance_covered}")
#Store results
                all_results_for_game.append({
                    'Play_Index': i,
                    'Contact_Timestamp': ts,
                    'Max_Speed_Range': max_speed_range,
                    'Time_to_Max_Speed_s': time_to_max_speed,
                    'Distance_Covered_ft': distance_covered,
                    'Projected_60_Yard_Time_s': proj_60_time,
                    'Hitter ID': hitter_id,
                    'Game String': game_str
                    })

            else:
                print(f"Skipping Play {i+1} of {game_str} due to conditions not met.")
#Save results
        if all_results_for_game:
            final_summary_df = pd.DataFrame(all_results_for_game)
            print(f"\n------- Final Summary DataFrame For {game_str} -------")
            print(final_summary_df)

            full_results.append(final_summary_df)

            Path("Projected_60_By_Game").mkdir(parents=True, exist_ok=True)
            final_summary_df.to_csv(f"Projected_60_By_Game/{game_str}_projected_60_yard_dash.csv")

        else:
            print("\nNo valid plays processed to build a final summary DataFrame.")

    Path("Final_Projected_60_Data").mkdir(parents=True, exist_ok=True)
    if full_results:
        all_games_df = pd.concat(full_results, ignore_index=True)
        all_games_df.to_csv("Final_Projected_60_Data/full_projected_60_yard_dash.csv", index=False)


#Run it
if __name__ == "__main__":
    main()
