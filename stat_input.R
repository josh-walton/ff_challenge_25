
source("global.R")

# Read Lineup Submissions ####
# By most recent submission
submissions <- read_sheet(
  ss = sheet_url,
  sheet = "Lineups"
) %>%
  mutate(
    timestamp = ymd_hms(timestamp)
  ) %>%
arrange(manager_full_name, desc(timestamp)) %>%
  distinct(manager_full_name, playoff_round, .keep_all = TRUE)

# Pivot Data
lineups_long <- submissions %>%
  pivot_longer(
    cols = c(qb, rb1, rb2, wr1, wr2, te, flex, k, def),
    names_to = "slot",
    values_to = "player_w_team"
  )

# Format player names
lineups_long <- lineups_long %>%
  separate(
    player_w_team,
    into = c("player", "team"),
    sep = " - "
  )

## Gathering Stats per Player ####

# Player IDs and names for later join
player_info <- load_players() %>% 
  filter(last_season == "2025") %>% 
  select(gsis_id, display_name) %>% 
  mutate(clean_name = clean_player_names(display_name)) %>% 
  select(-display_name)

# Play by play data for 2025 #
pbp_data <- load_pbp(2025)

current_round_number <- round_windows %>% 
  filter(Sys.Date() > open_time) %>% 
  pull(week)

week_data <- pbp_data %>% 
  filter(week == current_round_number)

# Fantasy points per QB (passer)
passing_stats <- week_data %>% 
  filter(play_type %in% c("run", "pass")) %>%
  group_by(passer_id) %>%
  reframe(f_passing_yards = sum(passing_yards, na.rm = T) * 0.04,
          passing_yards = sum(passing_yards, na.rm = T),
          scramble_yards = sum(if_else(qb_scramble == 1, rushing_yards, 0), na.rm = TRUE),
          f_scramble_yards = sum(if_else(qb_scramble == 1, rushing_yards, 0), na.rm = TRUE) * 0.1,
          f_pass_tds = sum(pass_touchdown, na.rm = T) * 6,
          pass_tds = sum(pass_touchdown, na.rm = T),
          pass_bonus = sum(yards_gained >= 40, na.rm = TRUE),
          f_pass_bonus = sum(yards_gained >= 40, na.rm = TRUE) *2,
          f_ints = sum(interception, na.rm = T) * -2,
          ints = sum(interception, na.rm = T),
          pass_fumbles = sum(if_else(is.na(receiver_player_name) & !is.na(passer_player_name), fumble_lost, 0), na.rm = T),
          f_pass_fumbles = sum(pass_fumbles, na.rm = T) * -2,
          f_pass_two_pts = sum(two_point_conv_result == "success", na.rm = T) * 2,
          pass_two_pts = sum(two_point_conv_result == "success", na.rm = T)
  ) %>% 
  mutate(f_total_pass_points = f_passing_yards + f_scramble_yards + f_pass_tds + 
           f_pass_bonus + f_ints + 
           f_pass_fumbles + f_pass_two_pts) %>% 
  rename(gsis_id = passer_id) %>% 
  drop_na()

# Fantasy points per RB
rushing_stats <- week_data %>% 
  filter(play_type %in% c("run", "qb_kneel")) %>%
  group_by(rusher_id) %>% 
  reframe(f_rush_fumbles = sum(fumble_lost, na.rm = T) * -2,
          rush_fumbles = sum(fumble_lost, na.rm = T),
          f_rush_tds = sum(rush_touchdown, na.rm = T) * 6,
          rush_tds = sum(rush_touchdown, na.rm = T),
          rush_bonus = sum(yards_gained >= 40, na.rm = TRUE),
          f_rush_bonus = sum(yards_gained >= 40, na.rm = TRUE) *2,
          f_rushing_yds = sum(rushing_yards, na.rm = T) * 0.1,
          rushing_yds = sum(rushing_yards, na.rm = T),
          f_run_two_pts = sum(two_point_conv_result == "success", na.rm = T) * 2,
          run_two_pts = sum(two_point_conv_result == "success", na.rm = T)
  ) %>% 
  mutate(f_total_rush_points = f_rushing_yds + f_rush_tds + 
           f_rush_bonus + f_rush_fumbles + f_run_two_pts) %>% 
  rename(gsis_id = rusher_id) %>% 
  drop_na()

# Fantasy points per WR
receiving_stats <- week_data %>% 
  filter(play_type == "pass") %>%
  group_by(receiver_player_id) %>% 
  reframe(f_rec_tds = sum(pass_touchdown, na.rm = T) * 6,
          rec_tds = sum(pass_touchdown, na.rm = T),
          f_rec_fumbles = sum(fumble_lost, na.rm = T) * -2,
          rec_fumbles = sum(fumble_lost, na.rm = T),
          receiver_bonus = sum(yards_gained >= 40, na.rm = TRUE),
          f_receiver_bonus = sum(yards_gained >= 40, na.rm = TRUE) *2,
          f_rec_two_pts = sum(two_point_conv_result == "success", na.rm = T) * 2,
          rec_two_pts = sum(two_point_conv_result == "success", na.rm = T),
          f_rec = sum(complete_pass, na.rm = T) * 0.5,
          rec = sum(complete_pass, na.rm = T),
          f_rec_yards = sum(receiving_yards, na.rm = T) * 0.1,
          rec_yards = sum(receiving_yards, na.rm = T)) %>% 
  mutate(f_total_rec_points = f_rec_yards + f_rec + f_rec_tds + 
           f_receiver_bonus + f_rec_fumbles + f_rec_two_pts) %>% 
  rename(gsis_id = receiver_player_id) %>% 
  drop_na()

# Kicker
k_stats <- week_data %>%
  filter(play_type %in% c("extra_point", "field_goal")) %>% 
  group_by(kicker_player_id) %>%
  reframe(f_xp = sum(extra_point_result == "good", na.rm = T),
          f_fg_u40 = sum(field_goal_result == "made" & kick_distance < 40, na.rm = T) * 3,
          f_fg_4049 = sum(field_goal_result == "made" & kick_distance >39 & kick_distance <=49, na.rm = T) * 4,
          f_fg_5059 = sum(field_goal_result == "made" & kick_distance >50, na.rm = T) * 5,
          f_total_kicker_points = f_xp + f_fg_u40 + f_fg_4049 + f_fg_5059) %>% 
  rename(gsis_id = kicker_player_id)

# Defense
def_stats <- week_data %>%  
  group_by(defteam) %>% 
  reframe(f_int = sum(interception, na.rm = T) *2,
          f_sack = sum(sack, na.rm = T),
          f_safety = sum(safety, na.rm = T) * 4,
          f_fumble = sum(fumble_lost, na.rm = T) * 2,
          f_punt_block = sum(punt_blocked, na.rm = T) * 4,
          f_fg_block = sum(field_goal_result == "blocked", na.rm = T) * 4,
          f_pts_allowed = if_else(max(posteam_score_post) == 0, 10,
                                  if_else(max(posteam_score_post) <= 6, 7,
                                          if_else(max(posteam_score_post) <= 13, 4,
                                                  if_else(max(posteam_score_post) <= 20, 2,
                                                          if_else(max(posteam_score_post) <= 27, 0,
                                                                  if_else(max(posteam_score_post) <= 34, -1,
                                                                          -4)))))),
          f_total_def_points = f_int + f_sack + f_safety + f_fumble + f_punt_block + f_pts_allowed) %>% 
  filter(!is.na(defteam))

# Player Summary
joined_stats <- passing_stats %>% 
  full_join(rushing_stats, by = join_by(gsis_id)) %>% 
  full_join(receiving_stats, by = join_by(gsis_id)) %>% 
  full_join(k_stats, by = join_by(gsis_id))

joined_stats[is.na(joined_stats)] <- 0

raw_f_stats <- joined_stats %>% 
  left_join(player_info, by = join_by(gsis_id)) %>% 
  mutate(total_f_points = f_total_pass_points + f_total_rush_points + f_total_rec_points + f_total_kicker_points) %>% 
  select(gsis_id, player_name = clean_name, total_f_points) %>% 
  arrange(desc(total_f_points))

raw_player_stats_reference <- joined_stats %>% 
  left_join(player_info, by = join_by(gsis_id)) %>% 
  mutate(total_f_points = f_total_pass_points + f_total_rush_points + f_total_rec_points + f_total_kicker_points) %>% 
  rename(player_name = clean_name)

# Defense summary
def_summary <- def_stats %>% 
  left_join(nfl_teams, by = join_by(defteam)) %>% 
  mutate(week = current_round_number) %>% 
  select(week, player_name = player_name_full, total_f_points = f_total_def_points)

# Players summary
player_summary <- raw_f_stats %>% 
  mutate(week = current_round_number) %>% 
  distinct(week, player_name, total_f_points)

# FINAL DATA COMBINED
full_summary_stats <- rbind(def_summary, player_summary)

# full_summary_stats <- full_summary_stats %>% 
#   mutate(week = case_when(playoff_round == "Wild Card" ~ 19,
#                           playoff_round == "Divisional" ~ 20,
#                           playoff_round == "Conference" ~ 21,
#                           playoff_round == "Super Bown" ~ 22,
#                           TRUE ~ as.character(week)))

# Combine with Lineup Submissions ####

weekly_lineups_scored <- left_join(lineups_long, full_summary_stats, by = join_by(player == player_name))

# Change scoring NAs to 0
weekly_lineups_scored$total_f_points[is.na(weekly_lineups_scored$total_f_points)] <- 0

# Define if on first round bye
weekly_lineups_scored <- weekly_lineups_scored %>% 
  mutate(week = case_when(playoff_round == "Wild Card" ~ "19",
                          playoff_round == "Divisional" ~ "20",
                          playoff_round == "Conference" ~ "21",
                          playoff_round == "Super Bown" ~ "22",
                          TRUE ~ as.character(week)),
         first_round_bye = if_else(team %in% c("SEA", "DEN"), TRUE, FALSE))

