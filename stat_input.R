weekly_lineups_scored <- readRDS("data/weekly_lineups_scored.rds")

previous_round_players <- weekly_lineups_scored %>%
  filter(playoff_round == "Wild Card") %>%
  select(manager_full_name, player) %>%
  distinct() %>%
  mutate(in_prev_round = TRUE)
