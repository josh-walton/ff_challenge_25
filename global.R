# Global Items ####

# List of eliminated teams
eliminated_teams <- c("LAC", "PIT", "PHI", "GB", "JAX", "CAR", "BUF", "SF", "HOU", "CHI")

# Order of playoff rounds
round_levels <- c("Wild Card", "Divisional", "Conference", "Super Bowl")

# Use service account credentials
gs4_auth(path = "service-account.json")

# Google Sheets #
sheet_url <- "https://docs.google.com/spreadsheets/d/1o9Unvct-PpaV_SOus1gEMP8kkDY97KekQRJN2XxQM2E/edit?gid=0#gid=0"

# Main path
folder_path <- "/Users/thefam/Desktop/FF_challenge_25"

# Wild Card Kickoff time
kickoff_time <- ymd_hm("2026-01-10 15:30", tz = "America/Chicago")

# Load player pools 
load_pool <- function(filename) {
  read_csv(filename) %>% 
    mutate(player_w_team = paste0(player, " - ", team)) %>% 
    arrange(player_w_team)
}

qb_pool   <- load_pool("data/QB_pool.csv")
rb_pool   <- load_pool("data/RB_pool.csv")
wr_pool   <- load_pool("data/WR_pool.csv")
te_pool   <- load_pool("data/TE_pool.csv")
flex_pool <- load_pool("data/FLEX_pool.csv")
k_pool    <- load_pool("data/K_pool.csv")
def_pool  <- load_pool("data/DEF_pool.csv")

# Set timing windows for each round
round_windows <- tibble(
  round = c("Wild Card", "Divisional", "Conference", "Super Bowl"),
  week = c("19", "20", "21", "22"),
  
  open_time = force_tz(
    ymd_hm(c(
      "2026-01-06 12:00",
      "2026-01-12 22:00",
      "2026-01-18 23:00",
      "2026-01-25 23:00"
    )),
    "America/Chicago"
  ),
  
  close_time = force_tz(
    ymd_hm(c(
      "2026-01-10 15:30",
      "2026-01-17 15:30", # Edited for testing
      "2026-01-25 14:00",
      "2026-02-08 17:30"
    )),
    "America/Chicago"
  )
)

# NFL Teams Table
nfl_teams <- tribble(
  ~player_name_full,        ~defteam,
  "Arizona",         "ARI",
  "Atlanta",         "ATL",
  "Baltimore",       "BAL",
  "Buffalo",         "BUF",
  "Carolina",        "CAR",
  "Chicago",         "CHI",
  "Cincinnati",      "CIN",
  "Cleveland",       "CLE",
  "Dallas",          "DAL",
  "Denver",          "DEN",
  "Detroit",         "DET",
  "Green Bay",       "GB",
  "Houston",         "HOU",
  "Indianapolis",    "IND",
  "Jacksonville",    "JAX",
  "Kansas City",     "KC",
  "Las Vegas",       "LV",
  "Los Angeles Chargers",     "LAC",
  "Los Angeles Rams",     "LA",
  "Miami",           "MIA",
  "Minnesota",       "MIN",
  "New England",     "NE",
  "New Orleans",     "NO",
  "New York",        "NYG",
  "New York",        "NYJ",
  "Philadelphia",    "PHI",
  "Pittsburgh",      "PIT",
  "San Francisco",   "SF",
  "Seattle",         "SEA",
  "Tampa Bay",       "TB",
  "Tennessee",       "TEN",
  "Washington",      "WAS"
)

# List of playoff teams
playoff_teams <- c("DEN", "NE", "SEA",
                   "CHI", "JAX", "PHI", 
                   "PIT", "CAR", "HOU", 
                   "LA", "LAC", "BUF", 
                   "GB", "SF")

# Scoring Settings
scoring_settings <- tribble(
  ~Category,            ~Stat,                     ~Points,
  "Passing",            "Passing Yards (per yard)", 0.04,
  "Passing",            "Passing TD",               6,
  "Passing",            "Interception",            -2,
  "Rushing",            "Rushing Yards (per yard)", 0.10,
  "Rushing",            "Rushing TD",               6,
  "Receiving",          "Reception",                0.5,
  "Receiving",          "Receiving Yards (per yard)", 0.10,
  "Receiving",          "Receiving TD",              6,
  "Offense",               "Fumble Lost",              -2,
  "Offense",               "2-Point Conversion",        2,
  "Offense",               "40+ Yard Play Bonus",        2,
  "Kicking",            "Extra Point",               1,
  "Kicking",            "FG 0–39 yards",              3,
  "Kicking",            "FG 40–49 yards",             4,
  "Kicking",            "FG 50+ yards",               5,
  "Defense",            "Interception",               2,
  "Defense",            "Sack",                       1,
  "Defense",            "Safety",                     4,
  "Defense",            "Points Allowed (0)",        10,
  "Defense",            "Points Allowed (1–6)",       7,
  "Defense",            "Points Allowed (7–13)",      4,
  "Defense",            "Points Allowed (14–20)",     2,
  "Defense",            "Points Allowed (21–27)",     0,
  "Defense",            "Points Allowed (28–34)",    -1,
  "Defense",            "Points Allowed (35+)",      -4
)