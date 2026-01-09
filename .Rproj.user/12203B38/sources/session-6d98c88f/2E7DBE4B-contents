# Library ####

library(shiny)
library(tidyverse)
library(janitor)
library(data.table)
library(nflreadr)
library(nflfastR)
library(nflplotR)
library(scales)
library(rsconnect)
library(googlesheets4)


folder_path <- "/Users/thefam/Desktop/FF_challenge_25"

# Shiny App Server Connection #### 
# rsconnect::setAccountInfo(name='walton', 
# token='401BC3E019D04014659F8A6898C299CE', 
# secret='xaVL1JNKwZyfwwZ40q4TqOowqRk5pNPZNSAoVftP')


# Roster Pull ####
# 
# rosters <- load_rosters_weekly(seasons = 2025)
# 
# positions <- c("QB", "RB", "WR", "TE", "K")
# 
# for (x in positions) {
#   pool <- rosters %>% 
#     filter(week == 18,
#            depth_chart_position == x,
#            team %in% playoff_teams) %>% 
#     distinct(player = full_name, team)
#   
#   write_csv(pool, file = paste0(folder_path, x, "_pool.csv"))
# }

# Items ####

round_windows <- tibble(
  round = c("Wild Card", "Divisional", "Conference", "Super Bowl"),
  open_time = with_tz(ymd_hm(c(
    "2026-01-06 12:00",
    "2026-01-12 23:00",
    "2026-01-18 23:00",
    "2026-01-25 23:00"
  )), "America/Chicago"),
  close_time = with_tz(ymd_hm(c(
    "2026-01-10 15:30", # Wild Card lock
    "2026-01-17 15:30",
    "2026-01-25 14:00",
    "2026-02-08 17:30"
  )), "America/Chicago")
)

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


# Application ####
# ---- Load player pools ----
qb_pool  <- read_csv("QB_pool.csv") %>% 
  mutate(player_w_team = paste0(player, " - ", team)) %>% 
  arrange(player_w_team)
rb_pool  <- read_csv("RB_pool.csv") %>% 
  mutate(player_w_team = paste0(player, " - ", team)) %>% 
  arrange(player_w_team)
wr_pool  <- read_csv("WR_pool.csv") %>% 
  mutate(player_w_team = paste0(player, " - ", team)) %>% 
  arrange(player_w_team)
te_pool  <- read_csv("TE_pool.csv") %>% 
  mutate(player_w_team = paste0(player, " - ", team)) %>% 
  arrange(player_w_team)
flex_pool  <- read_csv("FLEX_pool.csv") %>% 
  mutate(player_w_team = paste0(player, " - ", team)) %>% 
  arrange(player_w_team)
k_pool   <- read_csv("K_pool.csv") %>% 
  mutate(player_w_team = paste0(player, " - ", team)) %>% 
  arrange(player_w_team)
def_pool <- read_csv("DEF_pool.csv") %>% 
  mutate(player_w_team = paste0(player, " - ", team)) %>% 
  arrange(player_w_team)

# ---- UI ----
ui <- fluidPage(
  titlePanel("NFL Fantasy Playoff Challenge"),
  
  sidebarLayout(
    
    sidebarPanel(
      textInput("first_name", "First Name (required)", placeholder = "First Name"),
      textInput("last_name", "Last Name (required)", placeholder = "Last Name"),
      uiOutput("round_selector"),
      
      tags$hr(),
      
      h5("Lineup Selection"),
      
      selectInput("qb", "QB", choices = qb_pool$player_w_team),
      selectInput("rb1", "RB1", choices = rb_pool$player_w_team),
      selectInput("rb2", "RB2", choices = rb_pool$player_w_team),
      selectInput("wr1", "WR1", choices = wr_pool$player_w_team),
      selectInput("wr2", "WR2", choices = wr_pool$player_w_team),
      selectInput("te", "TE", choices = te_pool$player_w_team),
      selectInput("flex", "FLEX", choices = flex_pool$player_w_team),
      selectInput("k",  "K",  choices = k_pool$player_w_team),
      selectInput("def","DEF",choices = def_pool$player_w_team),
      
      tags$hr(),
      
      h5("Current Selection"),
      tableOutput("summary"),
      
      tags$small("Click Submit after selections are made"),
      br(), br(),
      
      uiOutput("name_error"),
      uiOutput("lineup_error"),
      
      actionButton("submit", "Submit Lineup", class = "btn-primary"),
      uiOutput("confirmation")
    ),
    
    mainPanel(
      tabsetPanel(
        tabPanel(
          "Rules",
          h4("NFL Fantasy Playoff Challenge Rules"),
          uiOutput("rules_text")
        ),
        
        tabPanel(
          "Scoring",
          h4("Scoring Settings"),
          tableOutput("scoring_table")
        ),
        
        tabPanel(
          "Scoreboard",
          h4("Lineups will be locked and visible at kickoff of the first Wild Card game. 
             Then reopened at the end of the final game. You can reference previous lineups here.")
        )
      )
    )
    
  )
)

# ---- Server ----
# Google Sheets #
sheet_url <- "https://docs.google.com/spreadsheets/d/1o9Unvct-PpaV_SOus1gEMP8kkDY97KekQRJN2XxQM2E/edit?gid=0#gid=0"

# Use service account credentials
gs4_auth(path = "service-account.json")

server <- function(input, output, session) {
  
  
  
  output$summary <- renderTable({
    req(input$qb)
    
    tibble(
      Slot = c("Manager", "Round", "QB", "RB1", "RB2", "WR1", "WR2", "TE", "FLEX", "K", "DEF"),
      Selection = c(
        paste(input$first_name, input$last_name),
        input$playoff_round,
        input$qb,
        input$rb1,
        input$rb2,
        input$wr1,
        input$wr2,
        input$te,
        input$flex,
        input$k,
        input$def
      )
    )
  }, striped = TRUE, hover = TRUE)
  
  

  available_rounds <- reactive({
    now <- with_tz(Sys.time(), "America/Chicago")
    
    round_windows %>%
      filter(now >= open_time, now < close_time) %>%
      pull(round)
  })
  
  output$round_selector <- renderUI({
    rounds <- available_rounds()
    
    if (length(rounds) == 0) {
      tags$p(
        "⚠️ No lineup submissions are currently open.",
        style = "color: red; font-weight: bold;"
      )
    } else {
      selectInput(
        "playoff_round",
        "Round",
        choices = rounds
      )
    }
  })
  
  
  
  output$scoring_table <- renderTable({
    scoring_settings
  }, striped = TRUE, hover = TRUE, bordered = TRUE)
  
  output$rules_text <- renderUI({
    tagList(
      tags$ul(
        tags$li("Submit your best lineup every week of the playoffs."),
        tags$li("$5 entry fee. The manager with the most points takes all!"),
        tags$li("Each round has a different point value multiplier 
                (the x2 in the divisional round, x3 in the 
                conference round and x4 in the Super Bowl if you had 
                the player on your roster from the beginning)."),
        tags$li("You'll want to build your lineup around players 
                you think will make it all the way to the final game, thus taking advantage of the multipliers."),
        tags$li("If you change a roster spot in the 2nd, 3rd or 4th round (because either
        they've been eliminated or you just don't want them anymore), their point value goes back to x1."),
        tags$li("You can swap players out each week based on the matchups, but their multipliers 
                reset with every change. Their multiplier increases each round they're on your roster consecutively.")
      ),
      p("Good luck!")
    )
  })
  
  
  observeEvent(input$submit, {
    
    first <- trimws(input$first_name)
    last  <- trimws(input$last_name)
    
    if (input$first_name == "" || input$last_name == "") {
      
      output$name_error <- renderUI({
        tags$p(
          "⚠️ First and Last Name are required.",
          style = "color: red; font-weight: bold;"
        )
      })
      
      return(NULL)
    }
    
    # Clear error if valid
    output$name_error <- renderUI({ NULL })
    
    req(input$first_name, input$last_name)
    
    # Limiting Round availability for display options
    allowed <- available_rounds()
    
    if (!input$playoff_round %in% allowed) {
      output$lineup_error <- renderUI({
        tags$p(
          "⚠️ Submissions for this round are closed.",
          style = "color: red; font-weight: bold;"
        )
      })
      return(NULL)
    }
    
    
    # ---- Duplicate player validation (RB / WR / TE / FLEX) ----
    skill_players <- c(
      input$rb1,
      input$rb2,
      input$wr1,
      input$wr2,
      input$te,
      input$flex
    )
    
    # Identify duplicates
    dupes <- skill_players[duplicated(skill_players)]
    
    if (length(dupes) > 0) {
      output$lineup_error <- renderUI({
        tags$p(
          paste0(
            "⚠️ Duplicate player selected: ",
            paste(unique(dupes), collapse = ", ")
          ),
          style = "color: red; font-weight: bold;"
        )
      })
      return(NULL)
    }
    
    # Clear lineup error if valid
    output$lineup_error <- renderUI({ NULL })
    
    lineup <- tibble(
      timestamp  = format(
        with_tz(Sys.time(), "America/Chicago"),
        "%Y-%m-%d %H:%M:%S"
      ),
      first_name = first,
      last_name  = last,
      manager    = paste(first, last),
      week       = input$playoff_round,
      qb         = input$qb,
      rb1        = input$rb1,
      rb2        = input$rb2,
      wr1        = input$wr1,
      wr2        = input$wr2,
      te         = input$te,
      flex       = input$flex,
      k          = input$k,
      def        = input$def
    )
    
    tryCatch({
      sheet_append(ss = sheet_url, data = lineup, sheet = "Lineups")
      output$confirmation <- renderUI({
        tags$p("✅ Lineup submitted!", style="color: green; font-weight: bold; margin-top: 10px;")
      })
    }, error = function(e) {
      output$confirmation <- renderUI({
        tags$p(paste0("❌ Submission failed: ", e$message), style="color: red; font-weight: bold; margin-top: 10px;")
      })
    })
    
  })
}

# Shiny App ####
shinyApp(ui, server)
