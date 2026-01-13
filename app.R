# Library and Sources ####

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


# Source helper scripts
source("global.R")
source("stat_input.R")
source("helpers.R")


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
      uiOutput("summary_ui"),
      
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
          "Scoreboard",
          uiOutput("scoreboard_ui"),
          selectInput(
            "manager_detail",
            "View lineup for:",
            choices = NULL
          ),
          tableOutput("lineup_detail_table")
        ),
        
        tabPanel(
          "Rules",
          h4("NFL Fantasy Playoff Challenge Rules"),
          uiOutput("rules_text")
        ),
        
        tabPanel(
          "Scoring Settings",
          tableOutput("scoring_table")
        )
      )
    )
    
  )
)

# ---- Server ----

server <- function(input, output, session) {
  
  now <- reactive({
  with_tz(Sys.time(), "America/Chicago")
  })
  
  available_rounds <- reactive({
    round_windows %>%
      filter(now() >= open_time, now() < close_time) %>%
      pull(round)
  })
  
  manager_totals <- reactive({
    weekly_lineups_scored %>%
      group_by(manager_full_name, playoff_round) %>%
      summarise(
        Total = sum(total_f_points, na.rm = TRUE),
        .groups = "drop"
      ) %>%
      arrange(desc(Total)) %>%
      mutate(Rank = row_number()) %>%
      select(
        Rank,
        Manager = manager_full_name,
        Total
      )
  })
  
  scoreboard_rounds <- reactive({
    round_windows %>%
      filter(now() >= close_time) %>%
      pull(round)
  })
  
  scoreboard_data <- reactive({
    
    read_sheet(
      ss = sheet_url,
      sheet = "Lineups"
    ) %>%
      mutate(
        # timestamp as datetime
        timestamp = ymd_hms(timestamp, tz = "America/Chicago")
      ) %>%
      
      # ---- Filter to the currently active round ----
    filter(playoff_round %in% scoreboard_rounds()) %>% 
      
      # ---- Keep only most recent submission per manager ----
    arrange(manager_full_name, desc(timestamp)) %>%
      distinct(manager_full_name, playoff_round, .keep_all = TRUE) %>%
      
      # ---- Select & rename columns for display ----
    select(
      Manager = manager_full_name,
      Round   = playoff_round,
      QB      = qb,
      RB1     = rb1,
      RB2     = rb2,
      WR1     = wr1,
      WR2     = wr2,
      TE      = te,
      FLEX    = flex,
      K       = k,
      DEF     = def
    ) %>%
      
      arrange(Manager)
  })
  
  scoreboard_open <- reactive({
    now() >= kickoff_time })
  
  output$summary <- renderTable({
    req(input$qb)
    req(input$playoff_round)
    
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
  
  output$summary_ui <- renderUI({
    
    if (length(available_rounds()) == 0) {
      tags$p(
        "Lineups are currently locked.",
        style = "color: gray; font-style: italic;"
      )
    } else {
      tableOutput("summary")
    }
    
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
  
  output$scoreboard_ui <- renderUI({
    
    if (!scoreboard_open()) {
      tags$p(
        "⏳ Scoreboard will be available at kickoff (3:30 PM CT).",
        style = "font-weight: bold; color: gray;"
      )
    } else {
      tableOutput("scoreboard_table")
    }
  })
  
  output$lineup_detail_table <- renderTable({
    req(input$manager_detail)
    
    weekly_lineups_scored %>%
      filter(manager_full_name == input$manager_detail) %>%
      select(
        Slot = slot,
        Player = player,
        Team = team,
        "R1 Points" = total_f_points
      ) %>%
      mutate(
        Slot = factor(
          Slot,
          levels = c("qb", "rb1", "rb2", "wr1", "wr2", "te", "flex", "k", "def")
        ),
        Slot = str_to_upper(as.character(Slot))
      ) %>%
      arrange(
        match(Slot, c("QB", "RB1", "RB2", "WR1", "WR2", "TE", "FLEX", "K", "DEF"))
      )
  }, striped = TRUE, hover = TRUE)
  
  
  output$scoreboard_table <- renderTable({
    manager_totals()
  }, striped = TRUE, hover = TRUE)
  
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
  
  observe({
    updateSelectInput(
      session,
      "manager_detail",
      choices = unique(weekly_lineups_scored$manager_full_name)
    )
  })
  
  observeEvent(input$submit, {
    
    # ---- Name validation ----
    name_check <- validate_name(input$first_name, input$last_name)
    
    if (!name_check$valid) {
      output$name_error <- renderUI({
        tags$p(name_check$message,
               style = "color: red; font-weight: bold;")
      })
      return(NULL)
    }
    
    output$name_error <- renderUI({ NULL })
    
    # ---- Round validation ----
    round_check <- validate_round_open(
      input$playoff_round,
      available_rounds()
    )
    
    if (!round_check$valid) {
      output$lineup_error <- renderUI({
        tags$p(round_check$message, style = "color: red; font-weight: bold;")
      })
      return(NULL)
    }
    
    # ---- Duplicate validation ----
    dup_check <- validate_no_duplicates(c(
      input$rb1,
      input$rb2,
      input$wr1,
      input$wr2,
      input$te,
      input$flex
    ))
    
    if (!dup_check$valid) {
      output$lineup_error <- renderUI({
        tags$p(dup_check$message, style = "color: red; font-weight: bold;")
      })
      return(NULL)
    }
    
    output$lineup_error <- renderUI({ NULL })
    
    # ---- Build lineup ----
    lineup <- build_lineup(
      input,
      name_check$first,
      name_check$last
    )
    
    # ---- Submit ----
    tryCatch({
      sheet_append(ss = sheet_url, data = lineup, sheet = "Lineups")
      output$confirmation <- renderUI({
        tags$p("✅ Lineup submitted!",
               style = "color: green; font-weight: bold; margin-top: 10px;")
      })
    }, error = function(e) {
      output$confirmation <- renderUI({
        tags$p(paste0("❌ Submission failed: ", e$message),
               style = "color: red; font-weight: bold; margin-top: 10px;")
      })
    })
    
  })
}

# Shiny App ####
shinyApp(ui, server)
