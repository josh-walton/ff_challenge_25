
# Name Validation 
validate_name <- function(first, last) {
  first <- trimws(first)
  last  <- trimws(last)
  
  if (first == "" || last == "") {
    return(list(
      valid = FALSE,
      message = "⚠️ First and Last Name are required."
    ))
  }
  
  list(
    valid = TRUE,
    first = first,
    last  = last
  )
}

# Round Validation
validate_round_open <- function(selected_round, allowed_rounds) {
  if (!selected_round %in% allowed_rounds) {
    return(list(
      valid = FALSE,
      message = "⚠️ Submissions for this round are closed."
    ))
  }
  
  list(valid = TRUE)
}


# Dupe Player Validation

validate_no_duplicates <- function(players) {
  dupes <- players[duplicated(players)]
  
  if (length(dupes) > 0) {
    return(list(
      valid = FALSE,
      message = paste0(
        "⚠️ Duplicate player selected: ",
        paste(unique(dupes), collapse = ", ")
      )
    ))
  }
  
  list(valid = TRUE)
}

# Lineup Builder

build_lineup <- function(input, first, last) {
  tibble(
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
}
