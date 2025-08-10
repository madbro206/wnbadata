library(shiny)
library(wehoop)
library(dplyr)
library(rsconnect)

wnba_player_box <- wehoop::load_wnba_player_box()

players <- wnba_player_box %>%
  select(athlete_id, athlete_display_name, team_name, athlete_headshot_href) %>%
  distinct(athlete_id, .keep_all = TRUE) %>%
  arrange(athlete_id) %>%
#filter out missing pictures
  filter(!is.na(athlete_headshot_href) & athlete_headshot_href != "")
  ui <- fluidPage(
    tags$head(
      tags$style(HTML("
        /* Center title and button horizontally */
        .header-container {
          text-align: center;
          margin-top: 20px;
          margin-bottom: 20px;
        }
        /* Center the main content vertically and horizontally */
        .center-content {
          height: calc(100vh - 140px); /* full viewport height minus approx header height */
          display: flex;
          flex-direction: column;
          justify-content: center; /* vertical centering */
          align-items: center;     /* horizontal centering */
          text-align: center;
        }
        /* Margins between elements */
        .center-content h3, .center-content h4, .center-content img {
          margin: 10px 0;
        }
      "))
    ),
    # Wrap title and button in a container at the top
    div(class = "header-container",
        titlePanel("Respect Every WNBA Player"),
        actionButton("randomize", "respect")
    ),
    fluidRow(
      column(
        width = 12,
        div(class = "center-content",
            h3(textOutput("player_name")),
            h4(textOutput("team_name")),
            uiOutput("player_pic")
        )
      )
    )
  )
  


server <- function(input, output, session) {
  # Reactive value for the random player's row number,
  # but do not run until button clicked
  rand_player <- eventReactive(input$randomize, {
    sample(nrow(players), 1)
  }) # no ignoreNULL = FALSE here
  
  output$player_name <- renderText({
    idx <- rand_player()
    if (is.null(idx)) {
      return("")  # no text initially
    }
    players$athlete_display_name[idx]
  })
  output$team_name <- renderText({
    idx <- rand_player()
    if (is.null(idx)) {
      return("")
    }
    players$team_name[idx]
  })
  output$player_pic <- renderUI({
    idx <- rand_player()
    if (is.null(idx)) {
      return(NULL)  # no player shown initially
    }
    tags$img(src = players$athlete_headshot_href[idx], width = 200)
  })
}

shinyApp(ui, server)

#rsconnect::deployApp('/Users/maddy/Desktop/wehoop/respect_app')
