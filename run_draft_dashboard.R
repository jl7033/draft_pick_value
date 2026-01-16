library(shiny)
library(plotly)
library(tidyverse)

# load data
draft <- readRDS("Data/draft_shiny.rds")

# team colors dictionary

team_colors <- c(
  "ARI" = "#97233F",
  "ATL" = "#A71930",
  "BAL" = "#241773",
  "BUF" = "#00338D",
  "CAR" = "#0085CA",
  "CHI" = "#0B162A",
  "CIN" = "#FB4F14",
  "CLE" = "#311D00",
  "DAL" = "#003594",
  "DEN" = "#FB4F14",
  "DET" = "#0076B6",
  "GNB"  = "#203731",
  "HOU" = "#03202F",
  "IND" = "#002C5F",
  "JAX" = "#006778",
  "KAN"  = "#E31837",
  "LVR"  = "#000000",
  "LAC" = "#002A5E",
  "LAR" = "#0B0B0B",
  "MIA" = "#008E97",
  "MIN" = "#4F2683",
  "NWE"  = "#002244",
  "NOR"  = "#D3BC8D",
  "NYG" = "#0B2265",
  "NYJ" = "#125740",
  "PHI" = "#004C54",
  "PIT" = "#FFB612",
  "SEA" = "#002244",
  "SFO"  = "#AA0000",
  "TAM"  = "#D50A0A",
  "TEN" = "#4B92DB",
  "WAS" = "#5A1414"
)

# create the UI

ui <- fluidPage(
  titlePanel("NFL Draft Efficiency Dashboard"),
  
  sidebarLayout(
    sidebarPanel(
      selectInput(
        "team",
        "Select Team",
        choices = sort(unique(draft$team))
      ),
      
      sliderInput(
        "years",
        "Draft Years",
        min = min(draft$season),
        max = max(draft$season),
        value = c(1997, 1999),
        sep = ""
      ),
      
      selectInput(
        "category",
        "Select Position Group",
        choices = c("All", sort(unique(draft$category))),
        selected = "All",
        multiple = TRUE
      ),
      
      selectInput(
        "metric",
        "Efficiency Metric",
        choices = c(
          "wAV / Meers"   = "efficiency_overall",
          "drAV / Meers"  = "efficiency_team",
          "wAV / Johnson" = "efficiency_overall_johnson",
          "drAV / Johnson"= "efficiency_team_johnson"
        )
      )
    ),
    
    mainPanel(
      plotlyOutput("eff_plot", height = "400px"),
      hr(),
      h3(textOutput("summary_stat"))
    )
  )
)

# create the server

server <- function(input, output) {
  
  # 1. Filter data reactively
  filtered_data <- reactive({
    display_df = draft %>%
      filter(
        team == input$team,
        season >= input$years[1],
        season <= input$years[2]
      )
    
    if (!("All" %in% input$category)) {
      display_df <- display_df %>%
        filter(category %in% input$category)
    }
    
    display_df
    
  })
  
  # 2. Pick-level bar plot
  output$eff_plot <- renderPlotly({
    df <- filtered_data()
    
    # Get the selected team's color
    team_col <- team_colors[df$team[1]]  # only one team selected
    
    plot_ly(
      df,
      x = ~pick_overall,
      y = ~get(input$metric),
      type = "bar",
      text = ~paste0(
        "Year: ", season,
        "<br>Player: ", player,
        "<br>Pick: ", pick_overall,
        "<br>Efficiency: ", round(get(input$metric), 3)
      ),
      marker = list(
        color = team_col,
        line = list(color = "black", width = 1)
      )
    ) %>%
      layout(
        xaxis = list(
          title = "Overall Pick Number",
          range = c(1, 270),
          tick0 = 0,
          dtick = 20
        ),
        yaxis = list(title = "Weighted AV per Draft Value")
      )
  })
  
  
  # 3. Summary efficiency statistic
  output$summary_stat <- renderText({
    df <- filtered_data()
    
    # Decide numerator and denominator based on metric
    if (input$metric %in% c("efficiency_overall", "efficiency_overall_johnson")) {
      numerator <- sum(df$w_av, na.rm = TRUE)
    } else {
      numerator <- sum(df$dr_av, na.rm = TRUE)
    }
    
    if (grepl("meers", input$metric)) {
      denominator <- sum(df$meers_value, na.rm = TRUE)
    } else {
      denominator <- sum(df$johnson_value, na.rm = TRUE)
    }
    
    eff <- numerator / denominator
    
    paste0(
      input$years[1], "–", input$years[2], ": ",
      input$team,
      " drafted with an efficiency of ",
      round(eff, 3),
      " AV per draft value point."
    )
  })
}

# run app
shinyApp(ui = ui, server = server)