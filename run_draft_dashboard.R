library(shiny)
library(plotly)
library(tidyverse)

# load data
draft <- readRDS("Data/draft_shiny.rds")

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
    
    gg <- ggplot(
      df,
      aes(
        x = pick_overall,
        y = .data[[input$metric]],
        text = paste0(
          "Year: ", season,
          "<br>Player: ", player,
          "<br>Pick: ", pick_overall,
          "<br>Efficiency: ", round(.data[[input$metric]], 3)
        )
      )
    ) +
      geom_col(width = 1, color = "black") +
      labs(
        x = "Overall Pick Number",
        y = "Weighted AV per Draft Value",
        title = paste(
          input$team,
          "Draft Picks (",
          input$years[1], "–", input$years[2], ")"
        )
      ) +
      theme_minimal()
    
    ggplotly(gg, tooltip = "text")
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