library(Lahman)
library(tidyverse)
library(plotly)
library(shiny)
library(shinydashboard)
library(DT)
library(lubridate)

years <- Teams$yearID

teams <- Teams %>%
  group_by(teamID, lgID, name) %>%
  filter(lgID %in% c("AL", "NL")) %>%
  summarise() %>%
  ungroup() %>%
  mutate(lgID = as.character(lgID))

# Define UI for application that draws a histogram
ui <- dashboardPage(
  dashboardHeader(),
  dashboardSidebar(),
  dashboardBody(
    box("Filters",
        width =4,
        numericInput("Year",
                     label = "Select a Year",
                     value = max(years),
                     min = min(years),
                     max = max(years),
                     step = 1),      
        selectizeInput("League",
                       label = "Pick a League",
                       choices = sort(unique(teams$lgID)), 
                       selected = "AL"),
        uiOutput("teamSelection")
    
    ),
    box("Player Info",
      width=8,
      dataTableOutput("players")
    )
  )
)

# Define server logic required to draw a histogram
server <- function(input, output,session) {
  
  playerData <- reactive({
    req(input$Teams)
    batting <- Batting %>%
      left_join(names, by = "playerID") %>%
      filter(teamID == input$Teams & yearID == input$Year) %>%
      arrange(playerID, yearID)
    
  })
  
  output$players <- renderDataTable({
    playerTable <- playerData() %>%
      datatable()
  }) 
  
  output$teamSelection <- renderUI({
    selectizeInput("Teams",
                   label = "Pick a Team",
                   choices = filter(teams, lgID == input$League)$teamID)
    })
  
}

# Run the application 
shinyApp(ui = ui, server = server)

