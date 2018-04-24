library(Lahman)
library(tidyverse)
library(plotly)
library(shiny)
library(shinydashboard)
library(DT)
library(lubridate)

years <- Teams$yearID

names <- Master %>%
  select(playerID, nameFirst, nameLast)

batting <- Batting

teams <- Teams %>%
  group_by(teamID, lgID, name) %>%
  filter(lgID %in% c("AL", "NL")) %>%
  summarise() %>%
  ungroup() %>%
  mutate(lgID = as.character(lgID))

# Define UI for application that draws a histogram
ui <- dashboardPage(
  dashboardHeader(title="Lahman Batting Data"),
  dashboardSidebar(),
  dashboardBody(
    fluidRow(
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
    box("Par Plot",
      width=8,
      plotlyOutput("linePlot")
      )
    ),
    fluidRow(
      box("Player Info",
          width=12,
          dataTableOutput("players")
      )
    )
  )
)

# Define server logic required to draw a histogram
server <- function(input, output,session) {
  
  playerData <- reactive({
    req(input$Teams)
    battingPlayer <- batting %>%
      left_join(names, by = "playerID") %>%
      filter(yearID == input$Year & teamID == input$Teams) %>%
      select(nameFirst, nameLast,G,AB,R,H,X2B,X3B,HR,RBI,SB,CS,BB,SO,IBB,HBP,SH,SF,GIDP)
    
  })
  
  plotData <- reactive({
    req(input$Teams)
    battingPlot <- batting %>%
      filter(yearID == input$Year & teamID == input$Teams)
  })
  
  output$linePlot <- renderPlotly({
    p <- plotData() %>%
      plot_ly() %>%
      add_trace(
        type = 'parcoords',
        line = list(
          color = ~G,
          showscal = TRUE
        ),
        dimensions = list(
          list(range = c(~min(G), ~max(G)),
               label = 'Games Played', values = ~G),
          list(range = c(~min(AB), ~max(AB)),
               label = 'At-Bats', values = ~AB),
          list(range = c(~min(H), ~max(H)),
               label = 'Hits', values = ~H),
          list(range = c(~min(RBI), ~max(RBI)),
               label = 'RBI', values = ~RBI),
          list(range = c(~min(HR), ~max(HR)),
               label = 'HR', values = ~HR)
        )
      )
    return(p)
  })
  
  output$players <- renderDataTable({
    playerData() 
  }) 
  
  output$teamSelection <- renderUI({
    selectizeInput("Teams",
                   label = "Pick a Team",
                   choices = filter(teams, lgID == input$League)$teamID)
    })
  
}

# Run the application 
shinyApp(ui = ui, server = server)

