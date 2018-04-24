library(tidyverse)
library(Lahman)
library(plotly)

batting <- Batting

batting %>%
  filter(yearID == 2016) %>%
  plot_ly() %>%
  add_trace(
    type = 'parcoords',
    line = list(
      color = ~G,
      showscal = TRUE
      ),
    dimensions = list(
      list(range = c(~min(G, na.rm =T), ~max(G, na.rm =T)),
           label = 'Games Played', values = ~G),
      list(range = c(~min(AB, na.rm =T), ~max(AB, na.rm =T)),
           label = 'At-Bats', values = ~AB),
      list(range = c(~min(H, na.rm =T), ~max(H, na.rm =T)),
           label = 'Hits', values = ~H),
      list(range = c(~min(RBI, na.rm =T), ~max(RBI, na.rm =T)),
           label = 'RBI', values = ~RBI),
      list(range = c(~min(HR, na.rm =T), ~max(HR, na.rm =T)),
           label = 'HR', values = ~HR)
    )
  )
  


