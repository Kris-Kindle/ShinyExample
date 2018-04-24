library(tidyverse)
library(Lahman)
library(gridExtra)

batting <- Batting

summary(batting)

p1 <- ggplot(batting, aes(G)) + geom_histogram(binwidth = 1)

p2 <- ggplot(batting, aes(AB)) + geom_histogram(binwidth =1)

p3 <- ggplot(batting, aes(R)) + geom_histogram(binwidth = 1)

p4 <- ggplot(batting, aes(HR)) + geom_histogram(binwidth = 1)

gridExtra::grid.arrange(p1,p2,p3,p4)

teams <- batting %>%
  group_by(lgID, teamID) %>%
  summarise(
    firstYear = min(yearID),
    last = max(yearID)
  )




player <- batting %>%
  group_by(playerID) %>%
  summarise()




