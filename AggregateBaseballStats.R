library(Lahman)
library(shiny)
library(tidyverse)


names <- Master %>%
  select(playerID, nameFirst, nameLast)

total <- battingStats() %>%
  group_by(playerID) %>%
  summarise(
    yearID = 9999,
    G = sum(G, na.rm =T),
    AB = sum(AB, na.rm =T),
    R = sum(R, na.rm =T),
    H = sum (H, na.rm =T),
    X2B = sum(X2B, na.rm =T),
    X3B = sum(X3B, na.rm =T),
    HR = sum(HR, na.rm =T),
    RBI = sum(RBI, na.rm =T),
    SB = sum(SB, na.rm =T),
    CS = sum(CS, na.rm =T),
    BB = sum(BB, na.rm =T),
    SO = sum(SO, na.rm =T),
    IBB = sum(IBB, na.rm =T),
    HBP = sum(HBP, na.rm =T),
    SH = sum(SH, na.rm =T),
    SF = sum(SF, na.rm =T),
    GIDP = sum(GIDP, na.rm =T),
    BA = H / AB,
    PA = sum(PA, na.rm =T),
    TB = sum(TB, na.rm =T),
    SlugPct = (HR * 4 + X3B * 3 + X2B *2 + (H - HR - X3B - X2B) *1) / AB,
    OBP = (BB + HBP + H) / (AB +IBB + BB + SF),
    OPS = SlugPct + OBP,
    BABIP = (H - HR)/(AB - HR - SO + SF)
  )

batting <- battingStats() %>%
  bind_rows(total) %>%
  left_join(names, by = "playerID") %>%
  arrange(playerID, yearID)
  


