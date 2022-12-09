library(tidyverse)

get_bbdb(table = "Teams")
get_bbdb(table = "People")
get_bbdb(table = "Appearances")
get_bbdb(table = "TeamsFranchises")

Bullpen <- Appearances %>%
  filter(yearID == 2019, G_p > 0) %>%
  left_join(People, on="playerID") %>%
  left_join(Teams, on="teamID") %>%
  left_join(TeamsFranchises, on="franchID") %>%
  select(teamIDBR, nameFirst, nameLast, playerID, bbrefID)

