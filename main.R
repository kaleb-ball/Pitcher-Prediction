library(mgcv)
library(xgboost)
library(Lahman)
library(baseballDBR)


setwd("/Users/kalebball/DataSpellProjects/Graduate School/Advanced Machine Learning/Baseball Project")
source("data.R")
source("utils.baseball.R")
source("model.R")

dates <- read.csv('mlb_start_end.csv')
data <- cleanData(getRawData(getwd(), dates, useCSV = T)) %>%
  addSeasonStats(dates)
## Select Data

data_model <- data %>%
  ungroup() %>%
  select(game_pk, pitcher, batter, total_bases, p_throws,
         on_1b, on_2b, on_3b, outs_when_up,
         inning, home, pitcher_batters_faced,
         pitcher_cumulative_pitches, is_starter, tto, runs_scored,
         pitches_seen, times_facing_batter, pitches_prev_at_bat, woba_value,
         IP_cumsum:"SwStr%.batter"
  )%>%
  select (-c(PA, season.batter)) %>%
  mutate(across(c(p_throws, on_1b, on_2b, on_3b, home, is_starter), as.factor),
          across(c(pitcher_batters_faced, pitcher_cumulative_pitches, tto:'SwStr%.batter'), ~ replace_na(.x, 0)))

xgb(data_model)

