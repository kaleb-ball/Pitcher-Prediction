library(tidyverse)
library(baseballr)
library(lubridate)
library(stringr)
library(bbplot)
library(rvest)
library(car)


get_statcast_data <- function(start, end, player_type="") {
  data <- data.frame()
  i <- start %--% end
  while(start %within% i) {
    end <- start + ddays(1)
    new_data <- tibble()
    new_data <- statcast_search(start_date = start, end_date = end, player_type=player_type)
    if (nrow(new_data) > 1) {
      data <- rbind(new_data, data)
    }
    start <- end + ddays(1)
  }
  return(data)
}

clean_statcast_data <- function(data) {
  return(data %>%
     select(-contains("deprecated")) %>%
     ungroup() %>%
     mutate(
       total_bases = case_when(
         events == 'walk' ~ 1,
         events == 'single' ~ 1,
         events == 'double' ~ 2,
         events == 'triple' ~ 3,
         events == 'home run' ~ 4,
         TRUE ~ 0)
     ) %>%
     mutate(end_at_bat = ifelse(events != '', 1,0)) %>%
     group_by(game_pk, inning_topbot) %>%
     arrange(game_pk, inning, inning_topbot, at_bat_number, pitch_number) %>%
     mutate(team_cumulative_pitches = ifelse(row_number() == 1, 1, 0),
            team_cumulative_pitches = cumsum(pitch_number != ''),
            cumulative_batters_faced = ifelse(row_number() ==1, 1, 0),
            cumulative_batters_faced = 1 + cumsum(events != ''),
            home = ifelse(inning_topbot == 'Bot', 1, 0)) %>%
     group_by(game_pk, inning_topbot, pitcher) %>%
     mutate(pitcher_batters_faced = ifelse(row_number() ==1, 1, 0),
            pitcher_batters_faced = 1 + cumsum(events != ''),
            pitcher_cumulative_pitches = ifelse(row_number() == 1, 1, 0),
            pitcher_cumulative_pitches = cumsum(pitch_number != ''),
            is_starter = ifelse(pitcher_cumulative_pitches == team_cumulative_pitches, 1, 0),
            tto = floor(pitcher_batters_faced / 10) + 1,
            runs_scored = post_bat_score - bat_score) %>%
     group_by(game_pk, pitcher, batter) %>%
     arrange(game_pk, pitcher, batter, pitcher_cumulative_pitches ) %>%
     mutate(pitches_seen = ifelse(row_number() == 1, 1, 0),
            pitches_seen = cumsum(pitch_number != '')) %>%
     group_by(game_pk, at_bat_number, pitcher, batter) %>%
     arrange(game_pk, inning, inning_topbot, pitcher, pitcher_cumulative_pitches ) %>%
     mutate(pitches_seen_at_bat = ifelse(row_number() == 1, 1, 0),
            pitches_seen_at_bat = cumsum(pitch_number != '')) %>%
     group_by(game_pk, pitcher, batter) %>%
     arrange(game_pk, batter, pitcher, pitcher_cumulative_pitches ) %>%
     mutate(
       times_facing_batter =  cumsum(end_at_bat),
       times_facing_batter = ifelse(events != '', times_facing_batter - 1, times_facing_batter),
       pitches_prev_at_bat = ifelse(lag(events != ''), lag(pitches_seen_at_bat), NA),
       pitches_prev_at_bat= ifelse(row_number() == 1, 0, pitches_prev_at_bat)) %>%
     group_by(game_pk, at_bat_number, pitcher, batter) %>%
            mutate(pitches_prev_at_bat = ifelse(is.na(pitches_prev_at_bat), first(pitches_prev_at_bat), pitches_prev_at_bat))) %>%
    ungroup() %>%
    group_by( game_pk, inning_topbot, at_bat_number, batter) %>%
    mutate( total_bases = last(total_bases)) %>%
    mutate( across( c(on_1b, on_2b, on_3b), ~ ifelse(is.na(.x), 0, 1)))
}

join_pitch_speed_data <- function(data) {
  ave_pitch_speed <- data %>%
    ungroup() %>%
    select(pitcher, pitch_type, release_speed) %>%
    group_by(pitcher, pitch_type) %>%
    summarise(meanSpeed = mean(release_speed)) %>%
    mutate(pitch_type = ifelse(pitch_type == "", 'Other', pitch_type)) %>%
    drop_na()

  return(data %>%
           left_join(ave_pitch_speed, by="pitcher") %>%
           filter(pitch_type.y == pitch_type.x) %>%
           mutate(speed_percent_change = effective_speed / meanSpeed))

}

addSeasonStats <- function(data, dates) {
  ids <- chadwick_player_lu()
  pitcherMLBAMIds <- unique(data$pitcher)
  batterMLBAMIds <- unique(data$batter)

  pitcherFGIds <- ids %>% filter(key_mlbam %in% pitcherMLBAMIds) %>% select(key_fangraphs)
  batterFGIds <- ids %>% filter(key_mlbam %in% batterMLBAMIds) %>% select(key_fangraphs)

  pitcherGameLogs <- c()
  batterGameLogs <- c()

  pitcher_stat_cols <- c('FIP', 'xFIP-', 'K/9','BB/9', 'K/BB', 'HR/9', 'WHIP', 'BABIP',
                         'GB/FB', 'LD%', 'GB%', 'FB%', 'IFFB%', 'HR/FB', 'IFH%', 'Pull%', 'Cent%', 'Oppo%',
                         'Soft%', 'Med%', 'Hard%', 'Barrel%', 'wFB/C', 'wSL/C', 'wCT/C', 'wCB/C', 'wCH/C', 'wSF/C', 'wKN/C',
                         'O-Swing%', 'Z-Swing%', 'Swing%', 'O-Contact%', 'Z-Contact%', 'Contact%', 'Zone%', 'F-Strike%', 'SwStr%')

  batter_stat_cols <- c('wOBA', 'wRC+', 'K%', 'BB%', 'BABIP', 'BB/K',
                        'GB/FB', 'LD%', 'GB%', 'FB%', 'IFFB%', 'HR/FB', 'IFH%', 'Pull%', 'Cent%', 'Oppo%',
                        'Soft%', 'Med%', 'Hard%', 'Barrel%', 'wFB/C', 'wSL/C', 'wCT/C', 'wCB/C', 'wCH/C', 'wSF/C', 'wKN/C',
                        'O-Swing%', 'Z-Swing%', 'Swing%', 'O-Contact%', 'Z-Contact%', 'Contact%', 'Zone%', 'F-Strike%', 'SwStr%')

  for (i in seq_len(nrow(dates))) {
    year <- dates[i,]$year
    daySeq <- data.frame(Date = seq(ymd(dates[i,]$start), ymd(dates[i,]$end), by= 'day'))

    pitcherGameLogs <- rbind(pitcherGameLogs, do.call(
      rbind,
      apply(pitcherFGIds, 1, function(id) {
        fg_pitcher_game_logs(id, year) %>%
          summarise_game_logs(., id, count_col = IP, stat_col = pitcher_stat_cols)
      })))

    batterGameLogs <- rbind(batterGameLogs, do.call(
      rbind,
      batterGameLogs <-
        apply(batterFGIds, 1, function(id) {
          logBat <- fg_batter_game_logs(id, year) %>%
            summarise_game_logs(., id, count_col = PA, stat_col = batter_stat_cols)
        })))
  }


  pitcherGameLogs <- pitcherGameLogs %>% left_join(ids %>% select(key_mlbam, key_fangraphs), by=c("playerid" = "key_fangraphs"))
  batterGameLogs <- batterGameLogs %>% left_join(ids %>% select(key_mlbam, key_fangraphs), by=c("playerid" = "key_fangraphs"))

  data_join <- data %>%
    filter(pitches_seen_at_bat == 1) %>%
    left_join(
      pitcherGameLogs %>%
        select(Date, IP, season, key_mlbam, IP_cumsum, {{pitcher_stat_cols}}) %>%
        mutate(
          key_mlbam = as.numeric(key_mlbam),
          Date = as.character(Date)
        ),
      by=c("pitcher" = "key_mlbam", "game_date" ="Date")) %>%
    left_join(
      batterGameLogs %>%
        select(Date, PA, season, key_mlbam, PA_cumsum, {{batter_stat_cols}})%>%
        mutate(
          key_mlbam = as.numeric(key_mlbam),
          Date = as.character(Date),
        ),
      by=c("batter" = "key_mlbam", "game_date" ="Date"),
      suffix = c(".pitcher", ".batter")
    )
}

# join_pitch_data <- function(data) {
#   ave_pitch_speed <- data %>%
#     ungroup() %>%
#     select(pitcher, pitch_type, release_speed) %>%
#     group_by(pitcher, pitch_type) %>%
#     summarise(meanSpeed = mean(release_speed)) %>%
#     mutate(pitch_type = ifelse(pitch_type == "", 'Other', pitch_type)) %>%
#     drop_na()
#
#   return(data %>%
#            left_join(ave_pitch_speed, by="pitcher") %>%
#            filter(pitch_type.y == pitch_type.x) %>%
#            mutate(speed_percent_change = effective_speed / meanSpeed))
#
# }

writeRawData <- function(dates) {
  raw <- tibble()

  for (i in seq_len(nrow(dates))) {
    start_date <- as_datetime(dates$start[i])
    end_data <- as_datetime(dates$end[i])
    download_pbp <- get_statcast_data(start_date, end_data, player_type="pitcher")
    raw <- rbind(raw, download_pbp)
  }

  write.csv(raw, "raw.csv")
}

cleanData <- function(data) {
  data <- clean_statcast_data(data)
  data <- join_pitch_speed_data(data)
  write.csv(data, "pbp.csv")
  return(data)
}



getRawData <- function(dir, dates,  useCSV = F) {
  setwd(dir)
  if (useCSV) {
    data <- read.csv("raw-2020.csv")
  } else {
    writeRawData(dates)
    data <- read.csv("raw.csv")
  }

  return(data)
}
