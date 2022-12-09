#From baseballR, fixed
daily_pitcher <- function(t1, t2) {
  tryCatch(expr = {
    payload <- paste0("http://www.baseball-reference.com/leagues/daily.cgi?user_team=&bust_cache=&type=p&lastndays=7&dates=fromandto&fromandto=", t1, ".", t2, "&level=mlb&franch=&stat=&stat_value=0") %>% xml2::read_html()
    df <- payload %>% rvest::html_elements(xpath = "//*[@id=\"daily\"]") %>% rvest::html_table(fill = TRUE)
    df <- as.data.frame(df)[-c(1, 3, 5)]
    names(df)[1 : 4] <- c("Name", "Age", "Level", "Team")
    suppressWarnings(df[, c(2, 5 : 29, 36 : 39)] <- lapply(df[, c(2, 5 : 29, 36 : 39)], as.numeric))
    df$X1B <- with(df, H - (X2B + X3B + HR))
    season <- substr(t1, 1, 4)
    df$season <- as.integer(season)
    df$uBB <- with(df, BB - IBB)
    suppressWarnings(df[, 30] <- as.numeric(sub("%", "", df[, 30])))
    suppressWarnings(df[, 31] <- as.numeric(sub("%", "", df[, 31])))
    suppressWarnings(df[, 32] <- as.numeric(sub("%", "", df[, 32])))
    suppressWarnings(df[, 33] <- as.numeric(sub("%", "", df[, 33])))
    suppressWarnings(df[, 34] <- as.numeric(sub("%", "", df[, 34])))
    suppressWarnings(df[, 35] <- as.numeric(sub("%", "", df[, 35])))
    df[, c(30 : 35)] <- df[, c(30 : 35)] / 100
    df <- df[, c(41, 1 : 13, 42, 14 : 19, 40, 20 : 39)]
    df$SO_perc <- with(df, round(SO / BF, 3))
    df$uBB_perc <- with(df, round(uBB / BF, 3))
    df$SO_uBB <- with(df, round(SO_perc - uBB_perc))
    df$Team <- gsub(" $", "", df$Team, perl = T)
    df <- df %>% dplyr::filter(.data$Name != "Name") %>% dplyr::arrange(desc(.data$IP), desc(.data$WHIP))
    playerids <- payload %>% rvest::html_elements("table") %>% rvest::html_elements("a") %>% rvest::html_attr("href") %>% as.data.frame() %>% dplyr::rename(slug = .data$.) %>% dplyr::filter(grepl("redirect", .data$slug)) %>% dplyr::mutate(playerid = gsub("/redirect.fcgi\\?player=1&mlb_ID=", "", .data$slug))
    df <- df %>% dplyr::mutate(bbref_id = playerids$playerid) %>% dplyr::select(.data$bbref_id, tidyr::everything())
    return(df)
  }, error = function(e) {
    message(glue::glue("{Sys.time()}: Invalid arguments or no daily pitcher data available!"))
  }, warning = function(w) {

  }, finally = {
    return(df)
  })

}

#From baseballR, fixed
daily_batter <- function(t1, t2) {
  tryCatch(expr = {
    payload <- xml2::read_html(paste0("http://www.baseball-reference.com/leagues/daily.cgi?user_team=&bust_cache=&type=b&lastndays=7&dates=fromandto&fromandto=", t1, ".", t2, "&level=mlb&franch=&stat=&stat_value=0"))
    df <- payload %>% rvest::html_elements(xpath = "//*[@id=\"daily\"]") %>% rvest::html_table(fill = TRUE)
    df <- as.data.frame(df)[-c(1, 3, 5)]
    names(df)[1 : 4] <- c("Name", "Age", "Level", "Team")
    suppressWarnings(df[, c(2, 5 : 26)] <- lapply(df[, c(2, 5 : 26)], as.numeric))
    df$X1B <- with(df, H - (X2B + X3B + HR))
    season <- substr(t1, 1, 4)
    df$season <- as.integer(season)
    df$uBB <- with(df, BB - IBB)
    df <- df[, c(28, 1 : 9, 27, 10 : 15, 29, 16 : 26)]
    df$Team <- gsub(" $", "", df$Team, perl = T)
    df <- df %>% dplyr::filter(.data$Name != "Name")
    playerids <- payload %>% rvest::html_elements("table") %>% rvest::html_elements("a") %>% rvest::html_attr("href") %>% as.data.frame() %>% dplyr::rename(slug = ".") %>% dplyr::filter(grepl("redirect", .data$slug)) %>% dplyr::mutate(playerid = gsub("/redirect.fcgi\\?player=1&mlb_ID=", "", .data$slug))
    df <- df %>% dplyr::mutate(bbref_id = playerids$playerid) %>% dplyr::select(.data$bbref_id, tidyr::everything())
    return(df)
  }, error = function(e) {
    message(glue::glue("{Sys.time()}: Invalid arguments or no daily batter data available!"))
    print(e)
  }, warning = function(w) {
  }, finally = {
    return(df)
  })
}

summarise_game_logs <- function(data, id, stat_col = NULL, count_col = NULL) {
  if (nrow(data) == 0 ) {
    return()
  }
  data[stat_col[!(stat_col %in% colnames(data))]] <- 0
  data %>%
    select(playerid, Date, {{count_col}}, {{stat_col}}) %>%
    mutate(Date = as.Date(Date)) %>%
    right_join(daySeq, by = "Date") %>%
    mutate(across({{stat_col}} , ~ replace_na(.x, 0)),
           across({{count_col}}, ~ replace_na(.x, 0)),
           playerid = replace_na(playerid, id),
           season = year
    ) %>%
    arrange(Date) %>%
    mutate(across({{stat_col}}, ~ cumsum({{count_col}} * .x) / cumsum({{count_col}})),
           across({{stat_col}}, ~ ifelse(is.nan(.x), 0, .x)),
           across({{count_col}}, list(cumsum=cumsum), .names = "{col}_{fn}"))
}