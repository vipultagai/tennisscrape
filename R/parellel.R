library(dplyr)
library(ggplot2)
library(readxl)
library(plotly)
library(shiny)
library(shinydashboard)
library(tidyr)
library(rvest)
library(stringr)
library(foreach)
library(doSNOW)
latest_ranking_available <- function(association = 'atp') {
  if (association == 'atp') {
    ATP <- read_html('https://www.tennislive.net/atp/ranking/')
  }
  else{
    ATP <- read_html('https://www.tennislive.net/wta/ranking/')
  }
  atp_dates <-
    ATP %>% html_element('select') %>% html_elements('option') %>%
    html_attr('value')
  return(atp_dates)
}

get_rankings <- function(date = "2021-04-05", type = "atp") {
  if (type == "wta") {
    url <- paste0("https://www.tennislive.net/wta/ranking/EN/", date, "/")
  }
  else{
    url <- paste0("https://www.tennislive.net/atp/ranking/EN/", date, "/")
  }
  temp <- read_html(url) %>% html_elements("table")
  ranks <- temp[[1]] %>% html_table()
  ranks$country_code <- str_extract(ranks$X2, "[A-Z][A-Z][A-Z]")
  ranks$age <- str_extract(ranks$X2, "\\d\\d")
  ranks$Name <-
    str_remove(ranks$X2, " \\([A-Z][A-Z][A-Z]\\) \\(\\d\\d years\\)")
  ranks <- subset(ranks, select = -c(X2))
  ranks <- ranks[-1, ]
  ranks$X1 <- str_remove(ranks$X1, "\\.")
  ranks <- rename(ranks, rank = X1, points = X3)
  ranks$link <- temp[[1]] %>% html_elements("a") %>% html_attr("href")
  ranks$date <- date
  return(ranks)
}

get_player_data_par <-
  function(association = 'ATP',
           link = NULL,
           type = 'players') {
    library(rvest)
    library(stringr)
    library(tidyr)
    library(zoo)
    library(dplyr)
    temp <- read_html(link)
    stats <-
      temp %>% html_element('table.table_stats') %>% html_table(header = TRUE)
    temp <-
      temp %>% html_element("div.player_stats") %>% html_text2() %>% str_split("\n") %>%
      unlist()
    temp <-
      suppressWarnings(data.frame(temp) %>% separate(temp, c("col", "value"), sep = ":"))
    new <- data.frame(link = link)
    new$name <- as.character(filter(temp, col == "Name")["value"])
    new$points <- as.character(filter(temp, col == "Points")["value"])
    if (association == "ATP") {
      new$rank <- as.character(filter(temp, col == "ATP ranking")["value"])
    }
    if (association == 'WTA') {
      new$rank <- as.character(filter(temp, col == "WTA ranking")["value"])
    }
    new$age <-
      as.character(filter(temp, col == "Birthdate")["value"]) %>% substr(12, 13)
    new$height <- as.character(filter(temp, col == "Height")["value"])
    new$weight <- as.character(filter(temp, col == "Weight")["value"])
    new$country <-
      as.character(filter(temp, col == "Country")["value"])
    new$birthdate <-
      as.character(filter(temp, col == "Birthdate")["value"]) %>%
      str_extract("\\d\\d\\.\\d\\d\\.\\d\\d")
    new$arm <-
      as.character(filter(temp, col == "Right-handed" |
                            col == "Left-handed")["col"])
    new["prizemoney"] <-
      as.character(filter(temp, col == "Prize money")["value"])
    new["matches_total"]  <-
      as.character(filter(temp, col == "Matches total")["value"])
    new$total_wins <- as.character(filter(temp, col == "Win")["value"])
    new["total_win_percentage"] <-
      as.character(filter(temp, col == "%")["value"])
    new <- cbind(new, stats %>% filter(year == 'TOTAL'))
    new$year <- NULL
    names(new) <- paste0(type, '_', names(new))
    return(list(bio = new, stats = stats))
  }


get_matches_par <- function(link,
                            association = "ATP",
                            conn = 5) {
  get_player_data_par <-
    function(association = 'ATP',
             link = NULL,
             type = 'players') {
      library(rvest)
      library(stringr)
      library(tidyr)
      library(zoo)
      library(dplyr)
      temp <- read_html(link)
      stats <-
        temp %>% html_element('table.table_stats') %>% html_table(header = TRUE)
      temp <-
        temp %>% html_element("div.player_stats") %>% html_text2() %>% str_split("\n") %>%
        unlist()
      temp <-
        suppressWarnings(data.frame(temp) %>% separate(temp, c("col", "value"), sep = ":"))
      new <- data.frame(link = link)
      new$name <- as.character(filter(temp, col == "Name")["value"])
      new$points <- as.character(filter(temp, col == "Points")["value"])
      if (association == "ATP") {
        new$rank <- as.character(filter(temp, col == "ATP ranking")["value"])
      }
      if (association == 'WTA') {
        new$rank <- as.character(filter(temp, col == "WTA ranking")["value"])
      }
      new$age <-
        as.character(filter(temp, col == "Birthdate")["value"]) %>% substr(12, 13)
      new$height <- as.character(filter(temp, col == "Height")["value"])
      new$weight <- as.character(filter(temp, col == "Weight")["value"])
      new$country <-
        as.character(filter(temp, col == "Country")["value"])
      new$birthdate <-
        as.character(filter(temp, col == "Birthdate")["value"]) %>%
        str_extract("\\d\\d\\.\\d\\d\\.\\d\\d")
      new$arm <-
        as.character(filter(temp, col == "Right-handed" |
                              col == "Left-handed")["col"])
      new["prizemoney"] <-
        as.character(filter(temp, col == "Prize money")["value"])
      new["matches_total"]  <-
        as.character(filter(temp, col == "Matches total")["value"])
      new$total_wins <-
        as.character(filter(temp, col == "Win")["value"])
      new["total_win_percentage"] <-
        as.character(filter(temp, col == "%")["value"])
      new <- cbind(new, stats %>% filter(year == 'TOTAL'))
      new$year <- NULL
      names(new) <- paste0(type, '_', names(new))
      return(list(bio = new, stats = stats))
    }
  
  temp <- read_html(link) %>% html_elements("table.table_pmatches")
  table <- temp[[2]] %>% html_table()
  suppressWarnings(if (table$X1 == "no matches found") {
    return(NULL)
  })
  table$X6 <- NULL
  table$X7 <- NULL
  names(table) <-
    c(
      'date',
      'Match',
      'player1_name',
      'player2_name',
      'count',
      'tournament_place',
      'Surface'
    )
  tbody <- temp[[2]] %>% html_elements("tr")
  tournament <-
    tbody %>% html_element('td.w200') %>% html_element('a') %>%
    html_attr('title') %>% na.locf()
  `Match Outcome` <-
    tbody %>% html_elements('td.w16') %>% html_elements('img') %>% html_attr('alt')
  opponent_links <-
    tbody %>% html_elements('td.w130') %>% html_elements('a')
  opponent_names <- opponent_links %>% html_text2()
  opponent_links <- opponent_links %>% html_attr('href') %>% unique()
  if (length(opponent_links) >= 1) {
    temp <- foreach(i = 1:length(opponent_links)) %dopar%
      try(get_player_data_par(association = association,
                              link = opponent_links[i],
                              type = 'opponents'))
    opponent_data  <- temp[[1]]$bio
    if (length(temp) > 1) {
      for (i in 2:length(temp)) {
        if (length(temp[[i]]) == 2) {
          opponent_data <- rbind(opponent_data , temp[[i]]$bio)
        }
      }
    }
    opponent_data$opponents_name <-
      str_trim(opponent_data$opponents_name, "both")
    opponent_data <-
      left_join(data.frame(opponents_name = opponent_names),
                opponent_data)
    table <- cbind(table, opponent_data)
    table$tournament <- tournament
    table$`Match Outcome` <- `Match Outcome`
  }
  return(table)
}
