

reformat_date <- function(dates) {
  dates <- str_extract_all(dates, "\\d\\d")
  new <- list()
  for (i in 1:length(dates)) {
    if (!is.na(dates[i])) {
      t <- dates[i] %>% unlist()
      m <-  t[2]
      d <-  t[1]
      if (as.numeric(t[3]) >= 0 &&
          as.numeric(t[3]) <= as.numeric(str_extract(Sys.Date(), '\\d\\d\\d\\d') %>%
                                         str_sub(3, 4))) {
        y <- paste0('20', t[3])
      }
      else{
        y <- paste0('19', t[3])
      }
    }
    else{
      y = m = d = NA
    }
    new <- append(new, as.character(paste0(y, '-', m, '-', d)))
  }
  return(unlist(new))
}


add_surface <- function(data) {
  surfaces <- c('Hard', 'Clay', 'I. hard', 'Carpet', 'Grass', 'Acrylic')
  opponents_surface_percentage <- list()
  opponents_favourite_ground   <- list()
  players_surface_percentage   <- list()
  players_favourite_ground     <- list()
  for (i in 1:length(data$date)) {
    t <- data[i, ]
    s <- t$Surface
    if (is.na(t$opponents_name)) {
      return(NA)
    }
    else{
      osp <-
        as.character(t[paste0('opponents_', s)]) %>% str_split('/') %>% unlist() %>%
        as.numeric()
      opponents_surface_percentage[i] <- osp[1] / (osp[1] + osp[2]) * 100
      ofg <- t[paste0('opponents_', surfaces)]
      ofgv <- ofg %>% str_split('/') %>% unlist() %>% as.numeric()
      ofgv <- c(ofgv[1] + ofgv[2],
                ofgv[3] + ofgv[4],
                ofgv[5] + ofgv[6],
                ofgv[7] + ofgv[8],
                ofgv[9] + ofgv[10],
                ofgv[11] + ofgv[12])
      ofg <- data.frame(surface = surfaces, value = ofgv)
      v <- ofg[which.max(ofg$value), ]
      if (!nrow(v) == 0) {
        opponents_favourite_ground[i] <- v$surface
      }
      psp <-
        as.character(t[paste0('players_', s)]) %>% str_split('/') %>% unlist() %>%
        as.numeric()
      players_surface_percentage[i] <- psp[1] / (psp[1] + psp[2]) * 100
      pfg <- t[paste0('players_', surfaces)]
      pfgv <- pfg %>% str_split('/') %>% unlist() %>% as.numeric()
      pfgv <- try(c(pfgv[1] + pfgv[2],
                    pfgv[3] + pfgv[4],
                    pfgv[5] + pfgv[6],
                    pfgv[7] + pfgv[8],
                    pfgv[9] + pfgv[10],
                    pfgv[11] + pfgv[12]))
      pfg <- data.frame(surface = surfaces, value = pfgv)
      players_favourite_ground[i] <-
        pfg[which.max(pfg$value), ]$surface
    }
  }
  data$opponents_surface_percentage <- opponents_surface_percentage
  data$opponents_favourite_ground   <- opponents_favourite_ground
  data$players_surface_percentage   <- players_surface_percentage
  data$players_favourite_ground     <- players_favourite_ground
  return(data)
}

add_sets <- function(data) {
  t <- str_extract_all(data$count, pattern = '\\d\\-\\d')
  won <- numeric()
  lost <- numeric()
  for (i in 1:length(t)) {
    temp <- unlist(t[i])
    w <- numeric()
    l <- numeric()
    for (j in 1:length(temp)) {
      w[j] <- str_sub(temp[j], 1, 1) > str_sub(temp[j], 3, 3)
      l[j] <- str_sub(temp[j], 1, 1) < str_sub(temp[j], 3, 3)
    }
    won[i]  <- sum(w)
    lost[i] <- sum(l)
  }
  data$'Set#'    <- won + lost
  w <- numeric()
  l <- numeric()
  temp <- data$`Match Outcome`
  for (i in 1:length(temp)) {
    if (temp[i] == 'win') {
      w[i] <- max(won[i], lost[i])
      l[i] <- min(won[i], lost[i])
    }
    else{
      w[i] <- min(won[i], lost[i])
      l[i] <- max(won[i], lost[i])
    }
  }
  data$sets_win  <- w
  data$sets_lost <- l
  return(data)
}

get_total_games <- function(x) {
  x_numbers <- regmatches(x, gregexpr("[[:digit:]]+", x))
  for (i in 1:length(x_numbers)) {
    y <- unlist(x_numbers[i])
    x_numbers[i] <- sum(as.numeric(substr(y, 1, 1)))
  }
  return(as.numeric(x_numbers))
}


get_avg_games <- function(x) {
  x_numbers <- regmatches(x, gregexpr("[[:digit:]]+", x))
  for (i in 1:length(x_numbers)) {
    y <- unlist(x_numbers[i])
    x_numbers[i] <- sum(as.numeric(substr(y, 1, 1)))
  }
  return(round(mean(as.numeric(x_numbers)), 2))
}


