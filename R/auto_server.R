autoServer <- function(id) {
  moduleServer(id,
               function(input, output, session) {
                 #ranks
                 ranks <-
                   reactiveValues(
                     atp = readRDS('rds_data/atp_ranks.rds'),
                     wta = readRDS('rds_data/wta_ranks.rds'),
                     atp_l_a = max(latest_ranking_available('atp')),
                     wta_l_a = max(latest_ranking_available('wta'))
                   )
                 player_data <- reactiveValues(p1 = NULL, p2 = NULL)
                 
                 output$ala    <-
                   renderText({
                     paste0('ATP Latest Available : ', ranks$atp_l_a)
                   })
                 output$wla    <-
                   renderText({
                     paste0('WTA Latest Available : ', ranks$wta_l_a)
                   })
                 output$a_last <- renderText({
                   temp <- ranks$atp
                   paste0('ATP Last Updated : ', max(temp$date))
                 })
                 output$w_last <- renderText({
                   temp <- ranks$wta
                   paste0('WTA Last Updated : ', max(temp$date))
                 })
                 
                 output$atp_ranks_out <- renderDataTable(ranks$atp)
                 output$wta_ranks_out <- renderDataTable(ranks$wta)
                 
                 observeEvent(input$update_ranks, {
                   withProgress(
                     min = 0,
                     max = 1,
                     value = 0,
                     message = 'Updating Ranks',
                     style = getShinyOption("progress.style", default = "notification"),
                     {
                       incProgress(1, 'Reading Data')
                       atp_ranks <- ranks$atp
                       wta_ranks <- ranks$wta
                       incProgress(1, 'Finding Last Update Date')
                       a_last <- max(atp_ranks$date)
                       w_last <- max(wta_ranks$date)
                       incProgress(1, 'Fetching Latest Available Dates')
                       a_latest <- ranks$atp_l_a
                       w_latest <- ranks$wta_l_a
                       if (a_latest > a_last) {
                         incProgress(1, 'Updating ATP Ranking Data')
                         ranks$atp <- get_rankings(a_latest, 'atp')
                         saveRDS(ranks$atp, 'rds_data/atp_ranks.rds')
                       }
                       if (w_latest > w_last) {
                         incProgress(1, 'Updating WTA Ranking Data')
                         ranks$wta <- get_rankings(w_latest, 'wta')
                         saveRDS(ranks$wta, 'rds_data/wta_ranks.rds')
                       }
                     }
                   )
                 })
                 
                 
                 #compare
                 output$p1_source_out <- renderUI({
                   req(input$p1_association)
                   req(input$p1_source)
                   ns <- NS(id)
                   if (input$p1_source == 'File') {
                     return(fileInput(ns('p1_data'), 'Upload File'))
                   }
                   if (input$p1_source == 'Fetch by Link') {
                     return(
                       textInput(ns('p1_data'), 'Enter Player Link',
                                 placeholder = 'https://www.tennislive.net/atp/alejandro-davidovich-fokina/')
                     )
                   }
                   if (input$p1_source == 'Player Name') {
                     if (input$p1_association == 'ATP') {
                       choices = as.list(ranks$atp['Name'])
                     }
                     if (input$p1_association == 'WTA') {
                       choices = as.list(ranks$wta['Name'])
                     }
                     return(
                       selectizeInput(
                         ns('p1_data'),
                         'Select Player',
                         choices = choices,
                         selected = NULL,
                         options = list(create = TRUE, maxOptions = 5)
                       )
                     )
                   }
                 })
                 
                 output$p2_source_out <- renderUI({
                   req(input$p2_association)
                   req(input$p2_source)
                   ns <- NS(id)
                   if (input$p2_source == 'File') {
                     return(fileInput(ns('p2_data'), 'Upload File'))
                   }
                   if (input$p2_source == 'Fetch by Link') {
                     return(
                       textInput(ns('p2_data'), 'Enter Player Link',
                                 placeholder = 'https://www.tennislive.net/atp/alejandro-davidovich-fokina/')
                     )
                   }
                   if (input$p2_source == 'Player Name') {
                     if (input$p2_association == 'ATP') {
                       choices = as.list(ranks$atp['Name'])
                     }
                     if (input$p2_association == 'WTA') {
                       choices = as.list(ranks$wta['Name'])
                     }
                     return(
                       selectizeInput(
                         ns('p2_data'),
                         'Select Player',
                         choices = choices,
                         selected = NULL,
                         options = list(create = TRUE, maxOptions = 5)
                       )
                     )
                   }
                 })
                 
                 observeEvent(input$p1_data_get, {
                   req(input$p1_association)
                   req(input$p1_source)
                   req(input$p1_data)
                   print(input$p1_association)
                   print(input$p1_source)
                   print(input$p1_data)
                   
                   if (input$p1_source == 'File') {
                     file <- input$p1_data
                     data <- read_excel(file$datapath)
                     data <- dplyr::arrange(data, desc(date))
                     write.xlsx(data, 'player1.xlsx', row.names = FALSE)
                   }
                   if (input$p1_source == 'Fetch by Link' |
                       input$p1_source == 'Player Name') {
                     link = input$p1_data
                     association = input$p1_association
                     
                     if (input$p1_source == 'Player Name') {
                       print("source = Player Name")
                       if (input$p1_association == 'ATP') {
                         link <- filter(ranks$atp, Name == input$p1_data)
                       }
                       if (input$p1_association == 'WTA') {
                         link <- filter(ranks$wta, Name == input$p1_data)
                       }
                       link <- link$link
                     }
                     
                     withProgress(
                       min = 0,
                       max = 100,
                       value = 0,
                       message = 'Initializing',
                       {
                         start <- Sys.time()
                         player_data <-
                           get_player_data_par(association, link, type = 'players')
                         matches <- NULL
                         conn = 6
                         incProgress(0.5,
                                     paste0('Building Cluster of ', conn, ' processes.'))
                         t <- Sys.time()
                         cluster = makeCluster(conn, type = "SOCK")
                         registerDoSNOW(cluster)
                         print(paste0("Number of parellel processes = ", conn))
                         print(paste0("Time to create processes     =", Sys.time() -
                                        t))
                         incProgress(0.5,
                                     paste0('Cluster of ', conn, ' processes initialized.'))
                         for (i in 1:6) {
                           print(paste0("Surface Category = ", i))
                           t <-
                             get_matches_par(paste0(link, '?su=', i), association, conn = 20)
                           if (!is.null(t)) {
                             incProgress((1 / 6) * 98, message = paste0(round((i / 6) * 98), " % Completed"))
                             if (is.null(matches)) {
                               matches <- t
                             }
                             else{
                               matches <- rbind(matches, t)
                             }
                           }
                         }
                         stopCluster(cluster)
                         
                         matches <- cbind(matches, player_data$bio)
                         matches$date <- reformat_date(matches$date)
                         matches[matches == 'character(0)'] <- NA
                         matches$opponents_birthdate <-
                           reformat_date(matches$opponents_birthdate)
                         matches$players_birthdate <-
                           reformat_date(matches$players_birthdate)
                         matches <- add_surface(matches)
                         matches <- add_sets(matches)
                         matches$winner <- matches$opponents_name
                         matches$winner[matches$`Match Outcome` == 'win'] <-
                           matches$players_name[1]
                         matches$players_surface_percentage <-
                           paste0(round(as.numeric(
                             matches$players_surface_percentage
                           ), 2), " %")
                         matches$opponents_surface_percentage <-
                           paste0(round(
                             as.numeric(matches$opponents_surface_percentage),
                             2
                           ), " %")
                         matches$age <- matches$players_age
                         matches$`player years on tour` <-
                           matches$date %>% max() %>% str_sub(1, 4) %>% as.numeric() -
                           matches$date %>% min() %>% str_sub(1, 4) %>% as.numeric()
                         matches$opponents_rank <-
                           as.numeric(matches$opponents_rank)
                         matches$`Match Outcome`[matches$`Match Outcome` == 'lost'] <-
                           'lose'
                         matches$player_total_games <-
                           get_total_games(matches$count)
                         write.xlsx(matches, 'player1.xlsx', row.names = FALSE)
                         print(paste0("totaltime = ", Sys.time() - start))
                       }
                     )
                   }
                 })
                 
                 output$p1_down_btn <- downloadHandler(
                   filename = function() {
                     data <- read_excel('player1.xlsx')
                     name <- data$players_name[1]
                     paste(Sys.Date(), name, ".xlsx", sep = "_")
                   },
                   content = function(file) {
                     data <- read_excel('player1.xlsx')
                     write.xlsx(as.data.frame(data), file, row.names = FALSE)
                   }
                 )
                 
                 
                 observeEvent(input$p2_data_get, {
                   req(input$p2_association)
                   req(input$p2_source)
                   req(input$p2_data)
                   if (input$p2_source == 'File') {
                     file <- input$p2_data
                     data <- read_excel(file$datapath)
                     data <- dplyr::arrange(data, desc(date))
                     write.xlsx(data, 'player2.xlsx', row.names = FALSE)
                   }
                   if (input$p2_source == 'Fetch by Link' |
                       input$p2_source == 'Player Name') {
                     link = input$p2_data
                     association = input$p2_association
                     if (input$p2_source == 'Player Name') {
                       if (input$p2_association == 'ATP') {
                         link <- filter(ranks$atp, Name == input$p2_data)
                       }
                       if (input$p2_association == 'WTA') {
                         link <- filter(ranks$wta, Name == input$p2_data)
                       }
                       link <- link$link
                     }
                     withProgress(
                       min = 0,
                       max = 100,
                       value = 0,
                       message = 'Initializing',
                       {
                         start <- Sys.time()
                         player_data <-
                           get_player_data_par(association, link, type = 'players')
                         matches <- NULL
                         conn = 6
                         incProgress(0.5,
                                     paste0('Building Cluster of ', conn, ' processes.'))
                         t <- Sys.time()
                         cluster = makeCluster(conn, type = "SOCK")
                         registerDoSNOW(cluster)
                         print(paste0("Number of parellel processes = ", conn))
                         print(paste0("Time to create processes", Sys.time() - t))
                         incProgress(0.5,
                                     paste0('Cluster of ', conn, ' processes initialized.'))
                         
                         for (i in 1:6) {
                           print(paste0("Surface Category = ", i))
                           if (is.null(t)) {
                             next
                           }
                           t <-
                             get_matches_par(paste0(link, '?su=', i), association, conn = 5)
                           if (is.data.frame(t)) {
                             if (is.null(matches)) {
                               matches <- t
                             }
                             else{
                               matches <- rbind(matches, t)
                             }
                             incProgress((1 / 6) * 98, message = paste0(round((i / 6) *
                                                                                98), " % Finished"))
                           }
                         }
                         stopCluster(cluster)
                         print("downloads completed")
                         matches <- cbind(matches, player_data$bio)
                         matches$date <- reformat_date(matches$date)
                         matches[matches == 'character(0)'] <- NA
                         matches$opponents_birthdate <-
                           reformat_date(matches$opponents_birthdate)
                         matches$players_birthdate <-
                           reformat_date(matches$players_birthdate)
                         matches <- add_surface(matches)
                         matches <- add_sets(matches)
                         matches$winner <- matches$opponents_name
                         matches$winner[matches$`Match Outcome` == 'win'] <-
                           matches$players_name[1]
                         matches$players_surface_percentage <-
                           paste0(round(as.numeric(
                             matches$players_surface_percentage
                           ), 2), " %")
                         matches$opponents_surface_percentage <-
                           paste0(round(
                             as.numeric(matches$opponents_surface_percentage),
                             2
                           ), " %")
                         matches$age <- matches$players_age
                         matches$`player years on tour` <-
                           matches$date %>% max() %>% str_sub(1, 4) %>% as.numeric() -
                           matches$date %>% min() %>% str_sub(1, 4) %>% as.numeric()
                         matches$opponents_rank <-
                           as.numeric(matches$opponents_rank)
                         matches$`Match Outcome`[matches$`Match Outcome` == 'lost'] <-
                           'lose'
                         matches$player_total_games <-
                           get_total_games(matches$count)
                         player_data$p2 <- matches
                         write.xlsx(matches, 'player2.xlsx', row.names = FALSE)
                         print(paste0("totaltime = ", Sys.time() - start))
                         incProgress(1, message = "Finished")
                       }
                     )
                   }
                 })
                 output$p2_down_btn <- downloadHandler(
                   filename = function() {
                     data <- read_excel('player2.xlsx')
                     name <- data$players_name[1]
                     paste(Sys.Date(), name, ".xlsx", sep = "_")
                   },
                   content = function(file) {
                     data <- read_excel('player2.xlsx')
                     write.xlsx(as.data.frame(data), file, row.names = FALSE)
                   }
                 )
                 
                 
                 observeEvent(input$show_head_to_head, {
                   data1 <- try(read_excel('player1.xlsx'))
                   data2 <- try(read_excel('player2.xlsx'))
                   output$matches_played <-
                     renderText({
                       nrow(filter(data1, opponents_name == data2$players_name[1]))
                     })
                   output$matches_won1 <-
                     renderText({
                       nrow(
                         filter(
                           data1,
                           opponents_name == data2$players_name[1],
                           winner == data1$players_name[1]
                         )
                       )
                     })
                   output$matches_won2 <-
                     renderText({
                       nrow(
                         filter(
                           data1,
                           opponents_name == data2$players_name[1],
                           winner == data2$players_name[1]
                         )
                       )
                     })
                   
                 })
                 
                 
                 observeEvent(input$show_results1, {
                   data <- try(read_excel('player1.xlsx'))
                   
                   surfaces <- unique(data$Surface)
                   surfaces <- append(surfaces, "All")
                   updateSelectInput(
                     session = getDefaultReactiveDomain(),
                     'player1_ss',
                     'Select Surface',
                     choices = surfaces,
                     selected = 'All'
                   )
                   updateSliderInput(
                     session = getDefaultReactiveDomain(),
                     "slider1",
                     label = "Range",
                     value = c(0, max(data$opponents_rank, na.rm = TRUE)),
                     min = 0,
                     max = max(data$opponents_rank, na.rm = TRUE),
                     step = 1,
                   )
                   
                   output$player1_name <- renderText(data$players_name[1])
                   output$player1_rank <-
                     renderText(paste0("#", data$players_rank[1]))
                   output$player1_age <- renderText(data$age[1])
                   output$player1_country <-
                     renderText(data$players_country[1])
                   output$player1_arm <- renderText(data$players_arm[1])
                   output$player1_exp <-
                     renderText(data$`player years on tour`[1])
                   output$player1_fg <-
                     renderText(data$players_favourite_ground[1])
                   output$last_update1 <-
                     renderText(paste0("Last Updated: ", as.character(data$date[1])))
                   
                   output$player1_total <- renderText({
                     req(input$player1_ss)
                     req(input$slider1)
                     if (input$player1_ss == "All") {
                       x <- data
                     }
                     else{
                       x <- filter(data, Surface == input$player1_ss)
                     }
                     x <-
                       filter(
                         x,
                         opponents_rank >= as.numeric(input$slider1[1]),
                         opponents_rank <= as.numeric(input$slider1[2])
                       )
                     temp <- nrow(x)
                   })
                   
                   output$player1_avg_games <- renderText({
                     req(input$player1_ss)
                     req(input$slider1)
                     if (input$player1_ss == "All") {
                       x <- data
                     }
                     else{
                       x <- filter(data, Surface == input$player1_ss)
                     }
                     x <-
                       filter(
                         x,
                         opponents_rank >= as.numeric(input$slider1[1]),
                         opponents_rank <= as.numeric(input$slider1[2])
                       )
                     get_avg_games(x$count)
                   })
                   
                   
                   output$player1_win_p <- renderText({
                     req(input$player1_ss)
                     req(input$slider1)
                     if (input$player1_ss == "All") {
                       temp <- data$players_total_win_percentage[1]
                     }
                     else{
                       x <- filter(data, Surface == input$player1_ss)
                       x <-
                         filter(
                           x,
                           opponents_rank >= as.numeric(input$slider1[1]),
                           opponents_rank <= as.numeric(input$slider1[2])
                         )
                       temp <- x$players_surface_percentage[1]
                     }
                     
                   })
                   
                   output$player1_win_pm <- renderText({
                     req(input$player1_ss)
                     req(input$slider1)
                     if (input$player1_ss == "All") {
                       x <- data
                     }
                     else{
                       x <- filter(data,
                                   Surface == input$player1_ss,
                                   `Match Outcome` == 'win')
                     }
                     x <-
                       filter(
                         x,
                         opponents_rank >= as.numeric(input$slider1[1]),
                         opponents_rank <= as.numeric(input$slider1[2])
                       )
                     median(as.numeric(strsplit(
                       x$opponents_surface_percentage, ' %'
                     )), na.rm = TRUE)
                   })
                   
                   output$player1_lose_pm <- renderText({
                     req(input$player1_ss)
                     req(input$slider1)
                     if (input$player1_ss == "All") {
                       x <- data
                     }
                     else{
                       x <- filter(data,
                                   Surface == input$player1_ss,
                                   `Match Outcome` == 'lose')
                     }
                     x <-
                       filter(
                         x,
                         opponents_rank >= as.numeric(input$slider1[1]),
                         opponents_rank <= as.numeric(input$slider1[2])
                       )
                     median(as.numeric(strsplit(
                       x$opponents_surface_percentage, ' %'
                     )), na.rm = TRUE)
                   })
                   
                   output$player1_win_lose <- renderText({
                     req(input$player1_ss)
                     req(input$slider1)
                     if (input$player1_ss == "All") {
                       x <- data
                     }
                     else{
                       x <- filter(data, Surface == input$player1_ss)
                     }
                     x <-
                       filter(
                         x,
                         opponents_rank >= as.numeric(input$slider1[1]),
                         opponents_rank <= as.numeric(input$slider1[2])
                       )
                     p1 = paste0(nrow(filter(x, `Match Outcome` == "win")),
                                 "/",
                                 nrow(filter(x, `Match Outcome` == "lose")),
                                 " (",
                                 round(nrow(
                                   filter(x, `Match Outcome` == "win")
                                 ) / nrow(
                                   filter(x, `Match Outcome` == "lose")
                                 ), 2),
                                 ")")
                   })
                   
                   output$player1_plot <- renderPlotly({
                     req(input$player1_ss)
                     req(input$slider1)
                     if (input$player1_ss == "All") {
                       x <- data
                     }
                     else{
                       x <- filter(data, Surface == input$player1_ss)
                     }
                     x <-
                       filter(
                         x,
                         opponents_rank >= as.numeric(input$slider1[1]),
                         opponents_rank <= as.numeric(input$slider1[2])
                       )
                     #x$opponents_surface_percentage <-as.numeric(strsplit(x$opponents_surface_percentage,' %'))
                     #x$players_surface_percentage <-as.numeric(strsplit(x$players_surface_percentage,' %'))
                     pal <- c("red", "blue")
                     fig <-
                       plot_ly(
                         data = x,
                         x = ~ opponents_rank,
                         y = ~ 1:nrow(x),
                         color = ~ `Match Outcome`,
                         colors = pal,
                         size = 1.4
                       )
                     fig <- fig %>% layout(yaxis = list(zeroline = FALSE),
                                           xaxis = list(zeroline = FALSE))
                   })
                   
                   output$player1_plot2 <- renderPlotly({
                     req(input$player1_ss)
                     req(input$slider1)
                     if (input$player1_ss == "All") {
                       x <- data
                     }
                     else{
                       x <- filter(data, Surface == input$player1_ss)
                     }
                     x <-
                       filter(
                         x,
                         opponents_rank >= as.numeric(input$slider1[1]),
                         opponents_rank <= as.numeric(input$slider1[2])
                       )
                     x$opponents_surface_percentage <-
                       as.numeric(strsplit(x$opponents_surface_percentage, ' %'))
                     pal <- c("red", "blue")
                     fig <-
                       plot_ly(
                         data = x,
                         x = ~ opponents_surface_percentage,
                         y = ~ 1:nrow(x),
                         color = ~ `Match Outcome`,
                         colors = pal,
                         size = 1.4
                       )
                     fig <- fig %>% layout(yaxis = list(zeroline = FALSE),
                                           xaxis = list(zeroline = FALSE))
                   })
                   
                   
                   output$player1_plot3 <- renderPlotly({
                     req(input$player1_ss)
                     req(input$slider1)
                     if (input$player1_ss == "All") {
                       x <- data
                     }
                     else{
                       x <- filter(data, Surface == input$player1_ss)
                     }
                     x <-
                       filter(
                         x,
                         opponents_rank >= as.numeric(input$slider1[1]),
                         opponents_rank <= as.numeric(input$slider1[2])
                       )
                     x$opponents_surface_percentage <-
                       as.numeric(strsplit(x$opponents_surface_percentage, ' %'))
                     pal <- c("red", "blue")
                     fig <-
                       plot_ly(
                         data = x,
                         x = ~ opponents_rank,
                         y = ~ player_total_games,
                         color = ~ `Match Outcome`,
                         colors = pal,
                         size = 1.4
                       )
                     fig <- fig %>% layout(yaxis = list(zeroline = FALSE),
                                           xaxis = list(zeroline = FALSE))
                   })
                   
                   
                 })
                 
                 observeEvent(input$show_results2, {
                   data <- try(read_excel('player2.xlsx'))
                   surfaces <- unique(data$Surface)
                   surfaces <- append(surfaces, "All")
                   updateSelectInput(
                     session = getDefaultReactiveDomain(),
                     'player2_ss',
                     'Select Surface',
                     choices = surfaces,
                     selected = 'All'
                   )
                   updateSliderInput(
                     session = getDefaultReactiveDomain(),
                     "slider2",
                     label = "Range",
                     value = c(0, max(data$opponents_rank, na.rm = TRUE)),
                     min = 0,
                     max = max(data$opponents_rank, na.rm = TRUE),
                     step = 1,
                   )
                   output$player2_name <- renderText(data$players_name[1])
                   output$player2_rank <-
                     renderText(paste0("#", data$players_rank[1]))
                   output$player2_age <- renderText(data$age[1])
                   output$player2_country <-
                     renderText(data$players_country[1])
                   output$player2_arm <- renderText(data$players_arm[1])
                   output$player2_exp <-
                     renderText(data$`player years on tour`[1])
                   output$player2_fg <-
                     renderText(data$players_favourite_ground[1])
                   output$last_update2 <-
                     renderText(paste0("Last Updated: ", as.character(data$date[1])))
                   
                   output$player2_total <- renderText({
                     req(input$player2_ss)
                     req(input$slider2)
                     if (input$player2_ss == "All") {
                       x <- data
                     }
                     else{
                       x <- filter(data, Surface == input$player2_ss)
                     }
                     x <-
                       filter(
                         x,
                         opponents_rank >= as.numeric(input$slider2[1]),
                         opponents_rank <= as.numeric(input$slider2[2])
                       )
                     temp <- nrow(x)
                   })
                   
                   output$player2_avg_games <- renderText({
                     req(input$player2_ss)
                     req(input$slider2)
                     if (input$player2_ss == "All") {
                       x <- data
                     }
                     else{
                       x <- filter(data, Surface == input$player2_ss)
                     }
                     x <-
                       filter(
                         x,
                         opponents_rank >= as.numeric(input$slider2[1]),
                         opponents_rank <= as.numeric(input$slider2[2])
                       )
                     get_avg_games(x$count)
                   })
                   
                   
                   output$player2_win_p <- renderText({
                     req(input$player2_ss)
                     req(input$slider2)
                     if (input$player2_ss == "All") {
                       temp <- data$players_total_win_percentage[1]
                     }
                     else{
                       x <- filter(data, Surface == input$player2_ss)
                       x <-
                         filter(
                           x,
                           opponents_rank >= as.numeric(input$slider2[1]),
                           opponents_rank <= as.numeric(input$slider2[2])
                         )
                       temp <- x$players_surface_percentage[1]
                     }
                     
                   })
                   
                   output$player2_win_pm <- renderText({
                     req(input$player2_ss)
                     req(input$slider1)
                     if (input$player2_ss == "All") {
                       x <- data
                     }
                     else{
                       x <- filter(data,
                                   Surface == input$player2_ss,
                                   `Match Outcome` == 'win')
                     }
                     x <-
                       filter(
                         x,
                         opponents_rank >= as.numeric(input$slider2[1]),
                         opponents_rank <= as.numeric(input$slider2[2])
                       )
                     median(as.numeric(strsplit(
                       x$opponents_surface_percentage, ' %'
                     )), na.rm = TRUE)
                   })
                   output$player2_lose_pm <- renderText({
                     req(input$player2_ss)
                     req(input$slider1)
                     if (input$player2_ss == "All") {
                       x <- data
                     }
                     else{
                       x <- filter(data,
                                   Surface == input$player2_ss,
                                   `Match Outcome` == 'lose')
                     }
                     x <-
                       filter(
                         x,
                         opponents_rank >= as.numeric(input$slider2[1]),
                         opponents_rank <= as.numeric(input$slider2[2])
                       )
                     median(as.numeric(strsplit(
                       x$opponents_surface_percentage, ' %'
                     )), na.rm = TRUE)
                   })
                   
                   output$player2_win_lose <- renderText({
                     req(input$player2_ss)
                     req(input$slider1)
                     if (input$player2_ss == "All") {
                       x <- data
                     }
                     else{
                       x <- filter(data, Surface == input$player2_ss, )
                     }
                     x <-
                       filter(
                         x,
                         opponents_rank >= as.numeric(input$slider2[1]),
                         opponents_rank <= as.numeric(input$slider2[2])
                       )
                     p1 = paste0(nrow(filter(x, `Match Outcome` == "win")),
                                 "/",
                                 nrow(filter(x, `Match Outcome` == "lose")),
                                 " (",
                                 round(nrow(
                                   filter(x, `Match Outcome` == "win")
                                 ) / nrow(
                                   filter(x, `Match Outcome` == "lose")
                                 ), 2),
                                 ")")
                   })
                   
                   output$player2_plot <- renderPlotly({
                     req(input$player2_ss)
                     req(input$slider1)
                     if (input$player2_ss == "All") {
                       x <- data
                     }
                     else{
                       x <- filter(data, Surface == input$player2_ss, )
                     }
                     x <-
                       filter(
                         x,
                         opponents_rank >= as.numeric(input$slider2[1]),
                         opponents_rank <= as.numeric(input$slider2[2])
                       )
                     #x$opponents_surface_percentage <-as.numeric(strsplit(x$opponents_surface_percentage,' %'))
                     #x$players_surface_percentage <-as.numeric(strsplit(x$players_surface_percentage,' %'))
                     pal <- c("red", "blue")
                     fig <-
                       plot_ly(
                         data = x,
                         x = ~ opponents_rank,
                         y = ~ 1:nrow(x),
                         color = ~ `Match Outcome`,
                         colors = pal,
                         size = 1.4
                       )
                     fig <- fig %>% layout(yaxis = list(zeroline = FALSE),
                                           xaxis = list(zeroline = FALSE))
                   })
                   
                   output$player2_plot2 <- renderPlotly({
                     req(input$player2_ss)
                     req(input$slider1)
                     if (input$player2_ss == "All") {
                       x <- data
                     }
                     else{
                       x <- filter(data, Surface == input$player2_ss, )
                     }
                     x <-
                       filter(
                         x,
                         opponents_rank >= as.numeric(input$slider2[1]),
                         opponents_rank <= as.numeric(input$slider2[2])
                       )
                     x$opponents_surface_percentage <-
                       as.numeric(strsplit(x$opponents_surface_percentage, ' %'))
                     #x$players_surface_percentage <-as.numeric(strsplit(x$players_surface_percentage,' %'))
                     pal <- c("red", "blue")
                     fig <-
                       plot_ly(
                         data = x,
                         x = ~ opponents_surface_percentage,
                         y = ~ 1:nrow(x),
                         color = ~ `Match Outcome`,
                         colors = pal,
                         size = 1.4
                       )
                     fig <- fig %>% layout(yaxis = list(zeroline = FALSE),
                                           xaxis = list(zeroline = FALSE))
                   })
                   
                   
                   output$player2_plot3 <- renderPlotly({
                     req(input$player2_ss)
                     req(input$slider1)
                     if (input$player2_ss == "All") {
                       x <- data
                     }
                     else{
                       x <- filter(data, Surface == input$player2_ss, )
                     }
                     x <-
                       filter(
                         x,
                         opponents_rank >= as.numeric(input$slider2[1]),
                         opponents_rank <= as.numeric(input$slider2[2])
                       )
                     x$opponents_surface_percentage <-
                       as.numeric(strsplit(x$opponents_surface_percentage, ' %'))
                     #x$players_surface_percentage <-as.numeric(strsplit(x$players_surface_percentage,' %'))
                     pal <- c("red", "blue")
                     fig <-
                       plot_ly(
                         data = x,
                         x = ~ opponents_rank,
                         y = ~ player_total_games,
                         color = ~ `Match Outcome`,
                         colors = pal,
                         size = 1.4
                       )
                     fig <- fig %>% layout(yaxis = list(zeroline = FALSE),
                                           xaxis = list(zeroline = FALSE))
                   })
                   
                   
                 })
                 
               })
}

