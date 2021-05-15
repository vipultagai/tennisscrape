auto_ui <- function(id) {
  ns <- NS(id)
  tagList(
    tags$head(
      tags$style(
        ".shiny-notification {width: 50%;position: fixed; top: 0 ;right: 0"
      )
    ),
    
    tabsetPanel(
      id = 'autopanel',
      type = 'pills',
      
      tabPanel(
        'View Rankings',
        value = 'view_rankings',
        fluidRow(
          column(4, tags$h3("ATP/WTA Rankings")),
          column(3, textOutput(ns('ala'))),
          column(3, textOutput(ns('a_last'))),
          column(2, actionButton(
            ns('update_ranks'), 'Refresh', icon = icon('refresh')
          )),
        ),
        fluidRow(column(4),
                 column(3, textOutput(ns(
                   'wla'
                 ))),
                 column(3, textOutput(ns(
                   'w_last'
                 ))), ),
        fluidRow(
          tabsetPanel(
            id = 'rankings_display',
            type = 'tabs',
            tabPanel(title = 'ATP', value = 'ATP', dataTableOutput(ns('atp_ranks_out'))),
            tabPanel(title = 'WTA', value = 'WTA', dataTableOutput(ns('wta_ranks_out')))
          ),
        ),
      ),
      tabPanel(
        'Compare',
        value = 'compare',
        fluidRow(
          fluidRow(
            column(3, selectInput(
              ns('p1_association'), 'Association', choices = c('ATP', 'WTA')
            )),
            column(3, selectInput(
              ns('p1_source'),
              'Data Source',
              choices = c('File', 'Fetch by Link', 'Player Name')
            )),
            column(3, selectInput(
              ns('p2_source'),
              'Data Source',
              choices = c('File', 'Fetch by Link', 'Player Name')
            )),
            column(3, selectInput(
              ns('p2_association'), 'Association', choices = c('ATP', 'WTA')
            )),
          ),
          fluidRow(
            column(3, uiOutput(ns('p1_source_out'))),
            column(2, actionButton(ns('p1_data_get'), 'Fetch P-1', width = '100%')),
            column(1, downloadButton(
              ns('p1_down_btn'), 'P1', icon = icon('download')
            )),
            column(1, downloadButton(
              ns('p2_down_btn'), 'P2', icon = icon('download')
            )),
            column(2, actionButton(ns('p2_data_get'), 'Fetch P-2', width = '100%')),
            column(3, uiOutput(ns('p2_source_out')))
          ),
          
          fluidRow(
            column(1),
            column(1, actionButton(ns('show_results1'), 'Show')),
            column(3),
            column(2, actionButton(
              ns('show_head_to_head'), 'Show H2H', width = '100%'
            )),
            column(3),
            column(1, actionButton(ns('show_results2'), 'Show')),
            column(1)
          ),
          
          fluidRow(
            column(2, selectInput(
              ns('player1_ss'), "Player1 Surface", choices = NULL
            )),
            column(
              4,
              sliderInput(
                ns('slider1'),
                label = "Rank-Range",
                min = 0,
                max = 1000,
                value = c(0, 500),
                width = "100%"
              )
            ),
            column(
              4,
              sliderInput(
                ns('slider2'),
                label = "Rank-Range",
                min = 0,
                max = 1000,
                value = c(0, 500),
                width = "100%"
              )
            ),
            column(2, selectInput(
              ns('player2_ss'), "Player2 Surface", choices = NULL
            ))
          ),
          
          
          fluidRow(
            style = "border-bottom:solid silver 2px;",
            column(4, h3(textOutput(
              ns('player1_name')
            )), style = "background-color:ghostwhite;"),
            column(1, tags$h2(textOutput(
              ns("player1_rank")
            )), style = "background-color:beige;"),
            column(
              2,
              style = "text-align:center",
              tags$h4(textOutput(ns('matches_played')), style = "background-color:ghostwhite;"),
              fluidRow(
                column(6, textOutput(ns('matches_won1')), style = "border-right:solid silver 1px;background-color:powderblue;"),
                column(6, textOutput(ns('matches_won2')), style = "border-left:solid silver 1px;background-color:powderblue;")
              )
            ),
            column(1, tags$h2(textOutput(
              ns("player2_rank")
            )), style = "text-align:right;background-color:beige;"),
            column(4, h3(textOutput(
              ns('player2_name')
            )), style = "text-align:right;background-color:ghostwhite;")
          ),
          
          fluidRow(
            style = "border-bottom:solid silver 2px;",
            column(4, tags$h4(textOutput(
              ns('player1_age')
            )), style = "text-align:right;background-color:ghostwhite;"),
            column(4, tags$h4("AGE"), style = "text-align:center;background-color:white;"),
            column(4, tags$h4(textOutput(
              ns('player2_age')
            )), style = "text-align:left;background-color:ghostwhite;")
          ),
          
          fluidRow(
            style = "border-bottom:solid silver 2px;",
            column(4, tags$h4(textOutput(
              ns('player1_country')
            )), style = "text-align:right;background-color:ghostwhite;"),
            column(4, tags$h4("COUNTRY"), style = "text-align:center;background-color:white;"),
            column(4, tags$h4(textOutput(
              ns('player2_country')
            )), style = "text-align:left;background-color:ghostwhite;")
          ),
          fluidRow(
            style = "border-bottom:solid silver 2px;",
            column(4, tags$h4(textOutput(
              ns('player1_arm')
            )), style = "text-align:right;background-color:ghostwhite;"),
            column(4, tags$h4("ARM"), style = "text-align:center;background-color:white;"),
            column(4, tags$h4(textOutput(
              ns('player2_arm')
            )), style = "text-align:left;background-color:ghostwhite;")
          ),
          fluidRow(
            style = "border-bottom:solid silver 2px;",
            column(4, tags$h4(textOutput(
              ns('player1_exp')
            )), style = "text-align:right;background-color:ghostwhite;"),
            column(4, tags$h4("EXPERIENCE"), style = "text-align:center;background-color:white;"),
            column(4, tags$h4(textOutput(
              ns('player2_exp')
            )), style = "text-align:left;background-color:ghostwhite;")
          ),
          fluidRow(
            style = "border-bottom:solid silver 2px;",
            column(4, tags$h4(textOutput(ns(
              'player1_fg'
            ))), style = "text-align:right;background-color:ghostwhite;"),
            column(4, tags$h4("FAVORITE GROUND"), style = "text-align:center;background-color:white;"),
            column(4, tags$h4(textOutput(ns(
              'player2_fg'
            ))), style = "text-align:left;background-color:ghostwhite;")
          ),
          fluidRow(
            style = "border-bottom:solid silver 2px;",
            column(4, tags$h4(textOutput(
              ns('player1_win_p')
            )), style = "text-align:right;background-color:ghostwhite;"),
            column(4, tags$h4("SURFACE WIN %"), style = "text-align:center;background-color:white;"),
            column(4, tags$h4(textOutput(
              ns('player2_win_p')
            )), style = "text-align:left;background-color:ghostwhite;")
          ),
          fluidRow(
            style = "border-bottom:solid silver 2px;",
            column(4, tags$h4(textOutput(
              ns('player1_total')
            )), style = "text-align:right;background-color:ghostwhite;"),
            column(4, tags$h4("Matches Selected"), style = "text-align:center;background-color:white;"),
            column(4, tags$h4(textOutput(
              ns('player2_total')
            )), style = "text-align:left;background-color:ghostwhite;")
          ),
          fluidRow(
            style = "border-bottom:solid silver 2px;",
            column(4, tags$h4(textOutput(
              ns('player1_avg_games')
            )), style = "text-align:right;background-color:ghostwhite;"),
            column(4, tags$h4("Average Games Played"), style = "text-align:center;background-color:white;"),
            column(4, tags$h4(textOutput(
              ns('player2_avg_games')
            )), style = "text-align:left;background-color:ghostwhite;")
          ),
          fluidRow(
            style = "border-bottom:solid silver 2px;",
            column(4, tags$h4(textOutput(
              ns('player1_win_pm')
            )), style = "text-align:right;background-color:ghostwhite;"),
            column(4, tags$h4("MEDIAN OPPONENT WIN % BY SURFACE"), style = "text-align:center;background-color:white;"),
            column(4, tags$h4(textOutput(
              ns('player2_win_pm')
            )), style = "text-align:left;background-color:ghostwhite;")
          ),
          fluidRow(
            style = "border-bottom:solid silver 2px;",
            column(4, tags$h4(textOutput(
              ns('player1_lose_pm')
            )), style = "text-align:right;background-color:ghostwhite;"),
            column(4, tags$h4("MEDIAN OPPONENT LOSE % BY SURFACE"), style = "text-align:center;background-color:white;"),
            column(4, tags$h4(textOutput(
              ns('player2_lose_pm')
            )), style = "text-align:left;background-color:ghostwhite;")
          ),
          fluidRow(
            style = "border-bottom:solid silver 2px;",
            column(4, tags$h4(textOutput(
              ns('player1_win_lose')
            )), style = "text-align:right;background-color:ghostwhite;"),
            column(4, tags$h4("WIN/LOSE BY SURFACE"), style = "text-align:center;background-color:white;"),
            column(4, tags$h4(textOutput(
              ns('player2_win_lose')
            )), style = "text-align:left;background-color:ghostwhite;")
          ),
          
          fluidRow(column(6, plotlyOutput(
            ns('player1_plot')
          )),
          column(6, plotlyOutput(
            ns('player2_plot')
          ))),
          fluidRow(column(6, plotlyOutput(
            ns('player1_plot2')
          )),
          column(6, plotlyOutput(
            ns('player2_plot2')
          ))),
          fluidRow(column(6, plotlyOutput(
            ns('player1_plot3')
          )),
          column(6, plotlyOutput(
            ns('player2_plot3')
          )))
          
          
          
          #Compare Ends
          
        )
      ),
      
      tabPanel(
        'About',
        value = 'about',
        tags$h3("About"),
        tags$p("This application is built for Web Scrapping."),
        tags$p(
          "It scrapes player data from tennislive.net which allows scraping data."
        ),
        tags$p(
          "There are three options to scrape/view/download the player data."
        ),
        tags$p("1. Upload an excel file dowloaded using the application"),
        tags$p(
          "2. Select player name from select input. (900 players abailable in the rankings tab.) "
        ),
        tags$p("3. Paste a player profile link from tennislive.net"),
        tags$p(
          "After selecting the method and typing/selecting/uploading the link/name/file, click on fetch."
        ),
        tags$p(
          "It may take upto 2-3 minutes depending on the availibility of the data."
        ),
        tags$p(
          "After the progress bar dissapears, click on show to view data or click on download button to save as excel file."
        ),
      )
      
    )
  )
}
