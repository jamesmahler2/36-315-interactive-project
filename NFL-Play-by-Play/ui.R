library("shiny")
library("tidyverse")
library("readr")
library("plotly")
library("shinydashboard")
library("shinythemes")
library("rsconnect")

nfl_data <- read_csv("nfl_data.csv")
nfl_data_adam <- read.csv("nfl_data.csv")

sidebar <- dashboardSidebar(width = 300,
  sidebarMenu(
    menuItem("Home Page", tabName = "homepage", icon = icon("football-ball")),
    menuItem("Analysis of Play Selection", tabName = "graph1", icon = icon("football-ball")), 
    menuItem("YAC Analysis", tabName = "graph3", icon = icon("football-ball")), 
    menuItem("Receiver Explosion vs WPA", tabName = "graph4", icon = icon("football-ball")), 
    menuItem("WPA by Pass Location", tabName = "graph5", icon = icon("football-ball")), 
    menuItem("Are EPA and WPA Consistent?", tabName = "graph6", icon = icon("football-ball")), 
    menuItem("Play Selection by Game Situation", tabName = "graph7", icon = icon("football-ball")), 
    menuItem("Receiver Efficiency", tabName = "graph8", icon = icon("football-ball"))
  )
)

body <- dashboardBody(
  tabItems(
    
    ### Home Page ###
    tabItem(tabName = "homepage",
            h2("NFL Offensive Performance and Play Selection: 
               Analysis of NFL Play-by-Play Data"), 
            p("By Andrew Wissinger, James Mahler, Deepak Vanjani, and Adam Tucker"),
            p("Hello, and welcome to our NFL play-by-play data visualization page. We used the nflscrapR data
              set, which can be found here: https://github.com/ryurko/nflscrapR-data. The data set contains every
              NFL play from the 2017 season. In this app, we analyze a few things including play selection, win
              probability added, and wide receiver performance. Some variable explanations are as follows: WPA is
              'win probability added', and is a measure of how much the offensive team added to their win probability
              on a given play. We say a play is successful if WPA is greater than zero. EPA is 'expected points
              added', and measures how many points the offensive team added to their expected point total. The
              rest of the variables are pretty self explanatory. Enjoy!")
    ),
    
    ### Graph1 and Graph2 (they go together)###
    tabItem(tabName = "graph1",
            h2("Play Selection and Success by Down, Distance and Team"),
            p("These graphs show how each team performs on each down and distance, and how sucessful they were
              for each play type. The first graph shows each team's play selection and their success rate based
              on WPA, and the second graph shows the average yards gained and the EPA for each play type by down
              and distance for all teams."),
            fluidPage(
              inputPanel(
                selectInput("down", label = "Down:",
                            choices = c(1, 2, 3, 4), selected = 1),
                selectInput("distance", label = "Distance:",
                            choices = c("short", "medium", "long", "xlong"), 
                            selected = "long"),
                selectInput("team", label = "Team:",
                                       choices = c("All", sort(unique(nfl_data$posteam))), 
                                       selected = "All")),
              plotOutput("play_plot"),
              plotlyOutput("EPA_plot"))
    ),
    
    ### Graph3 ###
    tabItem(tabName = "graph3",
            h2("Yards After Catch Analysis"),
            p("This shows the correlation between air yards (how far down the field the ball traveled in the air),
              and yards after catch gained by the receiver. This can be separated by team and down."),
            fluidPage(
              inputPanel(
                selectInput("team3", label = "Team:",
                            choices = c("All", sort(unique(nfl_data$posteam))), 
                            selected = "All"),
                selectInput("down3", label = "Down:",
                            choices = c(1, 2, 3), selected = 1)), 
              plotlyOutput("receiver_plot"))
    ),
    
    ### Graph4 ###
    tabItem(tabName = "graph4",
            h2("Yards After Catch vs Win Probability Added"),
            p("This shows the correlation between yards gained after catch and win probabililty added
              for each team."),
            fluidPage(
              inputPanel(
                selectInput("team4", label = "Team:",
                                       choices = sort(unique(nfl_data$posteam)),
                            selected = "ARI"),
                checkboxInput("show_conf", label = "Include confidence band")),
              plotOutput("WPA_YAC_plot")
            )
    ),
    
    ### Graph5 ###
    tabItem(tabName = "graph5",
            h2("WPA by Pass Location"),
            p("This graph shows the win probabiliity added for each team by pass location. This allows us to
              see how good each team is throwing to different parts of the field."),
            fluidPage(
              inputPanel(
                selectInput("team5", label = "Team:",
                                       choices = sort(unique(nfl_data$posteam)),
                            selected = "ARI")),
              plotlyOutput("passlocation_WPA_plot")
            )
    ),
    
    ### Graph6 ###
    tabItem(tabName = "graph6",
            h2("Comparison of Measures of Success"), 
            p("This shows the correlation between the two main measures of success for a play: win probability
              added and expected points added. This shows us how consistent the measures are with each other and
              how they differ amongst teams."),
            fluidPage(
              inputPanel(
                radioButtons("quarter1", label = "Which quarter?",
                             choices = c("All Quarters" = "5",
                                         "First Quarter" = "1",
                                         "Second Quarter" = "2",
                                         "Third Quarter" = "3",
                                         "Fourth Quarter" = "4"))),
              plotlyOutput("adam_graph1")
            )
    ),
    
    ### Graph7 ###
    tabItem(tabName = "graph7",
            h2("Play Selection Based on the State of the Game"), 
            p("This graph shows us how teams select plays depending on the state of the game, which includes
              who is winning and what quarter it is."),
            fluidPage(
              inputPanel(
                radioButtons("quarter2", label = "Which quarter?",
                             choices = c("All Quarters" = "5",
                                         "First Quarter" = "1",
                                         "Second Quarter" = "2",
                                         "Third Quarter" = "3",
                                         "Fourth Quarter" = "4")),
                radioButtons("score", label = "Which score state?",
                             choices = c("Offense losing" = "neg",
                                         "Tied" = "zero",
                                         "Offense winning" = "pos"))),
              plotlyOutput("adam_graph2")
            )
    ),
    
    ### Graph8 ###
    tabItem(tabName = "graph8",
            h2("Wide Receiver Efficiency"),
            p("This graph shows the correlation between air yards when targeted and average yards gained per
              target for each receiver."),
            fluidPage(
              inputPanel(
                sliderInput("min_targets", label = "Minimum # of Targets:", min = 25, max = 150, 
                            value = 25, step = 1)
              ),
              plotlyOutput("receiver_plot_2"))
    )
  )
)

# Put them together into a dashboardPage
dashboardPage(
  dashboardHeader(title = "NFL Play-by-Play", titleWidth = 300),
  sidebar,
  body, 
  skin = "red"
)
