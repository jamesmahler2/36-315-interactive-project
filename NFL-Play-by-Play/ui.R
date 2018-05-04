library("shiny")
library("tidyverse")
library("readr")
library("plotly")
library("shinydashboard")
library("shinythemes")
library("rsconnect")

nfl_data <- read_csv("nfl_data.csv")
nfl_data_adam <- read.csv("nfl_data.csv")

sidebar <- dashboardSidebar(
  sidebarMenu(
    menuItem("Home Page", tabName = "homepage", icon = icon("football-ball")),
    menuItem("Graph1", tabName = "graph1", icon = icon("football-ball")), 
    menuItem("Graph2", tabName = "graph2", icon = icon("football-ball")), 
    menuItem("Graph3", tabName = "graph3", icon = icon("football-ball")), 
    menuItem("Graph4", tabName = "graph4", icon = icon("football-ball")), 
    menuItem("Graph5", tabName = "graph5", icon = icon("football-ball")), 
    menuItem("Graph6", tabName = "graph6", icon = icon("football-ball")), 
    menuItem("Graph7", tabName = "graph7", icon = icon("football-ball")), 
    menuItem("Graph8", tabName = "graph8", icon = icon("football-ball"))
  )
)

body <- dashboardBody(
  tabItems(
    
    ### Home Page ###
    tabItem(tabName = "homepage",
            h2("NFL Offensive Performance and Play Selection: 
               Analysis of NFL Play-by-Play Data"), 
            p("By Andrew Wissinger, James Mahler, Deepak Vanjani, and Adam Tucker")
    ),
    
    ### Graph1 ###
    tabItem(tabName = "graph1",
            h2("Graph1"), 
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
              plotOutput("play_plot"))
    ),
    
    ### Graph2 ###
    tabItem(tabName = "graph2",
            h2("Graph2"), 
            fluidPage(
              inputPanel(
                selectInput("down2", label = "Down:",
                            choices = c(1, 2, 3, 4), selected = 1),
                selectInput("distance2", label = "Distance:",
                            choices = c("short", "medium", "long", "xlong"), 
                            selected = "long")), 
              plotlyOutput("EPA_plot"))
    ),
    
    ### Graph3 ###
    tabItem(tabName = "graph3",
            h2("Graph3"), 
            fluidPage(
              inputPanel(
                selectInput("team3", label = "Team:",
                            choices = c("All", sort(unique(nfl_data$posteam))), 
                            selected = "All"),
                selectInput("down3", label = "Down:",
                            choices = c(1, 2, 3, 4), selected = 1)), 
              plotlyOutput("receiver_plot"))
    ),
    
    ### Graph4 ###
    tabItem(tabName = "graph4",
            h2("Graph4"), 
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
            h2("Graph5"), 
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
            h2("Graph6"), 
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
            h2("Graph7"), 
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
            h2("Graph8"), 
            fluidPage(
              plotlyOutput("receiver_plot_2"))
    )
  )
)

# Put them together into a dashboardPage
dashboardPage(
  dashboardHeader(title = "NFL Play-by-Play"),
  sidebar,
  body
)
