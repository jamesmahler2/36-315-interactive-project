library("shiny")
library("tidyverse")
library("readr")
library("plotly")
library("shinydashboard")
library("shinythemes")
library("rsconnect")


nfl_data <- read_csv("nfl_data.csv")
nfl_data_adam <- read.csv("nfl_data.csv")


# Create data frame for Adam's graph 1
remove_these <- 
  union(x = which(is.na(nfl_data_adam$EPA) == TRUE),
        y = which(is.na(nfl_data_adam$WPA) == TRUE))

graph1_data <- nfl_data_adam[-(remove_these), ]

Team <- c()
EPA <- c()
WPA <- c()
Qtr <- c()

for (j in 1:4) {
  for (i in 1:length(unique(graph1_data$posteam))) {
    current_team = as.character(unique(graph1_data$posteam)[i])
    current_qtr =  unique(graph1_data$qtr)[j]
    if (current_team != "") {
      loop_data = filter(
        .data = graph1_data,
        unique(graph1_data$posteam)[i] == graph1_data$posteam &
          current_qtr == graph1_data$qtr
      )
      EPA_team = mean(loop_data$EPA)
      WPA_team = mean(loop_data$WPA)
      Team = c(Team, current_team)
      EPA = c(EPA, EPA_team)
      WPA = c(WPA, WPA_team)
      Qtr = c(Qtr, current_qtr)
    } else {
      next
    }
  }
}

graph1_plot_data <- data.frame(Team, EPA, WPA, Qtr)
graph1_plot_data$Qtr <- factor(graph1_plot_data$Qtr)


# Create data frame for Adam's graph 2
graph2_plot_data <- 
  nfl_data_adam %>%
  filter(PlayType == 'Pass' | PlayType == 'Run')

graph2_plot_data <- 
  graph2_plot_data[-(which(is.na(graph2_plot_data$ScoreDiff))), ]

graph2_plot_data <-
  filter(graph2_plot_data, qtr != 5)

score_state <- c()

for (i in 1:nrow(graph2_plot_data)) {
  if (graph2_plot_data$ScoreDiff[i] < 0) {
    score_state = c(score_state, 0) 
  } else if (graph2_plot_data$ScoreDiff[i] == 0) {
    score_state = c(score_state, 1) 
  } else {
    score_state = c(score_state, 2) 
  }
}

graph2_plot_data <- cbind(graph2_plot_data, score_state)


# create categorical variables based on WPA and EPA to determine if a play was a "success"
nfl_data$isSuccess_EPA = ifelse(nfl_data$EPA>0 & !is.na(nfl_data$EPA), 1, 0)
nfl_data$isSuccess_WPA = ifelse(nfl_data$WPA>0 & !is.na(nfl_data$WPA), 1, 0)

# create categorical variable for yards to go
nfl_data$ytg = ifelse(nfl_data$ydstogo<4, "short",
                      ifelse(nfl_data$ydstogo<8, "medium",
                             ifelse(nfl_data$ydstogo<11, "long", "xlong")))

# create a categorical variable that merges Pass Location and Run Location to create
# a general direction variable
nfl_data$direction = NA  # new merged column start with NA
nfl_data$direction[!is.na(nfl_data$PassLocation)] = nfl_data$PassLocation[!is.na(nfl_data$PassLocation)]  # merge with pass location
nfl_data$direction[!is.na(nfl_data$RunLocation)] = nfl_data$RunLocation[!is.na(nfl_data$RunLocation)]  # merge with pass location


play_selection <- nfl_data %>%
  filter(!is.na(down), PlayType %in% c("Pass", "Run"),
         !is.na(direction), !is.na(EPA)) %>%
  group_by(down, ytg, PlayType) %>%
  summarise(count = n(),
            success_rate_wpa = sum(isSuccess_WPA)/count,
            avg_yds = sum(Yards.Gained)/count,
            epa_avg = sum(EPA) / count) %>%
  mutate(freq = count/sum(count))


play_selection_by_team <- nfl_data %>%
  filter(!is.na(down), PlayType %in% c("Pass", "Run"),
         !is.na(direction), !is.na(EPA)) %>%
  group_by(down, ytg, PlayType, posteam) %>%
  summarise(count = n(),
            success_rate_wpa = sum(isSuccess_WPA)/count,
            avg_yds = sum(Yards.Gained)/count,
            epa_avg = sum(EPA) / count) %>%
  mutate(freq = count/sum(count))


receiver_data <- nfl_data %>% 
  filter(PassOutcome == "Complete", down %in% c(1, 2, 3, 4)) %>% 
  group_by(Receiver, down, posteam) %>% 
  summarise(count = n(),
            air_yds = mean(AirYards),
            YAC = mean(YardsAfterCatch)) %>% 
  filter(count>5)

receiver_data_2 <- nfl_data %>% 
  filter(PlayType == "Pass", !is.na(Receiver)) %>% 
  group_by(Receiver) %>% 
  summarise(count = n(),
            catch_percent = sum(PassOutcome == "Complete")/count,
            avg_yds_when_targeted = mean(AirYards),
            avg_yds_gained_target = mean(Yards.Gained)) %>% 
  filter(count>25)



function(input, output) {
  
  ### Graph1 ###
  output$play_plot <- renderPlot({
    if(input$team == "All"){
      ggplot(play_selection[play_selection$down == input$down & play_selection$ytg == input$distance,],
             aes(x = factor(PlayType), y = count, fill = success_rate_wpa)) + geom_bar(stat = "identity") +
        scale_fill_gradient(low = "gray100", high = "blue", limits = c(.15, .75)) +
        labs(title = "Play Selection by Down and Distance", x = "Play Type", y = "Count",
             fill = "Success Rate (WPA>0)") + theme_bw() + theme(text = element_text(size = 16))}
    else{
      ggplot(filter(play_selection_by_team, down == input$down, ytg == input$distance, posteam == input$team),
             aes(x = factor(PlayType), y = count, fill = success_rate_wpa)) + geom_bar(stat = "identity") +
        scale_fill_gradient(low = "gray100", high = "blue", limits = c(0, 1)) +
        labs(title = "Play Selection by Down, Distance and Team", x = "Play Type", y = "Count",
             fill = "Success Rate (WPA>0)") + theme_bw()
    }
  })
  
  ### Graph2 ###
  output$EPA_plot <- renderPlotly({
    EPA_plot <- ggplot(filter(play_selection_by_team, down == input$down, ytg == input$distance),
                       aes(x = avg_yds, y = epa_avg, color = PlayType, label = posteam)) + geom_point() +
      labs(title = "Yards vs EPA for Each Team by Down and Distance", x = "Average Yards Gained",
           y = "EPA (Expeceted Points Added)", color = "Play Type") + theme_bw()
    if(input$team != "All"){
      EPA_plot <- EPA_plot + geom_text(data = filter(play_selection_by_team, posteam == input$team,
                                                     down == input$down, ytg == input$distance),
                                       aes(label = input$team, fontface = "bold"), color = "black")
    }
    ggplotly(EPA_plot)
  })
  
  ### Graph3 ###
  output$receiver_plot <- renderPlotly({
    if(input$team3 == "All"){
      yac_plot <- ggplot(filter(receiver_data, down == input$down3, count>10),
                         aes(x = air_yds, y = YAC)) +
        geom_point(aes(text=sprintf("Receiver: %s<br>Catches: %s", Receiver, count))) +
        coord_cartesian(xlim = c(-5, 20), ylim = c(0, 15)) + geom_smooth(method = "loess") +
        labs(title = "Air Yards vs Yards After Catch by Down", x = "How Far Ball traveled in the Air",
             y = "Yards Gained After Catch") + theme_bw()
    }
    else{
      yac_plot <- ggplot(filter(receiver_data, down == input$down3, posteam == input$team3),
                         aes(x = air_yds, y = YAC)) +
        geom_text(aes(label = Receiver, text=sprintf("Catches: %s", count))) +
        coord_cartesian(xlim = c(-5, 20), ylim = c(0, 15)) +
        labs(title = "Air Yards vs Yards After Catch by Down and Team", x = "How Far Ball traveled in the Air",
             y = "Yards Gained After Catch") + theme_bw()
    }
    ggplotly(yac_plot)
  })
  
  ### Graph4 ###
  output$WPA_YAC_plot <- renderPlot({
    newdat <- filter(nfl_data, PassOutcome == "Complete")
    ggplot(subset(newdat, posteam == input$team4), 
           aes(x = YardsAfterCatch, y = WPA)) + geom_point() + 
      geom_smooth(method = "lm", se = input$show_conf) + 
      labs(x = "Yards After Catch", y = "Win Probability Added", 
           title = "Yards After Catch vs WPA") + 
      theme_bw() + theme(text = element_text(size = 16))
  })
  
  ### Graph5 ###
  output$passlocation_WPA_plot <- renderPlotly({
    newdat <- nfl_data[!is.na(nfl_data$PassLocation), ]
    give.n <- function(x){
      return(c(y = median(x)*1.05, label = length(x))) }
    p <- ggplot(subset(newdat, posteam == input$team5), 
                aes(x = PassLocation, y = WPA)) + 
      geom_boxplot() + 
      stat_summary(fun.data = give.n, geom = "text", 
                   fun.y = median, position = position_dodge(width = 0.75), 
                   color = "black") + 
      labs(x = "Pass Location", y = "Win Probability Added",
           title = "WPA by Pass Location") + 
      scale_x_discrete(labels=c("left" = "Left", "middle" = "Middle", 
                                "right" = "Right")) + theme_bw()
    ggplotly(p)
  })
  
  ### Graph6 ###
  datasetInput <- reactive({
    switch(input$quarter1,
           "5" = 5,
           "1" = 1,
           "2" = 2,
           "3" = 3,
           "4" = 4)
  })
  output$adam_graph1 <- renderPlotly({
    selected_data <- datasetInput()
    if (selected_data == 1) {
      point_color = "#F8766D"
    }
    if (selected_data == 2) {
      point_color = "#7CAE00"
    }
    if (selected_data == 3) {
      point_color = "#00BFC4"
    }
    if (selected_data == 4) {
      point_color = "#C77CFF"
    }
    if (selected_data != 5) {
      a1_graph_data = 
        filter(graph1_plot_data, Qtr == selected_data)
      a1 = ggplot(a1_graph_data, aes(x = EPA, y = WPA, group = Team)) +
        geom_point(color = point_color) +
        geom_hline(yintercept = 0, linetype = "dashed") +
        geom_vline(xintercept = 0, linetype = "dashed") +
        labs(x = "Average Expected Points Added per Play",
             y = "Average Win Probability Added per Play",
             title = 
               paste("Average EPA and WPA during Quarter", selected_data, 
                     "for All Teams in 2017")) +
        coord_cartesian(xlim = c(-.25, .25), ylim = c(-0.01, 0.01)) +
        theme(legend.position="bottom") + theme_bw()
    } else {
      a1 = ggplot(graph1_plot_data, aes(x = EPA, y = WPA, 
                                        group = Team, color = Qtr)) +
        geom_point(alpha = 0.65) +
        geom_hline(yintercept = 0, linetype = "dashed") +
        geom_vline(xintercept = 0, linetype = "dashed") +
        labs(title = "Average EPA and WPA during All Quarters for All Teams in 2017",
             x = "Average Expected Points Added per Play",
             y = "Average Win Probability Added per Play",
             color = "Quarter") +
        coord_cartesian(xlim = c(-.25, .25), ylim = c(-0.01, 0.01)) +
        theme(legend.position="bottom") + theme_bw()
    }
    ggplotly(a1)
  })
  
  ### Graph7 ###
  qtrInput <- reactive({
    switch(input$quarter2,
           "5" = 5,
           "1" = 1,
           "2" = 2,
           "3" = 3,
           "4" = 4)
  })
  stateInput <- reactive({
    switch(input$score,
           "neg" = 0,
           "zero" = 1,
           "pos" = 2)
  })
  output$adam_graph2 <- renderPlotly({
    selected_qtr <- qtrInput()
    selected_score <- stateInput()
    if (selected_qtr != 5){
      a2_graph_data <-
        filter(graph2_plot_data, 
               qtr == selected_qtr & score_state == selected_score)
      a2 <- ggplot(a2_graph_data, aes(x = PlayType)) +
        geom_bar() +
        labs(x = "Play Types",
             y = "Amount of Plays",
             title = paste("Offensive Play Calling in Quarter",
                           selected_qtr)) +
        theme_bw()
    } else {
      a2_graph_data <-
        filter(graph2_plot_data, score_state == selected_score)
      a2 <- ggplot(a2_graph_data, aes(x = PlayType)) +
        geom_bar() +
        labs(x = "Play Types", 
             y = "Amount of Plays",
             title = "Offensive Play Calling in All Quarters") +
        facet_wrap(~ qtr, ncol = 2)
      theme_bw()
    }
    ggplotly(a2)
  })
  
  ### Graph8 ###
  output$receiver_plot_2 <- renderPlotly({
    yds_plot <- ggplot(filter(receiver_data_2, count > input$min_targets), aes(x = avg_yds_when_targeted, 
                                            y = avg_yds_gained_target,
                                            color = catch_percent)) +
      geom_point(aes(text=sprintf("Receiver: %s<br>Targets: %s", Receiver, count))) +
      coord_cartesian(xlim = c(min(receiver_data_2$avg_yds_when_targeted), max(receiver_data_2$avg_yds_when_targeted)),
                      ylim = c(0, max(receiver_data_2$avg_yds_gained_target))) +
      labs(title = "Air Yards vs Yards Gained for Receivers", 
           x = "Average Air Yards When Targeted",
           y = "Average Yards Gained per Target", 
           color = "Catch Percentage") + theme_bw()
    ggplotly(yds_plot)
  })
}