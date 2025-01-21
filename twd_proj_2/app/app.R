#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(rjson)
library(dplyr)
library(lubridate)
library(ggplot2)
library(leaflet)
library(bslib)


lok_joz <- fromJSON(file = "../data/Os_czasu_jo.json")

lok_mic <- fromJSON(file = "../data/Os_czasu_mi.json")
lok_kla <- fromJSON(file = "../data/Os_czasu_kl.json")

lok_names <- read.csv("../data/lok_names_jo.csv")


Sys.setlocale("LC_TIME", "C")
base_path <- "../data/"

transport <- function(x){
  col_names <- c("activity", "activity_type","distance", "startTime", "endTime", "weekNum")
  
  
  result <- data.frame(matrix(ncol = length(col_names), nrow = 0))
  colnames(result) <- col_names
  
  counter = 1
  for(i in 1:length(x)){
    if(!is.null(x[[i]]$activity)){
      
      result <- rbind(result, data.frame(activity = x[[i]]$activity$topCandidate$type, 
                                         activity_type = "transport",
                                         distance = x[[i]]$activity$distanceMeters,
                                         startTime = x[[i]]$startTime, endTime = x[[i]]$endTime,
                                         weekNum = ceiling(as.numeric(difftime(as.POSIXct(substr(x[[i]]$startTime, 1, 10), format = "%Y-%m-%d"),
                                                                               as.POSIXct("2024-12-08", format = "%Y-%m-%d"), units = "weeks")))))
      counter <- counter + 1
    }
  }
  result
}

miejsca <- function(x){
  
  col_names <- c("place", "activity_type", "startTime", "endTime", "weekNum")
  
  
  result <- data.frame(matrix(ncol = length(col_names), nrow = 0))
  colnames(result) <- col_names
  
  counter = 1
  for(i in 1:length(x)){
    if(!is.null(x[[i]]$visit)){
      
      
      day_diff <- ceiling(as.numeric(difftime(as.POSIXct(substr(x[[i]]$endTime, 1, 10), format = "%Y-%m-%d"),
                                              as.POSIXct(substr(x[[i]]$startTime, 1, 10), format = "%Y-%m-%d"), units = "days"))) + 1
      
      
      for(j in 1:day_diff){
        result <- rbind(result, data.frame(place = ifelse(sum(lok_names$placeID == x[[i]]$visit$topCandidate$placeID) == 1,lok_names[lok_names$placeID == x[[i]]$visit$topCandidate$placeID,"name"], "other"),
                                           activity_type = ifelse(sum(lok_names$placeID == x[[i]]$visit$topCandidate$placeID) == 1,lok_names[lok_names$placeID == x[[i]]$visit$topCandidate$placeID,"category"], "other"),
                                           startTime = case_when(j == 1 ~ substr(x[[i]]$startTime, 1, 19),
                                                                 TRUE ~ paste0(as.character(tail(seq(as.POSIXct(substr(x[[i]]$startTime, 1, 19), format = "%Y-%m-%d"), by= "day", length = j), n = 1)), "T00:00:00")), 
                                           endTime = case_when(j == day_diff ~ substr(x[[i]]$endTime, 1, 19),
                                                               j == 1 ~ paste0(as.character(as.POSIXct(substr(x[[i]]$startTime, 1, 19), format = "%Y-%m-%d")), "T23:59:59"),
                                                               TRUE ~ paste0(as.character(tail(seq(as.POSIXct(substr(x[[i]]$startTime, 1, 19), format = "%Y-%m-%d"), by= "day", length = j), n = 1)), "T23:59:59")),
                                           weekNum = ceiling(as.numeric(difftime(as.POSIXct(substr(x[[i]]$startTime, 1, 10), format = "%Y-%m-%d"),
                                                                                 as.POSIXct("2024-12-08", format = "%Y-%m-%d"), units = "weeks")))))
      }
      counter <- counter + 1
    }
  }
  result
}

podroze_joz <- transport(lok_joz) %>% 
  mutate(timeDurSec = as.integer(difftime(as.POSIXct(substr(endTime, 1, 19), format = "%Y-%m-%dT%H:%M:%S"),
                                          as.POSIXct(substr(startTime, 1, 19), format = "%Y-%m-%dT%H:%M:%S"), #
                                          units = "secs"))) %>% 
  mutate(day = as.POSIXct(substr(endTime, 1, 10), format = "%Y-%m-%d")) %>% 
  mutate(person = "Jozef")

wizyty_joz <- miejsca(lok_joz) %>% 
  
  mutate(timeDurSec = as.integer(difftime(as.POSIXct(substr(endTime, 1, 19), format = "%Y-%m-%dT%H:%M:%S"),
                                          as.POSIXct(substr(startTime, 1, 19), format = "%Y-%m-%dT%H:%M:%S"), #
                                          units = "secs")))%>% 
  group_by(place) %>% 
  mutate(sumTime = sum(timeDurSec)) %>% 
  mutate(day = as.POSIXct(substr(endTime, 1, 10), format = "%Y-%m-%d")) %>% 
  mutate(person = "Jozef")



podroze_mic <- transport(lok_mic) %>% 
  mutate(timeDurSec = as.integer(difftime(as.POSIXct(substr(endTime, 1, 19), format = "%Y-%m-%dT%H:%M:%S"),
                                          as.POSIXct(substr(startTime, 1, 19), format = "%Y-%m-%dT%H:%M:%S"), #
                                          units = "secs"))) %>% 
  mutate(day = as.POSIXct(substr(endTime, 1, 10), format = "%Y-%m-%d")) %>% 
  mutate(person = "Michal")

wizyty_mic <- miejsca(lok_mic) %>% 
  
  mutate(timeDurSec = as.integer(difftime(as.POSIXct(substr(endTime, 1, 19), format = "%Y-%m-%dT%H:%M:%S"),
                                          as.POSIXct(substr(startTime, 1, 19), format = "%Y-%m-%dT%H:%M:%S"), #
                                          units = "secs")))%>% 
  group_by(place) %>% 
  mutate(sumTime = sum(timeDurSec)) %>% 
  mutate(day = as.POSIXct(substr(endTime, 1, 10), format = "%Y-%m-%d")) %>% 
  mutate(person = "Michal")



podroze_kla <- transport(lok_kla) %>% 
  mutate(timeDurSec = as.integer(difftime(as.POSIXct(substr(endTime, 1, 19), format = "%Y-%m-%dT%H:%M:%S"),
                                          as.POSIXct(substr(startTime, 1, 19), format = "%Y-%m-%dT%H:%M:%S"), #
                                          units = "secs"))) %>% 
  mutate(day = as.POSIXct(substr(endTime, 1, 10), format = "%Y-%m-%d")) %>% 
  mutate(person = "Klaudia")

wizyty_kla <- miejsca(lok_kla) %>% 
  
  mutate(timeDurSec = as.integer(difftime(as.POSIXct(substr(endTime, 1, 19), format = "%Y-%m-%dT%H:%M:%S"),
                                          as.POSIXct(substr(startTime, 1, 19), format = "%Y-%m-%dT%H:%M:%S"), #
                                          units = "secs")))%>% 
  group_by(place) %>% 
  mutate(sumTime = sum(timeDurSec)) %>% 
  mutate(day = as.POSIXct(substr(endTime, 1, 10), format = "%Y-%m-%d")) %>% 
  mutate(person = "Klaudia")


# input$People <- c("Klaudia", "Jozef", "Michal")

# Define UI for application that draws a histogram
ui <- fluidPage(
  
    # Application title
    titlePanel("Localisation data analysis"),

    tabsetPanel(
      tabPanel("Weekly activities",
               sidebarLayout(
                 sidebarPanel(
                   sliderInput("sliderWeek1", "Select weeks for the plot:",
                               min = 1, max = 6, value = c(1, 2), step = 1),
                 
                 selectInput("dropdown1", "Choose the person:",
                             choices = c("Jozef", "Michal", "Klaudia"))
                 ),
                 mainPanel(
                   textOutput("weeklyActivitiesText1"),
                   plotOutput("dailyActivitiesPlot"),
                   
                 )
               )
      ),
      tabPanel("Transportation by type",
               sidebarLayout(
                 sidebarPanel(
                   selectInput(
                     inputId = "selectedPerson",
                     label = "Select Person:",
                     choices = c("Jozef", "Michal", "Klaudia"),
                     selected = "Jozef"
                   ),
                   selectInput(
                     inputId = "selectedWeek",
                     label = "Select Week (or Overall):",
                     choices = c("Overall", 1:5),
                     selected = "Overall"
                   )
                 ),
                 mainPanel(
                   plotOutput("transportCountPlot"),
                   plotOutput("transportTimePlot")
                 )
               )
      ),
      tabPanel("Transport speed",
               sidebarLayout(
                 sidebarPanel(
                   selectInput(
                     inputId = "People",
                     label = "Select people:",
                     choices = c("Jozef", "Klaudia", "Michal"),
                     selected = c("Klaudia", "Michal"),  
                     multiple = TRUE  
                   ),
                   selectInput(
                     inputId = "TransportType",
                     label = "Select transport type:",
                     choices = c("cycling", "in car", "in subway", "in tram", "walking", "in train"),
                     selected = "walking"
                   )
                   ,
                   selectInput(
                     inputId = "selectedPerson2",
                     label = "Select Person:",
                     choices = c("Jozef", "Michal", "Klaudia"),
                     selected = "Jozef"
                   )
                 ),
                 mainPanel(
                   textOutput("transportSpeedText1"),     
                   plotOutput("transportSpeedBoxPlot"),   
                   textOutput("transportSpeedText2"),  
                   plotOutput("transportSpeedBoxPlot2"),
                   textOutput("transportSpeedText3"),  
                   plotOutput("transportSpeedHeatmap"),
                 )
               )
      ),
      
      tabPanel("Top 5 Most Visited Places",
        sidebarLayout(
          sidebarPanel(
            selectInput("person", "Select Person Name:", 
                      choices = c("Klaudia" = "kl", "Michal" = "mi", "Jozef" = "jo")),
            selectInput("week", "Select Week Number:", choices = NULL),
            actionButton("update", "Update")
        ),
        mainPanel(
          h4("Top 5 Most Visited Places"),
          leafletOutput("map"),
          tableOutput("table")
        )
    )
)
)
)


# Define server logic required to draw a histogram
server <- function(input, output, session) {
  
  ######################### Rodzaj Transportu ##################################
  
  output$transportCountPlot <- renderPlot({
    
    # Filter data for the selected person and week (if specified)
    personData <- switch(input$selectedPerson,
                         "Jozef" = podroze_joz,
                         "Michal" = podroze_mic,
                         "Klaudia" = podroze_kla)
    
    if (input$selectedWeek != "Overall") {
      personData <- personData %>% filter(weekNum == as.numeric(input$selectedWeek))
    }
    
    # Summarize the count of each transportation type
    transportCounts <- personData %>%
      group_by(activity) %>%
      summarise(count = n()) %>%
      arrange(desc(count))
    
    # Plot transportation counts
    ggplot(transportCounts, aes(x = reorder(activity, -count), y = count, fill = activity)) +
      geom_bar(stat = "identity") +
      theme_minimal() +
      labs(title = paste("Most Frequently Used Transport (Count) -",
                         ifelse(input$selectedWeek == "Overall", "Overall", paste("Week", input$selectedWeek)),
                         "-", input$selectedPerson),
           x = "Transportation Type", y = "Count", fill = "Transport") +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
  })
  
  output$transportTimePlot <- renderPlot({
    
    # Filter data for the selected person and week (if specified)
    personData <- switch(input$selectedPerson,
                         "Jozef" = podroze_joz,
                         "Michal" = podroze_mic,
                         "Klaudia" = podroze_kla)
    
    if (input$selectedWeek != "Overall") {
      personData <- personData %>% filter(weekNum == as.numeric(input$selectedWeek))
    }
    
    # Summarize the total time spent on each transportation type
    transportTime <- personData %>%
      group_by(activity) %>%
      summarise(totalTime = sum(timeDurSec, na.rm = TRUE)) %>%
      arrange(desc(totalTime))
    
    # Plot transportation time
    ggplot(transportTime, aes(x = reorder(activity, -totalTime), y = totalTime / 3600, fill = activity)) +
      geom_bar(stat = "identity") +
      theme_minimal() +
      labs(title = paste("Time Spent on Transport (Hours) -",
                         ifelse(input$selectedWeek == "Overall", "Overall", paste("Week", input$selectedWeek)),
                         "-", input$selectedPerson),
           x = "Transportation Type", y = "Time (Hours)", fill = "Transport") +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
  })

  
  ############################## Aktywności ####################################
  
  output$weeklyActivitiesText1 <- renderText({
    paste0("As we can see noone likes trains except jozef on some days. As we c
           an see noone likes trains except jozef on some days. As we can see no
           one likes trains except jozef on some days. As we can see noone likes 
           trains except jozef on some days")
  })
  
    output$dailyActivitiesPlot <- renderPlot({
      if(input$dropdown1 == "Jozef"){
        podroze <- podroze_joz
        wizyty <- wizyty_joz
      } else if(input$dropdown1 == "Klaudia"){
        podroze <- podroze_kla
        wizyty <- wizyty_kla
      } else {
        podroze <- podroze_mic
        wizyty <- wizyty_mic
      }
      
      activity_colors <- c(
        "home" = "#1ea362",
        "studies" = "#4a89f3",
        "transport" = "#dd4b3e",
        "entertainment" = "#d3d3d3",
        "shopping" = "#ffe047",
        "restaurants" = "#aadaff",
        "other" = "#ff0000"
      )
      
      czas_w_transporcie <- podroze %>%
        filter(weekNum >= input$sliderWeek1[1] & weekNum <=  input$sliderWeek1[2]) %>% 
        mutate(dayOfWeek = weekdays(as.Date(endTime, format = "%Y-%m-%d")),
               dayOfWeek = factor(dayOfWeek, levels = c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday"))) %>% 
        group_by(dayOfWeek) %>% 
        mutate(meanTimeDur = sum(timeDurSec)/(input$sliderWeek1[2] - input$sliderWeek1[1] + 1)) %>% 
        ungroup() %>% 
        group_by() %>% 
        select(dayOfWeek, meanTimeDur, activity_type) %>% 
        group_by(dayOfWeek, meanTimeDur) %>% 
        slice(1)
      
      czas_miejsca <- wizyty %>% 
        filter(weekNum >= input$sliderWeek1[1] & weekNum <=  input$sliderWeek1[2]) %>% 
        filter(place != "MiNI") %>% 
        mutate(day = as.POSIXct(substr(endTime, 1, 10), format = "%Y-%m-%d")) %>%
        mutate(dayOfWeek = weekdays(as.Date(endTime, format = "%Y-%m-%d")),
               dayOfWeek = factor(dayOfWeek, levels = c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday"))) %>%
        select(dayOfWeek, timeDurSec, activity_type) %>% 
        group_by(dayOfWeek, activity_type) %>% 
        mutate(meanTimeDur = sum(timeDurSec)/(input$sliderWeek1[2] - input$sliderWeek1[1] + 1)) %>% 
        ungroup() %>% 
        select(dayOfWeek, activity_type, meanTimeDur) %>% 
        group_by(dayOfWeek, activity_type, meanTimeDur) %>% 
        mutate(activity_type = ifelse(activity_type == "holiday_home", "home", activity_type)) %>% 
        slice(1)
      
      czas <- rbind(czas_miejsca, czas_w_transporcie)
      
      
      
      wyk2 <- ggplot(czas, aes(y = meanTimeDur, x = dayOfWeek, fill = activity_type))+
        geom_bar(stat = "identity", position = "fill") +
        labs(fill = "Activity Type") +                         
        xlab("Day of week") +                                     
        ylab("% of time of day") +                      
        ggtitle(paste0("Average daily time spent in each place by ", input$dropdown1)) +     
        theme_minimal()+
        scale_fill_manual(values = activity_colors)
      
      wyk2
    })
  
    
   
  
  ############################## Predkość ######################################
    
    # output$transportSpeedPlot <- renderPlot({
    #   # pozniej dodam zeby kilka osob na raz dalo sie wziac
    #   
    # 
    #   
    #   podroze2 <- rbind(podroze_joz, podroze_kla, podroze_mic)%>%
    #     mutate(Day = weekdays(as.Date(startTime, format = "%Y-%m-%d")),
    #            Day = factor(Day, levels = c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday"))) %>% 
    #     filter(person %in% input$People) %>% 
    #     mutate(activity2 = case_when(activity == "CYCLING" ~ "cycling",
    #                                  activity == "IN_PASSENGER_VEHICLE" ~ "in car",
    #                                  activity == "IN_SUBWAY" ~ "in subway",
    #                                  activity == "IN_TRAM" ~ "in tram",
    #                                  activity == "WALKING" ~ "walking",
    #                                  activity == "IN_TRAIN" ~ "in train",
    #                                  TRUE ~ activity)) %>% 
    #     filter(distance >= 1)
    #   
    #   
    #   agg_data <- podroze2 %>%
    #     group_by(Day, activity2) %>%
    #     summarise(mean_speed = mean(as.numeric(distance) / as.numeric(timeDurSec), na.rm = TRUE), .groups = 'drop')
    #     
    #   
    #   
    #   wyk1 <- ggplot(agg_data, aes(x = Day, y = mean_speed, fill = activity2)) +
    #     geom_col(position = position_dodge(width = 0.9)) + 
    #     labs(fill = "") +                        
    #     xlab("Day") +                                     
    #     ylab("Average Speed (m/s)") +                      
    #     ggtitle("Average speed by different means of transport") +     
    #     theme_minimal()       
    #   
    #   wyk1
    # })
  
  output$transportSpeedBoxPlot <- renderPlot({
    # Combine datasets and filter based on inputs
    podroze2 <- rbind(podroze_joz, podroze_kla, podroze_mic) %>%
      mutate(Day = weekdays(as.Date(startTime, format = "%Y-%m-%d")),
             Day = factor(Day, levels = c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday"))) %>%
      filter(person %in% input$People, distance >= 1) %>%
      mutate(activity2 = case_when(
        activity == "CYCLING" ~ "cycling",
        activity == "IN_PASSENGER_VEHICLE" ~ "in car",
        activity == "in passenger vehicle" ~ "in car",
        activity == "IN_SUBWAY" ~ "in subway",
        activity == "IN_TRAM" ~ "in tram",
        activity == "WALKING" ~ "walking",
        activity == "IN_TRAIN" ~ "in train",
        TRUE ~ activity
      )) %>%
      filter(activity2 == input$TransportType)
    
    people_colors <- c(
      "Klaudia" = "#1ea362",
      "Michal" = "#4a89f3",
      "Jozef" = "#dd4b3e"
    )
    
    # Boxplot with people as color and weekdays on x-axis
    ggplot(podroze2, aes(x = Day, y = as.numeric(distance) / as.numeric(timeDurSec), fill = person)) +
      geom_boxplot() +
      theme_minimal() +
      labs(title = paste("Speed Distribution by Weekday and Person -", input$TransportType),
           x = "Weekday", y = "Speed (m/s)", fill = "Person") +
      theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
      scale_fill_manual(values = people_colors)
  })
  
  
  output$transportSpeedBoxPlot2 <- renderPlot({
    
    podroze2 <- rbind(podroze_joz, podroze_kla, podroze_mic) %>%
      mutate(Day = weekdays(as.Date(startTime, format = "%Y-%m-%d")),
             Day = factor(Day, levels = c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday"))) %>%
      filter(person %in% input$People, distance >= 1) %>%
      mutate(activity2 = case_when(
        activity == "CYCLING" ~ "cycling",
        activity == "IN_PASSENGER_VEHICLE" ~ "in car",
        activity == "in passenger vehicle" ~ "in car",
        activity == "IN_SUBWAY" ~ "in subway",
        activity == "IN_TRAM" ~ "in tram",
        activity == "WALKING" ~ "walking",
        activity == "IN_TRAIN" ~ "in train",
        TRUE ~ activity
      )) %>%
      filter(activity2 == input$TransportType)
    
    people_colors <- c(
      "Klaudia" = "#1ea362",
      "Michal" = "#4a89f3",
      "Jozef" = "#dd4b3e"
    )
    
    ggplot(podroze2, aes(x = person, y = as.numeric(distance) / as.numeric(timeDurSec), fill = person)) +
      geom_boxplot() +
      theme_minimal() +
      labs(title = paste("Average Speed Comparison by Person -", input$TransportType),
           x = "Person", y = "Average Speed (m/s)", fill = "Person") +
      theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
      scale_fill_manual(values = people_colors)
  })
  
  
  
  output$transportSpeedText1 <- renderText({
    paste0(input$People[0])
  })
  
  output$transportSpeedText2 <- renderText({
    paste0("As we can see noone likes trains except jozef on some days. As we c
           an see noone likes trains except jozef on some days. As we can see no
           one likes trains except jozef on some days. As we can see noone likes 
           trains except jozef on some days")
  })
  
  output$transportSpeedText3 <- renderText({
    paste0("write me an interesting and smart sounding analysis of the heatmap in the picture showing how much a person walked in a certin day")
  })
  
  
  
  
  
  output$transportSpeedHeatmap <- renderPlot({
    all_weeks <- 1:6
    all_days <- c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday")
    complete_data <- expand.grid(weekNum = all_weeks, Day = all_days)
    
    lighten_color <- function(color, factor) {
      rgb_vals <- col2rgb(color) / 255  
      white <- c(1, 1, 1) 
      lighter_rgb <- rgb_vals * (1 - factor) + white * factor 
      rgb(lighter_rgb[1], lighter_rgb[2], lighter_rgb[3]) 
    }
    
    
    max_people_colors <- list(
      "Klaudia" = c("#1ea362"),
      "Michal" = c("#4a89f3"),
      "Jozef" = c("#dd4b3e")
    )
    
   
    people_colors <- lapply(max_people_colors, function(max_color) {
      pastel_color <- lighten_color(max_color, factor = 0.95)  
      c(pastel_color, max_color)
    })
    
    # Resulting color list with minimum (lighter) and maximum colors
    names(people_colors) <- names(max_people_colors)
    
    
    
    podroze2 <- rbind(podroze_joz, podroze_kla, podroze_mic) %>%
      mutate(activity = case_when(
        activity == "CYCLING" ~ "cycling",
        activity == "IN_PASSENGER_VEHICLE" ~ "in car",
        activity == "in passenger vehicle" ~ "in car",
        activity == "IN_SUBWAY" ~ "in subway",
        activity == "IN_TRAM" ~ "in tram",
        activity == "WALKING" ~ "walking",
        activity == "IN_TRAIN" ~ "in train",
        TRUE ~ activity
      )) %>%
      filter(activity == input$TransportType) %>% 
      mutate(Day = weekdays(as.Date(startTime, format = "%Y-%m-%d")),
             Day = factor(Day, levels = c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday"))) %>%
      mutate(distance = as.numeric(distance)) %>% 
      filter(person == input$selectedPerson2, distance >= 1) %>% 
      group_by(person, Day, weekNum) %>% 
      mutate(distanceSum = sum(distance)) %>% 
      ungroup() %>% 
      select(Day, weekNum, distanceSum) %>% 
      group_by(Day, weekNum, distanceSum) %>% 
      slice(1)
    
    person_color = case_when(
      input$selectedPerson2 == "Jozef" ~ people_colors$Jozef,
      input$selectedPerson2 == "Klaudia" ~ people_colors$Klaudia,
      input$selectedPerson2 == "Michal" ~ people_colors$Michal
    )
    
    podroze2 <- merge(complete_data, podroze2, by = c("weekNum", "Day"), all.x = TRUE)
    #write.csv(podroze2, "df.csv", row.names = FALSE)
    podroze2[is.na(podroze2$distanceSum),"distanceSum"] <- 0
    #write.csv(podroze2, "df2.csv", row.names = FALSE)
    ggplot(podroze2 , aes(x = Day, y = weekNum, fill = distanceSum)) +
      geom_tile(color = "white") +
      scale_fill_gradient(low = person_color[1], high = person_color[2], name = "Distance (m)") +
      labs(title = paste0("Distance ", input$TransportType ," over 6 Weeks by ", input$selectedPerson2), x = "Day of Week", y = "Week") +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
    
    
  })
  
  
    ################################# Mapa #####################################
  # Reactive file path based on selected person
  file_path <- reactive({
    req(input$person)
    paste0(base_path, "Os_czasu_", input$person, ".csv")
  })
  
  # Reactive dataset loading
  dataset <- reactive({
    req(file_path())
    read.csv(file_path(), stringsAsFactors = FALSE)
  })
  
  # Update week choices based on the data
  observe({
    req(dataset())
    updateSelectInput(session, "week", choices = sort(unique(dataset()$relativeWeekNum)))
  })
  
  # Filter data based on selected week
  filtered_data <- reactive({
    req(dataset())
    data <- dataset()
    selected_week <- as.numeric(input$week)
    
    if (!is.null(selected_week)) {
      data <- data %>% filter(relativeWeekNum == selected_week)
    }
    
    # Get top 5 most visited places based on `visitProbability`
    data %>% 
      group_by(placeID, latitude, longitude) %>% 
      summarize(totalVisits = n(), .groups = "drop") %>% 
      arrange(desc(totalVisits)) %>% 
      head(5)
  })
  
  # Render map
  output$map <- renderLeaflet({
    req(filtered_data())
    data <- filtered_data()
    
    leaflet(data) %>%
      addTiles() %>%
      addMarkers(lng = as.numeric(data$longitude), 
                 lat = as.numeric(data$latitude), 
                 popup = paste("Place ID:", data$placeID, "<br>Total Visits:", data$totalVisits))
  })
  
  # Render table
  output$table <- renderTable({
    req(filtered_data())
    filtered_data()
  })
  
}

# Run the application 
shinyApp(ui = ui, server = server)

