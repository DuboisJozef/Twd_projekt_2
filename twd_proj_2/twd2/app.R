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


lok_joz <- fromJSON(file = "/home/joziop/Documents/twd_proj_2/twd2/Os_czasu_jo.json")
lok_joz <- lok_joz$semanticSegments
lok_mic <- fromJSON(file = "/home/joziop/Documents/twd_proj_2/twd2/Os_czasu_mi.json")
lok_kla <- fromJSON(file = "/home/joziop/Documents/twd_proj_2/twd2/Os_czasu_kl.json")
Sys.setlocale("LC_TIME", "C")


transport <- function(x){
  col_names <- c("activity", "distance", "startTime", "endTime", "weekNum")
  
  
  result <- data.frame(matrix(ncol = length(col_names), nrow = 0))
  colnames(result) <- col_names
  
  counter = 1
  for(i in 1:length(x)){
    if(!is.null(x[[i]]$activity)){
      
      result <- rbind(result, data.frame(activity = x[[i]]$activity$topCandidate$type, 
                                         distance = x[[i]]$activity$distanceMeters,
                                         startTime = x[[i]]$startTime, endTime = x[[i]]$endTime,
                                         weekNum = ceiling(as.numeric(difftime(as.POSIXct(substr(x[[i]]$startTime, 1, 10), format = "%Y-%m-%d"),
                                                                               as.POSIXct("2024-12-05", format = "%Y-%m-%d"), units = "weeks")))))
      counter <- counter + 1
    }
  }
  result
}

miejsca <- function(x){
  
  col_names <- c("place", "startTime", "endTime", "weekNum")
  
  
  result <- data.frame(matrix(ncol = length(col_names), nrow = 0))
  colnames(result) <- col_names
  
  counter = 1
  for(i in 1:length(x)){
    if(!is.null(x[[i]]$visit)){
      ifelse(is.null(x[[i]]$visit$topCandidate$placeId),
      place <- case_when(# x[[i]]$visit$topCandidate$placeID == "ChIJOWqUcOzMHkcROp3KVw-z22k" ~ "Politechnika", # mini
                        x[[i]]$visit$topCandidate$placeID == "ChIJF8u2SOnMHkcR7TrJJ2_WP80" ~ "Politechnika", # gg
                        x[[i]]$visit$topCandidate$placeID == "ChIJU-Q9-DzMHkcRARYuoIMMCx8" ~ "Dom Jozefa",
                        x[[i]]$visit$topCandidate$placeID == "ChIJGVrIUenMHkcRkSvEAxUOK3E" ~ "Politechnika", # angielski
                        x[[i]]$visit$topCandidate$placeID == "ChIJWTUjZY3MHkcR2U7HZ_LJC-s" ~ "Warszawa\nCentralna",
                        x[[i]]$visit$topCandidate$placeID == "ChIJB4mvyljUG0cRYV0oM-DLm2g" ~ "Koluszki",
                        x[[i]]$visit$topCandidate$placeID == "ChIJsQtqT0rTG0cRVbB818vuHhw" ~ "Działka Jozefa",
                        x[[i]]$visit$topCandidate$placeID == "ChIJSS5pkozMHkcRwi0fMeV66cI" ~ "Basen PKiN",
                        #x[[i]]$visit$topCandidate$placeId == "ChIJj1FLqc_MHkcR4HsjgGwILO4" ~ "Kampus Południowy",
                        #x[[i]]$visit$topCandidate$placeId == "CwILO4" ~ "Hen daleko(kampus poludniowy)",
                        x[[i]]$visit$topCandidate$placeID == "ChIJgU4sC8cJ80cRJ5AR6zA94mk" ~ "Dom rodzinny\nwe Francji"
                        TRUE ~ "Inne")
      , place <- case_when(# x[[i]]$visit$topCandidate$placeId == "ChIJOWqUcOzMHkcROp3KVw-z22k" ~ "Politechnika", # mini
                          x[[i]]$visit$topCandidate$placeId == "ChIJF8u2SOnMHkcR7TrJJ2_WP80" ~ "Politechnika", # gg
                          x[[i]]$visit$topCandidate$placeId == "ChIJU-Q9-DzMHkcRARYuoIMMCx8" ~ "Dom Jozefa",
                          x[[i]]$visit$topCandidate$placeId == "ChIJGVrIUenMHkcRkSvEAxUOK3E" ~ "Politechnika", # angielski
                          x[[i]]$visit$topCandidate$placeId == "ChIJWTUjZY3MHkcR2U7HZ_LJC-s" ~ "Warszawa\nCentralna",
                          x[[i]]$visit$topCandidate$placeId == "ChIJB4mvyljUG0cRYV0oM-DLm2g" ~ "Koluszki",
                          x[[i]]$visit$topCandidate$placeId == "ChIJsQtqT0rTG0cRVbB818vuHhw" ~ "Działka Jozefa",
                          x[[i]]$visit$topCandidate$placeId == "ChIJSS5pkozMHkcRwi0fMeV66cI" ~ "Basen PKiN",
                          #x[[i]]$visit$topCandidate$placeId == "ChIJj1FLqc_MHkcR4HsjgGwILO4" ~ "Kampus Południowy",
                          #x[[i]]$visit$topCandidate$placeId == "CwILO4" ~ "Hen daleko(kampus poludniowy)",
                          x[[i]]$visit$topCandidate$placeId == "ChIJgU4sC8cJ80cRJ5AR6zA94mk" ~ "Dom rodzinny\nwe Francji"
                          
                          TRUE ~ "Inne"))
      
      day_diff <- ceiling(as.numeric(difftime(as.POSIXct(substr(x[[i]]$endTime, 1, 10), format = "%Y-%m-%d"),
                                              as.POSIXct(substr(x[[i]]$startTime, 1, 10), format = "%Y-%m-%d"), units = "days"))) + 1
      
      
      for(j in 1:day_diff){
        result <- rbind(result, data.frame(place = place,
                                           startTime = case_when(j == 1 ~ substr(x[[i]]$startTime, 1, 19),
                                                                 TRUE ~ paste0(as.character(tail(seq(as.POSIXct(substr(x[[i]]$startTime, 1, 19), format = "%Y-%m-%d"), by= "day", length = j), n = 1)), "T00:00:00")), 
                                           endTime = case_when(j == day_diff ~ substr(x[[i]]$endTime, 1, 19),
                                                               j == 1 ~ paste0(as.character(as.POSIXct(substr(x[[i]]$startTime, 1, 19), format = "%Y-%m-%d")), "T23:59:59"),
                                                               TRUE ~ paste0(as.character(tail(seq(as.POSIXct(substr(x[[i]]$startTime, 1, 19), format = "%Y-%m-%d"), by= "day", length = j), n = 1)), "T23:59:59")),
                                           weekNum = ceiling(as.numeric(difftime(as.POSIXct(substr(x[[i]]$startTime, 1, 10), format = "%Y-%m-%d"),
                                                                                 as.POSIXct("2024-12-05", format = "%Y-%m-%d"), units = "weeks")))))
      }
      counter <- counter + 1
    }
  }
  result[result$place != "Inne",]
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
                               min = 1, max = 5, value = c(1, 2), step = 1),
                 
                 selectInput("dropdown1", "Choose the person:",
                             choices = c("Jozef", "Michal", "Klaudia"))
                 ),
                 mainPanel(
                   plotOutput("dailyActivitiesPlot")
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
                     selected = c("Jozef", "Klaudia", "Michal"),  
                     multiple = TRUE  
                   )
                 ),
                 mainPanel(
                   plotOutput("transportSpeedPlot")
                 )
               )
      )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {

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
      
      czas_w_transporcie <- podroze %>%
        filter(weekNum >= input$sliderWeek1[1] & weekNum <=  input$sliderWeek1[2]) %>% 
        mutate(dayOfWeek = weekdays(as.Date(endTime, format = "%Y-%m-%d")),
               dayOfWeek = factor(dayOfWeek, levels = c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday"))) %>% 
        group_by(dayOfWeek) %>% 
        mutate(meanTimeDur = sum(timeDurSec)/(input$sliderWeek1[2] - input$sliderWeek1[1] + 1)) %>% 
        ungroup() %>% 
        group_by() %>% 
        select(dayOfWeek, meanTimeDur) %>% 
        group_by(dayOfWeek, meanTimeDur) %>% 
        slice(1) %>% 
        mutate(place = "transport")
      
      czas_miejsca <- wizyty %>% 
        filter(weekNum >= input$sliderWeek1[1] & weekNum <=  input$sliderWeek1[2]) %>% 
        mutate(day = as.POSIXct(substr(endTime, 1, 10), format = "%Y-%m-%d")) %>%
        mutate(dayOfWeek = weekdays(as.Date(endTime, format = "%Y-%m-%d")),
               dayOfWeek = factor(dayOfWeek, levels = c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday"))) %>%
        select(dayOfWeek, place, timeDurSec) %>% 
        group_by(dayOfWeek, place) %>% 
        mutate(meanTimeDur = sum(timeDurSec)/(input$sliderWeek1[2] - input$sliderWeek1[1] + 1)) %>% 
        ungroup() %>% 
        select(dayOfWeek, place, meanTimeDur) %>% 
        group_by(dayOfWeek, place, meanTimeDur) %>% 
        slice(1)
      
      czas <- rbind(czas_miejsca, czas_w_transporcie)
      
      
      
      wyk2 <- ggplot(czas, aes(y = meanTimeDur, x = dayOfWeek, fill = place))+
        geom_bar(stat = "identity", position = "fill") +
        labs(fill = "Place") +                         
        xlab("Day of week") +                                     
        ylab("% of time of day") +                      
        ggtitle(paste0("Average daily time spent in each place by ", input$dropdown1)) +     
        theme_minimal()     
      
      wyk2
    })
    
    output$transportSpeedPlot <- renderPlot({
      # pozniej dodam zeby kilka osob na raz dalo sie wziac
      
      
      
      podroze2 <- rbind(podroze_joz, podroze_kla, podroze_mic)%>%
        mutate(Day = weekdays(as.Date(startTime, format = "%Y-%m-%d")),
               Day = factor(Day, levels = c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday"))) %>% 
        filter(person %in% input$People) %>% 
        mutate(activity2 = case_when(activity == "CYCLING" ~ "cycling",
                                     activity == "IN_PASSENGER_VEHICLE" ~ "in car",
                                     activity == "IN_SUBWAY" ~ "in subway",
                                     activity == "IN_TRAM" ~ "in tram",
                                     activity == "WALKING" ~ "walking",
                                     activity == "IN_TRAIN" ~ "in train",
                                     TRUE ~ activity)) %>% 
        filter(distance >= 1)
      
      
      agg_data <- podroze2 %>%
        group_by(Day, activity2) %>%
        summarise(mean_speed = mean(as.numeric(distance) / as.numeric(timeDurSec), na.rm = TRUE), .groups = 'drop')
        
      
      
      wyk1 <- ggplot(agg_data, aes(x = Day, y = mean_speed, fill = activity2)) +
        geom_col(position = position_dodge(width = 0.9)) + 
        labs(fill = "") +                        
        xlab("Day") +                                     
        ylab("Average Speed (m/s)") +                      
        ggtitle("Average speed by different means of transport") +     
        theme_minimal()       
      
      wyk1
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
