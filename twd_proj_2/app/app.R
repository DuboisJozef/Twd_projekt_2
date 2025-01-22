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
library(tidyr)
library(bslib)
library(shinythemes)
library(shinymaterial)


lok_joz <- fromJSON(file = "../data/Os_czasu_jo.json")
lok_joz <- lok_joz$semanticSegments
lok_mic <- fromJSON(file = "../data/Os_czasu_mi.json")
lok_kla <- fromJSON(file = "../data/Os_czasu_kl.json")

# read merged df
merged_df <- read.csv("../data/merged_data.csv")

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
               x[[i]]$visit$topCandidate$placeID == "ChIJgU4sC8cJ80cRJ5AR6zA94mk" ~ "Dom rodzinny\nwe Francji",
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
               x[[i]]$visit$topCandidate$placeId == "ChIJgU4sC8cJ80cRJ5AR6zA94mk" ~ "Dom rodzinny\nwe Francji",
               
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

######################################################################UI######################################################################

ui <- fluidPage(
  
  theme = bs_theme(
    version = 5,            
    bootswatch = "zephyr",   
    primary = "#4285F4",    
    secondary = "#0f9d58", 
    success = "#FBBC05", 
    warning = "#E94335", 
    base_font = font_google("Product Sans"),  
    heading_font = font_google("Product Sans")  
  ),
  
  tags$style(HTML("
  body {
    background-color: #f5f5f5;
  }
  ")),
  
  # start page
  uiOutput("startPage"),
  
  # app
  uiOutput("mainApp")
)


server <- function(input, output, session) {
  
  values <- reactiveValues(showStartPage = TRUE)
  
  output$startPage <- renderUI({
    if (values$showStartPage) {
      
      tagList(
        leafletOutput("mapBackground", height = "100vh"),
        
        tags$style(HTML("
      
        body {
          background-image: url('http://mapa-google.pl/warszawa/'); 
          background-size: cover;
          background-position: center;
          height: 100vh;
          margin: 0;
          font-family: 'Product Sans', sans-serif;
        }
        
        #person_icon {
        position: absolute;
        top: 10px;
        right: 20px;
        padding: 10px;
        }
        
        ")),
        
        tags$img(id = "person_icon", src = "https://cdn-icons-png.flaticon.com/512/3177/3177440.png", height = "70px", width = "70px"),
        
        div(style = "text-align: center;
        font-size: 100px;
        height: 200px;
        width: 400px;
        position: absolute;
        top: 40%;
        left: 50%;
        transform: translate(-50%, -50%);",
            
            h1(style = "text-align: center; font-size: 100px; font-weight: 1000",
               HTML(
                 paste0(
                   # Google colors to each letter
                   paste0(
                     '<span style="color: #4285F4;">', substr("Map My Moments", 1, 1), '</span>',
                     '<span style="color: #0f9d58;">', substr("Map My Moments", 2, 2), '</span>',
                     '<span style="color: #FBBC05;">', substr("Map My Moments", 3, 3), '</span>',
                     
                     '<span style="color: #EA4335;">', substr("Map My Moments", 4, 4), '</span>',
                     
                     '<span style="color: #4285F4;">', substr("Map My Moments", 5, 5), '</span>',
                     '<span style="color: #0f9d58;">', substr("Map My Moments", 6, 6), '</span>',
                     
                     '<span style="color: #FBBC02;">', substr("Map My Moments", 7, 7), '</span>',
                     
                     '<span style="color: #E94335;">', substr("Map My Moments", 8, 8), '</span>',
                     '<span style="color: #4285F4;">', substr("Map My Moments", 9, 9), '</span>',
                     '<span style="color: #0f9d58;">', substr("Map My Moments", 10, 10), '</span>',
                     '<span style="color: #FBBC05;">', substr("Map My Moments", 11, 11), '</span>',
                     '<span style="color: #E94335;">', substr("Map My Moments", 12, 12), '</span>',
                     '<span style="color: #4285F4;">', substr("Map My Moments", 13, 13), '</span>',
                     '<span style="color: #0f9d58;">', substr("Map My Moments", 14, 14), '</span>'
                   )
                 )
               )
            ),
            
            # p("Welcome to the app", style = "font-size: 20px; color: #5F6368; font-weight: 00;"),
            
            actionButton("startBtn", label = NULL, 
                         icon = icon("arrow-right"), 
                         class = "btn-enter",
                         style = "background-color: #4285F4; 
                     color: white; 
                     font-size: 18px; 
                     padding: 15px 30px; 
                     border-radius: 5px; 
                     border: none; 
                     cursor: pointer; 
                     font-weight: 500; 
                     box-shadow: 0 2px 6px rgba(0,0,0,0.2);")
        )
      )
    }
  })
  
  
  output$mainApp <- renderUI({
    if (!values$showStartPage) {
      tagList(
        
        tags$head(
          tags$link(rel = "stylesheet", href = "https://fonts.googleapis.com/icon?family=Material+Icons")
        ),
        
        tags$style(HTML("

        #person_icon {
        position: absolute;
        top: 10px;
        right: 20px;
        padding: 10px;
        }
        
        .nav-tabs {
          display: block;
          width: 90px;
          float: left;
          background-color: f1f1f1;
          position: fixed;
          top: 50%;
          transform: translateY(-50%);
        }
        
        .tab-content {
          margin-left: 100px;
        }
        
        .tab-content-wrapper {
          background-color: #333; 
        }
        
        .fixed-bottom-row {
          position: fixed;
          bottom: 0;
          width: 100%;
          background-color: #f8f9fa;
          padding: 10px 20px;
          box-shadow: 0 -2px 5px rgba(0, 0, 0, 0.1);
          z-index: 1000;
        }

        }

        ")),
        
        tags$img(id = "person_icon", src = "https://cdn-icons-png.flaticon.com/512/3177/3177440.png", height = "70px", width = "70px"),
        
        div(
          class = "fixed-bottom-row",
          fluidRow(
            column(4,
                   sliderInput(
                     "sliderWeekMap", 
                     "Select weeks for the plot:",
                     min = 1, max = 5, value = c(1, 2), step = 1
                   )
            ),
            column(4,
                   selectInput(
                     inputId = "PeopleMap",
                     label = "Select people:",
                     choices = c("Jozef", "Klaudia", "Michal"),
                     selected = c("Jozef", "Klaudia", "Michal"),
                     multiple = TRUE
                   )
            ),
            column(4,
                   conditionalPanel(
                     condition = "input.mainTabs === 'mapTab'",
                     sliderInput(
                       inputId = "topPlacesCountMap",
                       label = "Select number of top places to display:",
                       min = 1, max = 20, value = 5, step = 1
                     )
                   )
            )
          )
        ),
        
        titlePanel(
          
          h1(style = "font-size: 60px; font-weight: 1000; padding: 10px",
             
             HTML(
               
               paste0(
                 
                 # Google colors to each letter
                 paste0(
                   '<span style="color: #4285F4;">', substr("Map My Moments", 1, 1), '</span>',
                   '<span style="color: #0f9d58;">', substr("Map My Moments", 2, 2), '</span>',
                   '<span style="color: #FBBC05;">', substr("Map My Moments", 3, 3), '</span>',
                   
                   '<span style="color: #EA4335;">', substr("Map My Moments", 4, 4), '</span>',
                   
                   '<span style="color: #4285F4;">', substr("Map My Moments", 5, 5), '</span>',
                   '<span style="color: #0f9d58;">', substr("Map My Moments", 6, 6), '</span>',
                   
                   '<span style="color: #FBBC02;">', substr("Map My Moments", 7, 7), '</span>',
                   
                   '<span style="color: #E94335;">', substr("Map My Moments", 8, 8), '</span>',
                   '<span style="color: #4285F4;">', substr("Map My Moments", 9, 9), '</span>',
                   '<span style="color: #0f9d58;">', substr("Map My Moments", 10, 10), '</span>',
                   '<span style="color: #FBBC05;">', substr("Map My Moments", 11, 11), '</span>',
                   '<span style="color: #E94335;">', substr("Map My Moments", 12, 12), '</span>',
                   '<span style="color: #4285F4;">', substr("Map My Moments", 13, 13), '</span>',
                   '<span style="color: #0f9d58;">', substr("Map My Moments", 14, 14), '</span>'
                 )
                 
               )
               
             )
          )
          
        ),
        
        tabsetPanel(
          id = "mainTabs",
          type = "tabs",
          
          tabPanel(
            # title = tagList(
            #   tags$img(src = "https://cdn-icons-png.flaticon.com/128/2773/2773319.png", height = "30px", width = "30px"),
            #   " Weekly Activities"
            # ),
            title = tags$img(src = "https://cdn-icons-png.flaticon.com/128/2773/2773319.png", height = "40px", width = "40px"),
            
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
          tabPanel(
            
            title = tags$img(src = "https://cdn-icons-png.flaticon.com/128/1034/1034795.png", height = "40px", width = "40px"),
            
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
          
          tabPanel(
            
            title = tags$img(src = "https://cdn-icons-png.flaticon.com/128/7552/7552703.png", height = "40px", width = "40px"),
            
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
          ),
          
          tabPanel(
            
            title = tags$img(src = "https://cdn-icons-png.flaticon.com/128/854/854878.png", height = "40px", width = "40px"),
            value = 'mapTab',
            
            fluidRow(
              column(
                12,
                leafletOutput("map", width = "100%", height = "500px")
              )
            )
          )
          
          
        )
      )
    }
  })
  
  
  
  
  observeEvent(input$startBtn, {
    values$showStartPage <- FALSE
    shinyjs::show("loading")
    
    Sys.sleep(2)
    
    shinyjs::hide("loading")
    
  })
  
  
  
  
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
  
  
  ############################## Predkość ######################################
  
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
  
  
  ################################# Mapa #####################################
  # filter data
  filtered_data <- reactive({
    person_map <- c("Jozef" = "jo", "Michal" = "mi", "Klaudia" = "kl")
    selected_people <- input$PeopleMap
    ppl_map <- person_map[selected_people]
    
    merged_df %>%
      filter(relativeWeekNum >= input$sliderWeekMap[1] & relativeWeekNum <= input$sliderWeekMap[2]) %>%
      filter(person %in% ppl_map) %>% 
      filter(!is.na(placeName))
  })
  
  # map title
  output$dynamicTitleMap <- renderUI({
    h4(paste("Top", input$topPlacesCountMap, "Most Visited Places"))
  })
  
  # map
  output$map <- renderLeaflet({
    data_filtered <- filtered_data()
    
    top_n <- input$topPlacesCountMap
    
    # get top n places
    top_places <- data_filtered %>%
      count(placeName, latitude, longitude, person) %>%
      arrange(desc(n)) %>%
      head(top_n)
    
    # calculate values for setView
    lat_range <- max(top_places$latitude, na.rm = TRUE) - min(top_places$latitude, na.rm = TRUE)
    lng_range <- max(top_places$longitude, na.rm = TRUE) - min(top_places$longitude, na.rm = TRUE)
    
    if (nrow(top_places) > 0) {
      center_lat <- mean(top_places$latitude, na.rm = TRUE)
      center_lng <- mean(top_places$longitude, na.rm = TRUE)
      zoom <- ifelse(lat_range > 1 || lng_range > 1, 7, 11)
    } else {
      # warsaw default
      center_lat <- 52.2298
      center_lng <- 21.0118
      zoom <- 10
    }
    
    # colors
    color_palette <- colorFactor(c("red", "green", "blue"), levels = c("jo", "kl", "mi"))
    
    # plot
    leaflet(data = top_places) %>%
      addProviderTiles(providers$CartoDB.Positron) %>%
      setView(lng = center_lng, lat = center_lat, zoom = zoom) %>% 
      addCircleMarkers(
        lat = ~latitude, lng = ~longitude,
        radius = ~sqrt(n) * 3,
        color = ~color_palette(person),
        popup = ~paste(
          "<strong>Place Name:</strong>", placeName, "<br>",
          "<strong>Visits:</strong>", n, "<br>"
        ) 
      ) 
  })
  
  ################################# Mapa tło #####################################
  output$mapBackground <- renderLeaflet({
    leaflet() %>%
      addProviderTiles(providers$CartoDB.Positron)  %>%  # 
      setView(lng = 21.0122, lat = 52.2298, zoom = 12) 
  })
  
}

# Run the application 
shinyApp(ui = ui, server = server)