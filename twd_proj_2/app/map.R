library(shiny)
library(dplyr)
library(leaflet)


base_path <- "./twd_proj_2/data/"

ui <- fluidPage(
  titlePanel("Top 5 Most Visited Places"),
  
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

server <- function(input, output, session) {
  
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

# Run the app
shinyApp(ui = ui, server = server)
