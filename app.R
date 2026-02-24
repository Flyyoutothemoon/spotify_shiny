#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#

library(shiny)
library(tidyverse)

# ---- Load Data ----
spotify <- readr::read_csv(
  "https://raw.githubusercontent.com/Flyyoutothemoon/spotify_shiny/refs/heads/main/top_100_spotify_songs_2025.csv"
)%>%
  mutate(
    Release_Date = as.Date(Release_Date),
    Explicit = factor(Explicit)
  )



ui <- fluidPage(
  titlePanel("Spotify Top 100 Songs (2025) — Interactive Visualization"),
  
  sidebarLayout(
    sidebarPanel(
      selectInput(
        "genre",
        "Select Genre:",
        choices = c("All", sort(unique(spotify$Genre))),
        selected = "All"
      ),
      sliderInput(
        "streams",
        "Filter by Spotify Streams (Millions):",
        min = min(spotify$Spotify_Streams_Millions),
        max = max(spotify$Spotify_Streams_Millions),
        value = c(min(spotify$Spotify_Streams_Millions),
                  max(spotify$Spotify_Streams_Millions))
      )
    ),
    
    mainPanel(
      plotOutput("scatter", height = "450px", click = "plot_click"),
      h4("Selected Song Details:"),
      textOutput("song_info")
    )
  )
)


server <- function(input, output, session) {
  
  # ---- Reactive filtered data (Dynamic Query #1) ----
  filtered_data <- reactive({
    data <- spotify
    
    # Genre filtering
    if (input$genre != "All") {
      data <- data %>% filter(Genre == input$genre)
    }
    
    # Streams filtering (slider)
    data <- data %>%
      filter(Spotify_Streams_Millions >= input$streams[1],
             Spotify_Streams_Millions <= input$streams[2])
    
    data
  })
  

  output$scatter <- renderPlot({
    ggplot(filtered_data(),
           aes(x = Popularity_Score,
               y = Spotify_Streams_Millions,
               color = Genre)) +
      geom_point(size = 3, alpha = 0.8) +
      labs(
        x = "Popularity Score",
        y = "Spotify Streams (Millions)",
        color = "Genre",
        title = "Popularity vs. Stream Count"
      ) +
      theme_minimal(base_size = 14)
  })
  
  
  output$song_info <- renderText({
    req(input$plot_click)
    
    nearest <- nearPoints(filtered_data(), input$plot_click, threshold = 10, maxpoints = 1)
    
    if (nrow(nearest) == 0) return("Click a point to display information.")
    
    paste0(
      "Title: ", nearest$Song_Title, "\n",
      "Artist: ", nearest$Artist, "\n",
      "Genre: ", nearest$Genre, "\n",
      "Streams: ", nearest$Spotify_Streams_Millions, "M\n",
      "Popularity Score: ", nearest$Popularity_Score, "\n",
      "Release Date: ", nearest$Release_Date, "\n",
      "Explicit: ", nearest$Explicit
    )
  })
}

# ---- Run App ----
shinyApp(ui, server)
