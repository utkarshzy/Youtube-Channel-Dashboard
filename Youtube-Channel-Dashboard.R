# Load Libraries
library(httr)
library(jsonlite)
library(shiny)
library(shinydashboard)
library(ggplot2)
library(plotly)

youtube_api_key <- "AIzaSyCHVKEshQmQpeHA18Re8GyaDHddEnepgGw"  
channel_id <- "UCAuUUnT6oDeKwE6v1NGQxug"  # TED Channel ID

# === Step 2: Construct the API URL ===
url <- paste0("https://www.googleapis.com/youtube/v3/channels?part=snippet,statistics&id=",
              channel_id, "&key=", youtube_api_key)

# === Step 3: Make the API call ===
response <- GET(url)
data_text <- content(response, as = "text", encoding = "UTF-8")
json_data <- fromJSON(data_text, flatten = TRUE)

# === Step 4: Error handling ===
if (length(json_data$items) == 0) {
  stop("❌ No data found. Check your API key and channel ID.")
}

channel_info <- json_data$items[1, ]

# Build DataFrame for visualization
metrics_df <- data.frame(
  Metric = c("Subscribers", "Views", "Videos"),
  Count = as.numeric(c(channel_info$statistics.subscriberCount,
                       channel_info$statistics.viewCount,
                       channel_info$statistics.videoCount))
)

# === Step 6: Shiny Dashboard UI ===
ui <- dashboardPage(
  dashboardHeader(title = paste("YouTube Dashboard -", channel_info$snippet.title)),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Overview", tabName = "overview", icon = icon("dashboard")),
      menuItem("Charts", tabName = "charts", icon = icon("chart-bar"))
    )
  ),
  dashboardBody(
    tabItems(
      tabItem(tabName = "overview",
              fluidRow(
                valueBoxOutput("subscribersBox"),
                valueBoxOutput("viewsBox"),
                valueBoxOutput("videosBox")
              )
      ),
      tabItem(tabName = "charts",
              fluidRow(
                box(title = "Bar Chart", plotlyOutput("barPlot"), width = 6),
                box(title = "Pie Chart", plotlyOutput("pieChart"), width = 6)
              ),
              fluidRow(
                box(title = "Scatter Plot", plotlyOutput("scatterPlot"), width = 6),
                
              )
      )
    )
  )
)

server <- function(input, output) {
  output$subscribersBox <- renderValueBox({
    valueBox(formatC(channel_info$statistics.subscriberCount, format = "d", big.mark = ","),
             "Subscribers", icon = icon("users"), color = "blue")
  })
  
  output$viewsBox <- renderValueBox({
    valueBox(formatC(channel_info$statistics.viewCount, format = "d", big.mark = ","),
             "Total Views", icon = icon("eye"), color = "green")
  })
  
  output$videosBox <- renderValueBox({
    valueBox(formatC(channel_info$statistics.videoCount, format = "d", big.mark = ","),
             "Total Videos", icon = icon("video"), color = "red")
  })
  
  output$barPlot <- renderPlotly({
    p <- ggplot(metrics_df, aes(x = Metric, y = Count, fill = Metric)) +
      geom_col() + theme_minimal()
    ggplotly(p)
  })
  
  output$pieChart <- renderPlotly({
    plot_ly(metrics_df, labels = ~Metric, values = ~Count, type = "pie")
  })
  
  output$scatterPlot <- renderPlotly({
    p <- ggplot(metrics_df, aes(x = Metric, y = Count, color = Metric)) +
      geom_point(size = 5) + theme_minimal()
    ggplotly(p)
  })
  
  
}

shinyApp(ui, server)
