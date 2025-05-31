library(shiny)
library(ggplot2)
library(zoo)
library(dplyr)

ui <- fluidPage(
  titlePanel("📈 Custom Temperature Forecast App"),
  
  sidebarLayout(
    sidebarPanel(
      fileInput("file", "Upload CSV File (Location, Date, Temperature)", accept = ".csv"),
      uiOutput("locationSelect"),
      dateInput("from_date", "Start Forecast From:", value = Sys.Date()),
      numericInput("forecast_days", "Days to Forecast", value = 7, min = 1),
      numericInput("sma_window", "SMA Window (days)", value = 3, min = 1),
      actionButton("analyze", "Generate Forecast")
    ),
    
    mainPanel(
      plotOutput("tempPlot"),
      tableOutput("forecastTable"),
      uiOutput("forecastInsight")
    )
  )
)

server <- function(input, output, session) {
  # Load CSV
  raw_data <- reactive({
    req(input$file)
    df <- read.csv(input$file$datapath)
    colnames(df) <- tolower(colnames(df))
    df <- df %>%
      mutate(date = as.Date(date),
             location = as.factor(location)) %>%
      arrange(location, date)
    df
  })
  
  # Populate location choices
  output$locationSelect <- renderUI({
    req(raw_data())
    selectInput("selected_location", "Select Location", choices = unique(raw_data()$location))
  })
  
  # Forecast logic
  forecast_data <- eventReactive(input$analyze, {
    req(raw_data(), input$selected_location, input$forecast_days, input$from_date)
    df <- raw_data() %>% filter(location == input$selected_location)
    
    # Use data only before selected forecast date to train SMA
    train_data <- df %>% filter(date <= input$from_date)
    train_data$sma <- rollmean(train_data$temperature, input$sma_window, fill = NA, align = "right")
    
    # Generate forecast
    start_date <- input$from_date + 1
    forecast_dates <- seq(start_date, by = "day", length.out = input$forecast_days)
    forecast_temp <- rep(tail(train_data$sma[!is.na(train_data$sma)], 1), input$forecast_days)
    
    forecast_df <- data.frame(
      location = input$selected_location,
      date = forecast_dates,
      temperature = forecast_temp,
      forecast = TRUE
    )
    
    historical_df <- df %>%
      filter(date <= input$from_date) %>%
      select(location, date, temperature) %>%
      mutate(forecast = FALSE)
    
    bind_rows(historical_df, forecast_df)
  })
  
  # Line chart
  output$tempPlot <- renderPlot({
    df <- forecast_data()
    ggplot(df, aes(x = date, y = temperature, color = forecast)) +
      geom_line(size = 1.2) +
      geom_point() +
      scale_color_manual(values = c("FALSE" = "blue", "TRUE" = "red"), labels = c("Actual", "Forecast")) +
      labs(title = paste("Temperature Forecast -", input$selected_location),
           x = "Date", y = "Temperature (°C)", color = "Legend") +
      theme_minimal()
  })
  
  # Forecast table
  output$forecastTable <- renderTable({
    df <- forecast_data()
    df %>% filter(forecast == TRUE) %>% select(location, date, temperature)
  })
  
  # Forecast insight
  output$forecastInsight <- renderUI({
    df <- forecast_data()
    forecast_df <- df %>% filter(forecast == TRUE)
    avg_forecast <- round(mean(forecast_df$temperature), 2)
    recent_avg <- round(mean(tail(df$temperature[df$forecast == FALSE], input$sma_window)), 2)
    
    trend <- if (avg_forecast > recent_avg) "Rising 🔺" else if (avg_forecast < recent_avg) "Dropping 🔻" else "Stable ⚖️"
    
    HTML(paste0(
      "<h4>📊 Forecast Insight for ", input$selected_location, "</h4>",
      "<b>Forecast Start Date:</b> ", input$from_date + 1, "<br>",
      "<b>Forecast Days:</b> ", input$forecast_days, "<br>",
      "<b>Recent ", input$sma_window, "-Day Average:</b> ", recent_avg, "°C<br>",
      "<b>Forecasted Avg Temperature:</b> ", avg_forecast, "°C<br>",
      "<b>Trend:</b> <span style='color:green;'>", trend, "</span>"
    ))
  })
}

shinyApp(ui = ui, server = server)
