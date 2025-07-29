library(shiny)
library(shinydashboard)
library(ggplot2)
library(dplyr)
library(scales)
library(DT)
library(plotly)
library(lubridate)

set.seed(123)
num_months <- 60
start_date <- as.Date("2020-01-01")
dates <- seq(start_date, by = "month", length.out = num_months)

all_data <- data.frame(
  Month = dates,
  MedianPrice = round(runif(num_months, 300000, 550000) + cumsum(rnorm(num_months, 500, 200))),
  Inventory = round(runif(num_months, 1500, 3000) - cumsum(rnorm(num_months, 10, 5))),
  DOM = round(runif(num_months, 15, 60) - cumsum(rnorm(num_months, 0.5, 0.2))),
  PricePerSqFt = round(runif(num_months, 200, 350) + cumsum(rnorm(num_months, 2, 1))),
  SalesVolume = round(runif(num_months, 100, 250) + cumsum(rnorm(num_months, 0.8, 0.4)))
)

all_data$DOM[all_data$DOM < 1] <- 1
all_data$Inventory[all_data$Inventory < 1] <- 1

ui <- dashboardPage(
  skin = "purple",
  dashboardHeader(title = "Advanced Real Estate Dashboard"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Dashboard Overview", tabName = "dashboard", icon = icon("chart-line")),
      menuItem("Data Explorer", tabName = "data_table", icon = icon("table")),
      menuItem("About This App", tabName = "about", icon = icon("info-circle"))
    ),
    dateRangeInput(
      inputId = "dateRange",
      label = "Select Date Range:",
      start = min(all_data$Month),
      end = max(all_data$Month),
      min = min(all_data$Month),
      max = max(all_data$Month),
      format = "yyyy-mm-dd",
      startview = "year",
      separator = "to"
    ),
    selectInput(
      inputId = "metric",
      label = "Choose a market metric:",
      choices = c(
        "Median Home Price" = "MedianPrice",
        "Inventory Levels" = "Inventory",
        "Days on Market" = "DOM",
        "Price Per Square Foot" = "PricePerSqFt",
        "Sales Volume" = "SalesVolume"
      ),
      selected = "MedianPrice"
    ),
    conditionalPanel(
      condition = "input.metric == 'Inventory' || input.metric == 'SalesVolume'",
      radioButtons(
        inputId = "plotType",
        label = "Select Plot Type:",
        choices = c("Bar Chart" = "bar", "Line Chart" = "line"),
        selected = "bar"
      )
    ),
    actionButton("reset_zoom", "Reset Date Zoom", icon = icon("calendar-alt")),
    downloadButton("downloadData", "Download Filtered Data", icon = icon("download"))
  ),
  dashboardBody(
    tags$head(tags$style(HTML('
      .content-wrapper, .right-side {
        background-color: #f8f9fa;
      }
      .small-box.bg-aqua { background-color: #5bc0de !important; color: #fff !important; }
      .small-box.bg-purple { background-color: #6f42c1 !important; color: #fff !important; }
      .small-box.bg-green { background-color: #28a745 !important; color: #fff !important; }
      .small-box.bg-yellow { background-color: #ffc107 !important; color: #fff !important; }
      .small-box.bg-red { background-color: #dc3545 !important; color: #fff !important; }
      .main-header .navbar {
        background-color: #555555;
      }
      .main-header .logo {
        background-color: #444444;
      }
      .sidebar-menu > li.active > a,
      .sidebar-menu > li.active .treeview-menu > li.active > a {
        border-left-color: #8a2be2 !important;
      }
      .small-box h3 {
        font-size: 2.0em;
        margin-top: 0px;
        margin-bottom: 0px;
        white-space: nowrap;
        overflow: hidden;
        text-overflow: ellipsis;
      }
      .small-box p {
        font-size: 1.0em;
      }
      .info-box-text {
        font-size: 1.0em;
        font-weight: bold;
      }
      .info-box-number {
        font-size: 1.5em;
        white-space: normal;
        overflow: visible;
        text-overflow: clip;
      }
      .col-sm-4, .col-sm-6 {
        flex-grow: 1;
      }
      .small-box, .info-box {
        height: 120px;
        display: flex;
        flex-direction: column;
        justify-content: center;
        align-items: flex-start;
        padding: 10px;
      }
      .small-box .icon, .info-box-icon {
        top: 50%;
        transform: translateY(-50%);
        font-size: 50px;
      }
    '))),
    tabItems(
      tabItem(tabName = "dashboard",
              fluidRow(
                valueBoxOutput("latestMedianPrice"),
                valueBoxOutput("latestInventory"),
                valueBoxOutput("latestDOM") 
              ),
              fluidRow(
                valueBoxOutput("latestPricePerSqFt"),
                valueBoxOutput("latestSalesVolume")
              ),
              fluidRow(
                box(
                  title = textOutput("plotTitle"),
                  status = "primary",
                  solidHeader = TRUE,
                  width = 12,
                  plotlyOutput("trendPlot", height = 450)
                )
              )
      ),
      tabItem(tabName = "data_table",
              fluidRow(
                box(
                  title = "Filtered Raw Data",
                  status = "primary",
                  solidHeader = TRUE,
                  width = 12,
                  DTOutput("dataTable")
                )
              )
      ),
      tabItem(tabName = "about",
              fluidRow(
                box(
                  title = "About This Real Estate Dashboard",
                  status = "info",
                  solidHeader = TRUE,
                  width = 12,
                  h3("Purpose"),
                  p("This dashboard provides an interactive visualization of key real estate market trends over time. It allows users to select different metrics and date ranges to explore market dynamics."),
                  h3("Data Source"),
                  p("The data presented here is ", tags$strong("simulated"), " for demonstration purposes. It does not represent actual market data."),
                  h3("Key Metrics Explained"),
                  tags$ul(
                    tags$li(tags$strong("Median Home Price:"), " The mid-point of home prices in the market, indicating affordability and market value."),
                    tags$li(tags$strong("Inventory Levels:"), " The total number of residential properties available for sale, a key indicator of supply."),
                    tags$li(tags$strong("Days on Market (DOM):"), " The average number of days a property spends on the market before being sold, reflecting buyer demand and market speed."),
                    tags$li(tags$strong("Price Per Square Foot:"), " The average sale price divided by the property's square footage, useful for comparing property values across different sizes."),
                    tags$li(tags$strong("Sales Volume:"), " The total number of residential property transactions completed within a period, indicating market activity.")
                  ),
                  h3("Technology Stack"),
                  p("Built entirely in R using the following powerful packages:"),
                  tags$ul(
                    tags$li(tags$a(href="https://shiny.rstudio.com/", target="_blank", "Shiny"), ": For creating the interactive web application."),
                    tags$li(tags$a(href="https://rstudio.github.io/shinydashboard/", target="_blank", "shinydashboard"), ": For a structured and professional dashboard layout."),
                    tags$li(tags$a(href="https://ggplot2.tidyverse.org/", target="_blank", "ggplot2"), ": For generating high-quality static visualizations."),
                    tags$li(tags$a(href="https://plotly.com/r/", target="_blank", "plotly"), ": For converting ggplot2 charts into interactive web-based graphs."),
                    tags$li(tags$a(href="https://dplyr.tidyverse.org/", target="_blank", "dplyr"), ": For efficient data manipulation and transformation."),
                    tags$li(tags$a(href="https://lubridate.tidyverse.org/", target="_blank", "lubridate"), ": For simplified date and time handling."),
                    tags$li(tags$a(href="https://rstudio.github.io/DT/", target="_blank", "DT"), ": For creating interactive HTML data tables."),
                    tags$li(tags$a(href="https://scales.r-lib.org/", target="_blank", "scales"), ": For advanced data scaling and formatting on plots and in text.")
                  ),
                  h3("Developed By: Ojas and Natania ❤️"), 
                  h4("Team: Team Tang"),
                  p(Sys.Date())
                )
              )
      )
    )
  )
)

server <- function(input, output, session) {
  
  filtered_data <- reactive({
    req(input$dateRange)
    df <- all_data %>%
      filter(Month >= input$dateRange[1] & Month <= input$dateRange[2])
    if(nrow(df) < 2) {
      return(NULL)
    }
    df
  })
  
  trend_data <- reactive({
    data <- filtered_data()
    req(data)
    data %>%
      arrange(Month) %>%
      mutate(
        MedianPrice_prev = lag(MedianPrice, 1),
        Inventory_prev = lag(Inventory, 1),
        DOM_prev = lag(DOM, 1),
        PricePerSqFt_prev = lag(PricePerSqFt, 1),
        SalesVolume_prev = lag(SalesVolume, 1)
      )
  })
  
  observeEvent(input$reset_zoom, {
    updateDateRangeInput(session, "dateRange",
                         start = min(all_data$Month),
                         end = max(all_data$Month))
  })
  
  plot_title_text <- reactive({
    switch(input$metric,
           "MedianPrice" = "Median Home Prices Over Time",
           "Inventory" = "Inventory Levels Over Time",
           "DOM" = "Average Days on Market Over Time",
           "PricePerSqFt" = "Price Per Square Foot Over Time",
           "SalesVolume" = "Sales Volume Over Time"
    )
  })
  
  output$plotTitle <- renderText({
    plot_title_text()
  })
  
  output$trendPlot <- renderPlotly({
    data <- filtered_data()
    req(data)
    
    selected_metric <- input$metric
    plot_type <- if (selected_metric %in% c("Inventory", "SalesVolume")) input$plotType else "line"
    
    y_label <- switch(selected_metric,
                      "MedianPrice" = "Median Price (USD)",
                      "Inventory" = "Number of Homes Available",
                      "DOM" = "Days on Market",
                      "PricePerSqFt" = "Price Per Sq Ft (USD)",
                      "SalesVolume" = "Number of Homes Sold"
    )
    
    p <- ggplot(data, aes_string(x = "Month", y = selected_metric)) +
      labs(x = "Month", y = y_label) +
      theme_minimal(base_size = 15) +
      scale_x_date(date_labels = "%b %Y", date_breaks = "3 months") +
      theme(
        plot.title = element_text(hjust = 0.5, face = "bold", size = 20),
        axis.title.x = element_text(face = "bold", size = 14),
        axis.title.y = element_text(face = "bold", size = 14),
        axis.text.x = element_text(angle = 45, hjust = 1, size = 12),
        axis.text.y = element_text(size = 12),
        panel.grid.major = element_line(color = "grey85"),
        panel.grid.minor = element_line(color = "grey95")
      )
    
    if (selected_metric %in% c("MedianPrice", "DOM", "PricePerSqFt") || plot_type == "line") {
      p <- p + geom_line(color = '#6f42c1', size = 1.2) +
        geom_point(color = '#8a2be2', size = 3)
      if (selected_metric == "MedianPrice") {
        p <- p + scale_y_continuous(labels = dollar_format(accuracy = 1))
      }
      if (selected_metric == "PricePerSqFt") {
        p <- p + geom_smooth(method = "loess", se = FALSE, color = '#ffc107', linetype = "dashed", size = 1)
      }
    }
    
    if ((selected_metric %in% c("Inventory", "SalesVolume")) && plot_type == "bar") {
      fill_color <- if (selected_metric == "Inventory") "#5bc0de" else "#28a745"
      border_color <- if (selected_metric == "Inventory") "#31708f" else "#1c7430"
      p <- p + geom_bar(stat = "identity", fill = fill_color, color = border_color, width = 0.7) +
        geom_text(aes_string(label = selected_metric), vjust = -0.5, color = 'black', size = 3)
    }
    
    ggplotly(p)
  })
  
  format_change <- function(current, previous) {
    if (is.na(previous) || previous == 0) return("")
    current <- as.numeric(current)
    previous <- as.numeric(previous)
    
    if (is.na(previous) || previous == 0) return("") 
    
    change <- ((current - previous) / previous) * 100
    icon_type <- if (change >= 0) "arrow-up" else "arrow-down"
    color <- if (change >= 0) "green" else "red"
    paste0("(", round(change, 1), "%) ", tags$i(class = paste0("fa fa-", icon_type), style = paste0("color:", color, ";")))
  }
  
  output$latestMedianPrice <- renderValueBox({
    data <- trend_data()
    req(data)
    latest_val <- tail(data$MedianPrice, 1)
    prev_val <- tail(data$MedianPrice_prev, 1)
    subtitle_text <- paste("Latest Median Price", format_change(latest_val, prev_val))
    valueBox(
      value = tags$p(dollar(latest_val, accuracy = 1), style = "font-size: 2.0em;"),
      subtitle = HTML(subtitle_text),
      icon = icon("dollar-sign"),
      color = "purple"
    )
  })
  
  output$latestInventory <- renderValueBox({
    data <- trend_data()
    req(data)
    latest_val <- tail(data$Inventory, 1)
    prev_val <- tail(data$Inventory_prev, 1)
    subtitle_text <- paste("Latest Inventory", format_change(latest_val, prev_val))
    valueBox(
      value = tags$p(latest_val, style = "font-size: 2.0em;"),
      subtitle = HTML(subtitle_text),
      icon = icon("home"),
      color = "blue"
    )
  })
  
  output$latestDOM <- renderValueBox({
    data <- trend_data()
    req(data)
    latest_val <- tail(data$DOM, 1)
    prev_val <- tail(data$DOM_prev, 1)
    subtitle_text <- paste("Latest Days on Market", format_change(latest_val, prev_val))
    valueBox(
      value = tags$p(latest_val, style = "font-size: 2.0em;"),
      subtitle = HTML(subtitle_text),
      icon = icon("clock"),
      color = "yellow"
    )
  })
  
  output$latestPricePerSqFt <- renderValueBox({
    data <- trend_data()
    req(data)
    latest_val <- tail(data$PricePerSqFt, 1)
    prev_val <- tail(data$PricePerSqFt_prev, 1)
    subtitle_text <- paste("Latest Price/SqFt", format_change(latest_val, prev_val))
    valueBox(
      value = tags$p(dollar(latest_val, accuracy = 1), style = "font-size: 2.0em;"),
      subtitle = HTML(subtitle_text),
      icon = icon("money-bill-wave"),
      color = "green"
    )
  })
  
  output$latestSalesVolume <- renderValueBox({
    data <- trend_data()
    req(data)
    latest_val <- tail(data$SalesVolume, 1)
    prev_val <- tail(data$SalesVolume_prev, 1)
    subtitle_text <- paste("Latest Sales Volume", format_change(latest_val, prev_val))
    valueBox(
      value = tags$p(latest_val, style = "font-size: 2.0em;"),
      subtitle = HTML(subtitle_text),
      icon = icon("chart-bar"),
      color = "red"
    )
  })
  
  output$dataTable <- renderDT({
    req(filtered_data())
    datatable(
      filtered_data(),
      options = list(pageLength = 15, scrollX = TRUE),
      rownames = FALSE,
      filter = 'top'
    ) %>%
      formatDate('Month', method = 'toDateString') %>%
      formatCurrency('MedianPrice', currency = "$", interval = 3, mark = ",", digits = 0) %>%
      formatCurrency('PricePerSqFt', currency = "$", interval = 3, mark = ",", digits = 0)
  })
  
  output$downloadData <- downloadHandler(
    filename = function() {
      paste("real_estate_data-filtered-", Sys.Date(), ".csv", sep = "")
    },
    content = function(file) {
      write.csv(filtered_data(), file, row.names = FALSE)
    }
  )
}

shinyApp(ui = ui, server = server)