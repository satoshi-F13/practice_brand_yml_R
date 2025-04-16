# Load required libraries
library(shiny)
library(bslib)
library(dplyr)
library(ggplot2)
library(plotly)
library(lubridate)
library(DT)

# Generate sample sales data
set.seed(123)
months <- rep(month.abb, 3)
years <- rep(c("2021", "2022", "2023"), each = 12)
date <- paste(months, years)
categories <- c("Electronics", "Furniture", "Clothing", "Books", "Food")

sales_data <- data.frame(
  Date = date,
  Month = months,
  Year = years,
  Category = sample(categories, 36, replace = TRUE),
  Sales = runif(36, 1000, 50000),
  Units = sample(50:500, 36, replace = TRUE),
  Revenue = runif(36, 5000, 100000),
  Profit = runif(36, 1000, 30000)
)

# UI
ui <- page_sidebar(
  title = "Monthly Sales Dashboard",
  theme = bs_theme(brand = "_brand.yml"),
  fillable = TRUE, # Make the page fillable to enable scrolling
  
  sidebar = sidebar(
    title = "Controls",
    width = "300px", # Set a fixed width for the sidebar
    
    selectInput("year", "Select Year:",
                choices = unique(sales_data$Year),
                selected = "2023"),
    
    checkboxGroupInput("categories", "Select Categories:",
                       choices = unique(sales_data$Category),
                       selected = unique(sales_data$Category)),
    
    selectInput("metric", "Select Metric:",
                choices = c("Sales", "Units", "Revenue", "Profit"),
                selected = "Sales"),
    
    hr(),
    
    downloadButton("downloadData", "Download Data")
  ),
  
  # Main content area with scrolling
  layout_column_wrap(
    width = "100%",
    height = "auto",
    style = "overflow-y: auto; padding: 10px;",
    
    # Summary metrics row
    layout_column_wrap(
      width = 1/4,  # Each box takes 1/4 of the width
      height = "auto",
      value_box(
        title = "Total Sales",
        value = textOutput("totalSales"),
        showcase = bsicons::bs_icon("currency-dollar"),
        theme = "success",
        full_screen = TRUE,
        height = 120
      ),
      value_box(
        title = "Total Units",
        value = textOutput("totalUnits"),
        showcase = bsicons::bs_icon("box-seam"),
        theme = "info",
        full_screen = TRUE,
        height = 120
      ),
      value_box(
        title = "Total Revenue",
        value = textOutput("totalRevenue"),
        showcase = bsicons::bs_icon("cash-coin"),
        theme = "primary",
        full_screen = TRUE,
        height = 120
      ),
      value_box(
        title = "Total Profit",
        value = textOutput("totalProfit"),
        showcase = bsicons::bs_icon("graph-up-arrow"),
        theme = "warning",
        full_screen = TRUE,
        height = 120
      )
    ),
    
    # Charts row - two columns
    layout_column_wrap(
      width = 1/2,  # Two charts per row
      height = "auto",
      card(
        card_header("Monthly Performance"),
        card_body(
          plotlyOutput("monthlyTrend", height = "350px")
        ),
        full_screen = TRUE,
        height = "auto"
      ),
      card(
        card_header("Category Distribution"),
        card_body(
          plotlyOutput("categoryPie", height = "350px")
        ),
        full_screen = TRUE,
        height = "auto"
      )
    ),
    
    # Category sales chart - full width
    card(
      card_header("Monthly Sales by Category"),
      card_body(
        plotlyOutput("categorySales", height = "350px")
      ),
      full_screen = TRUE,
      height = "auto"
    ),
    
    # Data table - full width
    card(
      card_header("Detailed Sales Data"),
      card_body(
        DTOutput("salesTable")
      ),
      full_screen = TRUE,
      height = "auto"
    )
  )
)

# Server
server <- function(input, output, session) {
  
  # Filter data based on user inputs
  filtered_data <- reactive({
    # Check if inputs exist
    req(input$year)
    req(input$categories)
    
    # Return filtered data
    filtered <- sales_data %>%
      filter(
        Year == input$year,
        Category %in% input$categories
      )
    
    # Ensure we return at least an empty dataframe with the right columns if nothing matches
    if(nrow(filtered) == 0) {
      return(sales_data[0, ])
    }
    
    return(filtered)
  })
  
  # Value Box Outputs
  output$totalSales <- renderText({
    total <- sum(filtered_data()$Sales)
    paste0("$", format(round(total), big.mark = ","))
  })
  
  output$totalUnits <- renderText({
    total <- sum(filtered_data()$Units)
    format(total, big.mark = ",")
  })
  
  output$totalRevenue <- renderText({
    total <- sum(filtered_data()$Revenue)
    paste0("$", format(round(total), big.mark = ","))
  })
  
  output$totalProfit <- renderText({
    total <- sum(filtered_data()$Profit)
    paste0("$", format(round(total), big.mark = ","))
  })
  
  # Monthly Trend Plot
  output$monthlyTrend <- renderPlotly({
    # Check if we have necessary inputs
    req(input$year)
    req(length(input$categories) > 0)
    req(input$metric)
    
    # Define month order
    month_order <- c("Jan", "Feb", "Mar", "Apr", "May", "Jun",
                     "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")
    
    # Get filtered data
    fd <- filtered_data()
    
    # Fallback for empty dataset
    if(nrow(fd) == 0) {
      return(
        plot_ly() %>%
          add_annotations(
            text = "No data available for the selected criteria",
            showarrow = FALSE,
            font = list(size = 16)
          )
      )
    }
    
    # Aggregate data by month
    monthly_data <- fd %>%
      group_by(Month) %>%
      summarize(Metric = sum(!!sym(input$metric)), .groups = "drop") %>%
      mutate(Month = factor(Month, levels = month_order))
    
    # If no data after aggregation, show message
    if(nrow(monthly_data) == 0) {
      return(
        plot_ly() %>%
          add_annotations(
            text = "No data available for the selected criteria",
            showarrow = FALSE,
            font = list(size = 16)
          )
      )
    }
    
    # Calculate range for y-axis to prevent zero_range error
    y_min <- min(monthly_data$Metric) * 0.9  # 10% below min
    y_max <- max(monthly_data$Metric) * 1.1  # 10% above max
    
    # If y_min equals y_max (all values identical), add padding
    if(y_min == y_max) {
      # If value is 0, set range to 0-1, otherwise create a 10% range around the value
      if(y_min == 0) {
        y_min <- 0
        y_max <- 1
      } else {
        y_min <- y_min * 0.9
        y_max <- y_max * 1.1
      }
    }
    
    # Create the plot directly with plotly instead of ggplot
    plot_ly(data = monthly_data, x = ~Month, y = ~Metric, type = 'scatter', 
            mode = 'lines+markers', line = list(color = 'steelblue', width = 2),
            marker = list(color = 'steelblue', size = 10)) %>%
      layout(
        title = paste("Monthly", input$metric),
        xaxis = list(title = "Month"),
        yaxis = list(title = input$metric, range = c(y_min, y_max)),
        hovermode = "x", 
        autosize = TRUE, 
        margin = list(l = 50, r = 50, b = 50, t = 50, pad = 4)
      )
  })
  
  # Category Pie Chart
  output$categoryPie <- renderPlotly({
    # Ensure we have data to plot
    req(nrow(filtered_data()) > 0)
    req(length(input$categories) > 0)
    
    category_data <- filtered_data() %>%
      group_by(Category) %>%
      summarize(Metric = sum(!!sym(input$metric)), .groups = "drop")
    
    # Only create pie chart if we have data
    if(nrow(category_data) > 0) {
      # Ensure we have colors for all categories (handle edge cases)
      n_colors <- min(length(unique(category_data$Category)), 12)  # RColorBrewer Set3 has max 12 colors
      if(n_colors < 3) n_colors <- 3  # RColorBrewer requires at least 3 colors for Set3
      
      plot_ly(category_data, labels = ~Category, values = ~Metric, type = "pie",
              textinfo = "label+percent", hoverinfo = "text",
              text = ~paste(Category, ": ", format(round(Metric), big.mark = ",")),
              marker = list(colors = RColorBrewer::brewer.pal(n_colors, "Set3"))) %>%
        layout(title = paste("Category Distribution by", input$metric),
               autosize = TRUE, margin = list(l = 20, r = 20, b = 20, t = 50, pad = 4))
    } else {
      # Create an empty plot with a message if no data
      plot_ly() %>%
        add_annotations(
          text = "No data available for the selected criteria",
          showarrow = FALSE,
          font = list(size = 16)
        )
    }
  })
  
  # Category Sales Bar Chart
  output$categorySales <- renderPlotly({
    # Check if we have necessary inputs
    req(input$year)
    req(length(input$categories) > 0)
    req(input$metric)
    
    # Define month order
    month_order <- c("Jan", "Feb", "Mar", "Apr", "May", "Jun",
                     "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")
    
    # Get filtered data
    fd <- filtered_data()
    
    # Fallback for empty dataset
    if(nrow(fd) == 0) {
      return(
        plot_ly() %>%
          add_annotations(
            text = "No data available for the selected criteria",
            showarrow = FALSE,
            font = list(size = 16)
          )
      )
    }
    
    # Aggregate data by month and category
    category_sales <- fd %>%
      group_by(Month, Category) %>%
      summarize(Metric = sum(!!sym(input$metric)), .groups = "drop") %>%
      mutate(Month = factor(Month, levels = month_order))
    
    # If no data after aggregation, show message
    if(nrow(category_sales) == 0) {
      return(
        plot_ly() %>%
          add_annotations(
            text = "No data available for the selected criteria",
            showarrow = FALSE,
            font = list(size = 16)
          )
      )
    }
    
    # Calculate total for each month for stacked bar
    month_totals <- category_sales %>%
      group_by(Month) %>%
      summarize(TotalMetric = sum(Metric), .groups = "drop")
    
    # Calculate range for y-axis to prevent zero_range error
    y_min <- 0  # Start at 0 for bar charts
    y_max <- max(month_totals$TotalMetric) * 1.1  # 10% above max
    
    # If y_max is 0 (all values are 0), set a default range
    if(y_max == 0) {
      y_max <- 1
    }
    
    # Create direct plotly stacked bar chart instead of ggplot
    # Get unique categories for consistent colors
    unique_categories <- unique(category_sales$Category)
    
    # Set up a color palette (with a safe number of colors)
    n_colors <- min(length(unique_categories), 12)  # RColorBrewer Set3 has max 12 colors
    if(n_colors < 3) n_colors <- 3  # RColorBrewer requires at least 3 colors for Set3
    colors <- RColorBrewer::brewer.pal(n_colors, "Set3")
    
    # For each category, add a trace to the plot
    p <- plot_ly(type = 'bar')
    
    for(i in seq_along(unique_categories)) {
      cat_data <- category_sales %>% filter(Category == unique_categories[i])
      if(nrow(cat_data) > 0) {
        color_index <- min(i, length(colors))
        p <- p %>% add_trace(
          x = ~Month, 
          y = ~Metric, 
          data = cat_data,
          name = unique_categories[i],
          marker = list(color = colors[color_index])
        )
      }
    }
    
    # Complete the plot with layout
    p %>% layout(
      barmode = 'stack',
      title = paste("Monthly", input$metric, "by Category"),
      xaxis = list(title = "Month", categoryorder = "array", categoryarray = month_order),
      yaxis = list(title = input$metric, range = c(y_min, y_max)),
      hovermode = "closest",
      autosize = TRUE,
      margin = list(l = 50, r = 50, b = 50, t = 50, pad = 4)
    )
  })
  
  # Data Table
  output$salesTable <- renderDT({
    filtered_data() %>%
      select(Month, Category, Sales, Units, Revenue, Profit) %>%
      arrange(Month) %>%
      mutate(
        Sales = paste0("$", format(round(Sales), big.mark = ",")),
        Revenue = paste0("$", format(round(Revenue), big.mark = ",")),
        Profit = paste0("$", format(round(Profit), big.mark = ","))
      )
  }, options = list(pageLength = 10, autoWidth = TRUE, scrollX = TRUE))
  
  # Download handler
  output$downloadData <- downloadHandler(
    filename = function() {
      paste("sales_data_", input$year, ".csv", sep = "")
    },
    content = function(file) {
      write.csv(filtered_data(), file, row.names = FALSE)
    }
  )
}

# Run the application
shinyApp(ui = ui, server = server)