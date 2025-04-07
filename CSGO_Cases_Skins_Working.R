library(shiny)
library(RSelenium)
library(jsonlite)
library(stringr)
library(dplyr)
library(lubridate)
library(ggplot2)
library(tidyr)
library(tidyverse)
library(tidyquant)
library(promises)
library(future)
library(DT)
library(shinydashboard)  # Added for valueBox functionality

# Setup for async operations
plan(multisession)

#' Before running the app, scrape data first
#' 
#' This function stores data in a global variable that will be used by the app
#' 
#' @param use_real_data Logical, whether to scrape real data (TRUE) or use sample data (FALSE)
#' @param cases Character vector of case names to scrape
#' @return None, data is stored in global variables
cases <- c(
  "Chroma Case", "Chroma 2 Case", "Falchion Case"
)
skin_conditions <- c(
  "Factory New",
  "Minimal Wear",
  "Field-Tested",
  "Well-Worn",
  "Battle-Scarred"
)
# Define mapping of cases to their skins
case_to_skins <- list(
  "Chroma Case" = c(
    "AWP | Man-o'-war",
    "Galil AR | Chatterbox",
    "P250 | Muertos",
    "AK-47 | Cartel",
    "M4A4 | 龍王 (Dragon King)",
    "Sawed-Off | Serenity",
    "Desert Eagle | Naga",
    "Dual Berettas | Urban Shock",
    "Glock-18 | Catacombs",
    "M249 | System Lock",
    "MP9 | Deadly Poison",
    "SCAR-20 | Grotto",
    "XM1014 | Quicksilver",
    "MAC-10 | Malachite"
  ),
  "Chroma 2 Case" = c(
    "M4A1-S | Hyper Beast",
    "MAC-10 | Neon Rider",
    "Five-SeveN | Monkey Business",
    "FAMAS | Djinn",
    "Galil AR | Eco",
    "AWP | Worm God",
    "MAG-7 | Heat",
    "UMP-45 | Grand Prix",
    "CZ75-Auto | Pole Position",
    "Negev | Man-o'-war",
    "P250 | Valence",
    "MP7 | Armor Core",
    "Sawed-Off | Origami",
    "Desert Eagle | Bronze Deco",
    "AK-47 | Elite Build"
  ),
  "Falchion Case" = c(
    "AWP | Hyper Beast",
    "AK-47 | Aquamarine Revenge",
    "CZ75-Auto | Yellow Jacket",
    "SG 553 | Cyrex",
    "MP7 | Nemesis",
    "Negev | Loudmouth",
    "P2000 | Handgun",
    "Nova | Ranger",
    "Glock-18 | Bunsen Burner",
    "UMP-45 | Riot",
    "M4A4 | Evil Daimyo",
    "FAMAS | Neural Net",
    "P90 | Elite Build",
    "MP9 | Ruby Poison Dart",
    "MAC-10 | Rangeen",
    "Galil AR | Rocket Pop"
  )
)

# Modified scraping function to handle both cases and skins with conditions
scrape_and_prepare_data <- function(use_real_data = FALSE, cases = c("Chroma Case", "Chroma 2 Case"), 
                                    selected_case = NULL, selected_skin = NULL, selected_condition = NULL) {
  
  # Define scraping function for cases and skins (now with condition)
  scrape_case_chart_data <- function(item_name, remDr, is_skin = FALSE, condition = NULL, wait_time = 6) {
    # Add condition to skin name if provided
    if (is_skin && !is.null(condition)) {
      item_name <- paste0(item_name, " (", condition, ")")
      message(paste("Scraping data for", item_name))
    } else {
      message(paste("Scraping data for", item_name))
    }
    
    # For skins, we need to prepend the weapon type
    url_name <- item_name
    
    encoded_item <- URLencode(url_name)
    url <- paste0("https://steamcommunity.com/market/listings/730/", encoded_item)
    
    remDr$navigate(url)
    Sys.sleep(wait_time)
    
    html <- remDr$getPageSource()[[1]]
    html_clean <- gsub("[\r\n\t]", "", html)
    
    pattern <- "var line1=\\[(\\[.*?\\])\\];"
    matches <- stringr::str_match(html_clean, pattern)
    
    if (is.na(matches[2])) {
      message(paste("❌ Could not extract line1 for:", item_name))
      return(NULL)
    }
    
    json_array <- paste0("[", matches[2], "]")
    
    chart_data <- tryCatch({
      fromJSON(json_array)
    }, error = function(e) {
      message(paste("❌ JSON parse error for", item_name, ":", e$message))
      return(NULL)
    })
    
    num_cols <- ncol(chart_data)
    
    df <- tibble(
      time = chart_data[, 1],
      price_usd = as.numeric(chart_data[, 2]),
      volume = if (num_cols >= 3) as.numeric(chart_data[, 3]) else NA_real_
    ) %>%
      mutate(
        time_clean = str_remove(time, " \\+0$"),
        date = parse_date_time(time_clean, orders = "b d Y H", tz = "UTC"),
        Item = item_name,
        Type = if(is_skin) "Skin" else "Case",
        Condition = if(is_skin && !is.null(condition)) condition else NA_character_
      ) %>%
      select(Item, Type, Condition, date, price_usd, volume) %>%
      filter(!is.na(date))
    
    return(df)
  }
  
  # Get the data - either real or sample
  if (use_real_data) {
    message("Starting RSelenium... this may take a moment")
    rD <- rsDriver(browser = "firefox", chromever = NULL, verbose = FALSE)
    remDr <- rD$client
    
    # Create a list to store data for each item
    all_data_list <- list()
    
    # If specific case, skin, and condition are provided, scrape just those
    if (!is.null(selected_case) && !is.null(selected_skin) && !is.null(selected_condition) && selected_skin != "All") {
      # Scrape case
      case_data <- scrape_case_chart_data(selected_case, remDr, is_skin = FALSE)
      if (!is.null(case_data)) {
        all_data_list[[selected_case]] <- case_data
        message(paste("✓ Successfully scraped data for", selected_case))
      }
      
      # Scrape skin with condition
      skin_data <- scrape_case_chart_data(selected_skin, remDr, is_skin = TRUE, condition = selected_condition)
      if (!is.null(skin_data)) {
        full_skin_name <- paste0(selected_skin, " (", selected_condition, ")")
        all_data_list[[full_skin_name]] <- skin_data
        message(paste("✓ Successfully scraped data for", full_skin_name))
      }
    } 
    # If only case and skin are selected (without specific condition), scrape that combination
    else if (!is.null(selected_case) && !is.null(selected_skin) && selected_skin != "All") {
      # Scrape the case
      case_data <- scrape_case_chart_data(selected_case, remDr, is_skin = FALSE)
      if (!is.null(case_data)) {
        all_data_list[[selected_case]] <- case_data
        message(paste("✓ Successfully scraped data for", selected_case))
      }
      
      # If condition is not specified, use Factory New as default
      condition_to_use <- if(is.null(selected_condition)) "Factory New" else selected_condition
      
      # Scrape skin with default condition
      skin_data <- scrape_case_chart_data(selected_skin, remDr, is_skin = TRUE, condition = condition_to_use)
      if (!is.null(skin_data)) {
        full_skin_name <- paste0(selected_skin, " (", condition_to_use, ")")
        all_data_list[[full_skin_name]] <- skin_data
        message(paste("✓ Successfully scraped data for", full_skin_name))
      }
    }
    # If only case is selected, scrape that case and sample skins
    else if (!is.null(selected_case)) {
      # Scrape the case
      case_data <- scrape_case_chart_data(selected_case, remDr, is_skin = FALSE)
      if (!is.null(case_data)) {
        all_data_list[[selected_case]] <- case_data
        message(paste("✓ Successfully scraped data for", selected_case))
      }
      
      # Scrape some skins for the case with Factory New condition (default)
      for (i in 1:min(3, length(case_to_skins[[selected_case]]))) {
        skin <- case_to_skins[[selected_case]][i]
        skin_data <- scrape_case_chart_data(skin, remDr, is_skin = TRUE, condition = "Factory New")
        if (!is.null(skin_data)) {
          full_skin_name <- paste0(skin, " (Factory New)")
          all_data_list[[full_skin_name]] <- skin_data
          message(paste("✓ Successfully scraped data for", full_skin_name))
        }
        Sys.sleep(2)  # Add a small delay between requests
      }
    }
    # Otherwise, scrape all selected cases
    else {
      for (case_name in cases) {
        case_data <- scrape_case_chart_data(case_name, remDr, is_skin = FALSE)
        if (!is.null(case_data)) {
          all_data_list[[case_name]] <- case_data
          message(paste("✓ Successfully scraped data for", case_name))
        }
        Sys.sleep(2)  # Add a small delay between requests
      }
    }
    
    # Close the Selenium session
    remDr$close()
    rD$server$stop()
    
    # Combine all data
    all_data <- bind_rows(all_data_list)
    
    return(all_data)
  } else {
    # Use sample data if not scraping real data
    # Create synthetic data for demonstration
    set.seed(123)
    
    sample_data <- tibble(
      Item = character(),
      Type = character(),
      Condition = character(),
      date = as.POSIXct(character()),
      price_usd = numeric(),
      volume = numeric()
    )
    
    # Generate sample data for each case
    for (case_name in cases) {
      dates <- seq(as.POSIXct("2020-01-01"), as.POSIXct("2023-04-01"), by = "day")
      
      # Case data
      case_data <- tibble(
        Item = case_name,
        Type = "Case",
        Condition = NA_character_,
        date = dates,
        price_usd = cumsum(rnorm(length(dates), 0, 0.01)) + 
          runif(1, 0.5, 3) + 
          sin(seq(0, 2*pi*3, length.out = length(dates)))*0.2,
        volume = round(abs(rnorm(length(dates), 1000, 300)))
      )
      
      sample_data <- bind_rows(sample_data, case_data)
      
      # Add a few skins for each case, with different conditions
      if (!is.null(case_to_skins[[case_name]])) {
        for (i in 1:min(3, length(case_to_skins[[case_name]]))) {
          skin_name <- case_to_skins[[case_name]][i]
          
          for (condition in skin_conditions) {
            # Price multiplier based on condition (Factory New most expensive)
            condition_multiplier <- switch(condition,
                                           "Factory New" = 1.5,
                                           "Minimal Wear" = 1.2,
                                           "Field-Tested" = 1.0,
                                           "Well-Worn" = 0.8,
                                           "Battle-Scarred" = 0.6)
            
            full_skin_name <- paste0(skin_name, " (", condition, ")")
            
            skin_data <- tibble(
              Item = full_skin_name,
              Type = "Skin",
              Condition = condition,
              date = dates,
              price_usd = (cumsum(rnorm(length(dates), 0, 0.05)) + 
                             runif(1, 5, 50) + 
                             sin(seq(0, 2*pi*2, length.out = length(dates)))*2) * condition_multiplier,
              volume = round(abs(rnorm(length(dates), 500, 150)))
            )
            
            sample_data <- bind_rows(sample_data, skin_data)
          }
        }
      }
    }
    
    # Ensure all values are positive
    sample_data <- sample_data %>%
      mutate(
        price_usd = pmax(0.03, price_usd),
        volume = pmax(1, volume)
      )
    
    return(sample_data)
  }
}

# Define UI
ui <- dashboardPage(
  dashboardHeader(title = "CS:GO Market Analysis"),
  
  dashboardSidebar(
    sidebarMenu(
      checkboxInput("use_real_data", "Use Real Data (Slower)", FALSE),
      selectInput("case_select", "Select Case:", choices = cases),
      uiOutput("skin_select_ui"),
      # Add condition selection UI
      uiOutput("condition_select_ui"),
      actionButton("scrape_btn", "Scrape Data", class = "btn-primary"),
      hr(),
      dateRangeInput("date_range", "Date Range:",
                     start = "2022-01-01",
                     end = Sys.Date()),
      checkboxInput("show_volume", "Show Volume", TRUE),
      checkboxInput("show_ma", "Show Moving Averages", FALSE),
      conditionalPanel(
        condition = "input.show_ma == true",
        sliderInput("ma_periods", "MA Periods:", min = 5, max = 100, value = 30)
      ),
      hr(),
      checkboxInput("compare_mode", "Compare Items", FALSE),
      conditionalPanel(
        condition = "input.compare_mode == true",
        selectInput("compare_case", "Compare with Case:", choices = c("None", cases)),
        uiOutput("compare_skin_ui"),
        # Add compare condition UI
        uiOutput("compare_condition_ui")
      )
    )
  ),
  
  dashboardBody(
    tabBox(
      width = 12,
      tabPanel("Price Chart", 
               plotOutput("price_chart"),
               plotOutput("volume_chart", height = "200px")),
      tabPanel("Price Analysis", 
               fluidRow(
                 valueBox(value = textOutput("max_price"), subtitle = "Maximum Price", width = 3, color = "blue"),
                 valueBox(value = textOutput("min_price"), subtitle = "Minimum Price", width = 3, color = "red"),
                 valueBox(value = textOutput("avg_price"), subtitle = "Average Price", width = 3, color = "purple"),
                 valueBox(value = textOutput("price_change"), subtitle = "Price Change", width = 3, color = "green")
               ),
               plotOutput("price_distribution")),
      tabPanel("Data Table", DTOutput("data_table")),
      tabPanel("Condition Comparison", 
               checkboxInput("show_all_conditions", "Show All Conditions for Selected Skin", TRUE),
               plotOutput("condition_comparison"))
    )
  )
)

# Define server
server <- function(input, output, session) {
  
  # Store the data
  market_data <- reactiveVal(NULL)
  
  # Update skin selection based on case
  output$skin_select_ui <- renderUI({
    req(input$case_select)
    skins <- case_to_skins[[input$case_select]]
    selectInput("skin_select", "Select Skin:", choices = c("All", skins))
  })
  
  # Add condition selection UI
  output$condition_select_ui <- renderUI({
    req(input$skin_select)
    if(input$skin_select == "All") {
      return(NULL)
    } else {
      selectInput("condition_select", "Select Condition:", choices = skin_conditions)
    }
  })
  
  # Update comparison skin selection
  output$compare_skin_ui <- renderUI({
    req(input$compare_case)
    if(input$compare_case == "None") {
      return(NULL)
    }
    skins <- case_to_skins[[input$compare_case]]
    selectInput("compare_skin", "Compare with Skin:", choices = c("None", skins))
  })
  
  # Add comparison condition selection UI
  output$compare_condition_ui <- renderUI({
    req(input$compare_skin, input$compare_case)
    if(input$compare_skin == "None") {
      return(NULL)
    }
    selectInput("compare_condition", "Compare Condition:", choices = skin_conditions)
  })
  
  # Scrape data when button is clicked
  observeEvent(input$scrape_btn, {
    withProgress(message = 'Scraping data...', value = 0, {
      # Capture the current values of inputs BEFORE passing to future_promise
      use_real_data_val <- input$use_real_data
      selected_case <- input$case_select
      selected_skin <- if(input$skin_select == "All") NULL else input$skin_select
      selected_condition <- if(!is.null(selected_skin) && selected_skin != "All") input$condition_select else NULL
      
      # Scrape data asynchronously with the captured values
      future_promise({
        scrape_and_prepare_data(
          use_real_data = use_real_data_val,
          cases = cases,
          selected_case = selected_case,
          selected_skin = selected_skin,
          selected_condition = selected_condition
        )
      }) %...>% 
        (function(data) {
          market_data(data)
          showNotification("Data scraping completed!", type = "message")
        }) %...!% 
        (function(error) {
          showNotification(paste("Error:", error$message), type = "error")
        })
    })
  })
  
  # Filter data based on UI selections
  filtered_data <- reactive({
    req(market_data())
    
    data <- market_data() %>%
      filter(date >= input$date_range[1], date <= input$date_range[2])
    
    # Filter for primary selection
    if (input$skin_select != "All") {
      # Create the full skin name with condition
      full_skin_name <- paste0(input$skin_select, " (", input$condition_select, ")")
      primary_items <- c(input$case_select, full_skin_name)
    } else {
      # Select the case and all its skins
      primary_items <- c(input$case_select)
      # Add all skins from the case if "All" is selected
      for (skin in case_to_skins[[input$case_select]]) {
        for (condition in skin_conditions) {
          full_skin_name <- paste0(skin, " (", condition, ")")
          if (full_skin_name %in% data$Item) {
            primary_items <- c(primary_items, full_skin_name)
          }
        }
      }
    }
    
    # Add comparison items if in compare mode
    if (input$compare_mode && input$compare_case != "None") {
      comparison_items <- c(input$compare_case)
      if (!is.null(input$compare_skin) && input$compare_skin != "None") {
        full_compare_skin <- paste0(input$compare_skin, " (", input$compare_condition, ")")
        comparison_items <- c(comparison_items, full_compare_skin)
      }
      selected_items <- c(primary_items, comparison_items)
    } else {
      selected_items <- primary_items
    }
    
    data %>% filter(Item %in% selected_items)
  })
  
  # Special filtered data for condition comparison
  condition_comparison_data <- reactive({
    req(market_data(), input$skin_select)
    req(input$skin_select != "All")
    
    data <- market_data() %>%
      filter(date >= input$date_range[1], date <= input$date_range[2])
    
    # Get all conditions for the selected skin
    base_skin_pattern <- paste0("^", input$skin_select, " \\(")
    data %>% 
      filter(grepl(base_skin_pattern, Item)) %>%
      mutate(Condition = stringr::str_extract(Item, "\\([^)]+\\)") %>% 
               stringr::str_remove_all("\\(|\\)"))
  })
  
  # Price chart
  output$price_chart <- renderPlot({
    req(filtered_data())
    
    data <- filtered_data()
    
    # Add moving averages if requested
    if (input$show_ma) {
      data <- data %>%
        group_by(Item) %>%
        arrange(date) %>%
        mutate(
          MA_price = rollmean(price_usd, k = input$ma_periods, fill = NA, align = "right")
        ) %>%
        ungroup()
    }
    
    p <- ggplot(data, aes(x = date, y = price_usd, color = Item)) +
      geom_line() +
      theme_minimal() +
      labs(
        title = "Price History",
        x = "Date",
        y = "Price (USD)",
        color = "Item"
      )
    
    if (input$show_ma) {
      p <- p + 
        geom_line(aes(y = MA_price, linetype = "Moving Average"), size = 1) +
        scale_linetype_manual(values = "dashed", name = "")
    }
    
    p
  })
  
  # Volume chart
  output$volume_chart <- renderPlot({
    req(filtered_data())
    req(input$show_volume)
    
    filtered_data() %>%
      ggplot(aes(x = date, y = volume, color = Item)) +
      geom_line(alpha = 0.7) +
      theme_minimal() +
      theme(legend.position = "none") +
      labs(
        title = "Volume History",
        x = "Date",
        y = "Volume"
      )
  })
  
  # Price distribution chart
  output$price_distribution <- renderPlot({
    req(filtered_data())
    
    filtered_data() %>%
      ggplot(aes(x = price_usd, fill = Item)) +
      geom_density(alpha = 0.5) +
      theme_minimal() +
      labs(
        title = "Price Distribution",
        x = "Price (USD)",
        y = "Density",
        fill = "Item"
      )
  })
  
  # Condition comparison chart
  output$condition_comparison <- renderPlot({
    req(condition_comparison_data())
    req(input$skin_select != "All")
    
    if(input$show_all_conditions) {
      condition_comparison_data() %>%
        ggplot(aes(x = date, y = price_usd, color = Condition)) +
        geom_line() +
        theme_minimal() +
        labs(
          title = paste("Price Comparison by Condition:", input$skin_select),
          x = "Date",
          y = "Price (USD)",
          color = "Condition"
        )
    } else {
      # Show just the selected condition if not showing all
      condition_comparison_data() %>%
        filter(Condition == input$condition_select) %>%
        ggplot(aes(x = date, y = price_usd)) +
        geom_line(color = "blue") +
        theme_minimal() +
        labs(
          title = paste("Price for", input$skin_select, "(", input$condition_select, ")"),
          x = "Date",
          y = "Price (USD)"
        )
    }
  })
  
  # Data table
  output$data_table <- renderDT({
    req(filtered_data())
    
    filtered_data() %>%
      arrange(desc(date)) %>%
      mutate(
        date = format(date, "%Y-%m-%d"),
        price_usd = round(price_usd, 2)
      ) %>%
      datatable(options = list(pageLength = 15))
  })
  
  # Value box outputs
  output$max_price <- renderText({
    req(filtered_data())
    paste0("$", round(max(filtered_data()$price_usd, na.rm = TRUE), 2))
  })
  
  output$min_price <- renderText({
    req(filtered_data())
    paste0("$", round(min(filtered_data()$price_usd, na.rm = TRUE), 2))
  })
  
  output$avg_price <- renderText({
    req(filtered_data())
    paste0("$", round(mean(filtered_data()$price_usd, na.rm = TRUE), 2))
  })
  
  output$price_change <- renderText({
    req(filtered_data())
    data <- filtered_data() %>%
      arrange(date)
    
    first_price <- data$price_usd[1]
    last_price <- data$price_usd[nrow(data)]
    
    change_pct <- (last_price - first_price) / first_price * 100
    
    paste0(ifelse(change_pct >= 0, "+", ""), round(change_pct, 2), "%")
  })
}

# Run the application
shinyApp(ui = ui, server = server)