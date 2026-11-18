# --- 1. Load Required R Packages ---
# Make sure these are installed: install.packages(c("shiny", "shinydashboard", "ggplot2", "plotly", "dplyr", "lubridate", "scales", "tidyr", "readr", "DT"))
library(shiny)
library(shinydashboard)
library(ggplot2)
library(plotly)      # For interactive plots
library(dplyr)
library(lubridate)   # For date handling
library(scales)      # For number formatting (e.g., comma separated)
library(tidyr)       # For data manipulation like pivot_longer
library(readr)       # For robust CSV reading
library(DT)          # For interactive tables (useful for evaluation metrics)

# --- Define Country Name Mapping ---
# IMPORTANT: Adjust this map based on the 3-letter codes used in your Python output CSVs
# and the full country names you want to display.
country_name_map <- c(
  "VNM" = "Vietnam",
  "SGP" = "Singapore",
  "THA" = "Thailand",
  "KOR" = "South Korea",
  "PHL" = "Philippines",
  "IND" = "India",
  "IDN" = "Indonesia",
  "CHN" = "China",
  "BRN" = "Brunei Darussalam",
  "BGD" = "Bangladesh"
)

# --- Function to apply mapping ---
map_country_names <- function(country_codes) {
  mapped_names <- country_name_map[country_codes]
  # Handle cases where a code might not be in the map, by keeping the original code
  ifelse(is.na(mapped_names), country_codes, mapped_names)
}


# --- 2. Load Pre-computed Data from Python Outputs (Global) ---
# Ensure these CSV files are in the same directory as your app.R
# Using global variables for data loaded once at startup
df_monthly_historical <- NULL
df_forecast_xgb <- NULL
df_forecast_rf <- NULL
df_eval_metrics <- NULL
df_feat_imp_xgb <- NULL
df_feat_imp_rf <- NULL
df_gender_predictions <- NULL
df_raw_edited <- NULL
all_countries_available <- NULL # This will now store full names
country_codes_for_files <- NULL # This will still be used for file names if needed

tryCatch({
  # Define common date formats for robust parsing
  date_formats <- c("Y-m-d", "m/d/Y", "d/m/Y", "b-Y", "ymd", "Ymd", "Y-m") # Added "Y-m"
  
  # Load historical monthly data
  df_monthly_historical <- read_csv("monthly_data.csv", show_col_types = FALSE) %>%
    mutate(month_year = lubridate::parse_date_time(month_year, orders = date_formats),
           country = map_country_names(country)) # Apply mapping here
  
  # --- DIAGNOSTIC: Print column names to R console ---
  print("Columns in df_monthly_historical after loading monthly_data.csv:")
  print(colnames(df_monthly_historical))
  print("Head of df_monthly_historical (with mapped names):")
  print(head(df_monthly_historical))
  print("Tail of df_monthly_historical (with mapped names):")
  print(tail(df_monthly_historical))
  # --- END DIAGNOSTIC ---
  
  # Validate historical data
  if (is.null(df_monthly_historical) || nrow(df_monthly_historical) == 0) {
    stop("monthly_data.csv is empty or could not be read.")
  }
  if (!"country" %in% colnames(df_monthly_historical)) {
    stop("monthly_data.csv is missing the 'country' column.")
  }
  if (length(unique(df_monthly_historical$country)) == 0) {
    stop("No unique countries found in 'country' column of monthly_data.csv.")
  }
  
  # Get unique countries (now full names) from historical data for dropdowns
  all_countries_available <- unique(df_monthly_historical$country)
  # This list is used for the dropdown, so it should be full names
  # We'll still need the original codes or a way to derive them for file lookups
  # The 'country_codes_for_files' logic might need review if Python saves full names in filenames
  
  # Load other global dataframes (these will cause a stop if they fail)
  df_forecast_xgb <- read_csv("future_forecast_xgb.csv", show_col_types = FALSE) %>%
    mutate(month_year = lubridate::parse_date_time(month_year, orders = date_formats))
  # --- DIAGNOSTIC: Check XGBoost forecast data ---
  print("Head of df_forecast_xgb:")
  print(head(df_forecast_xgb))
  # --- END DIAGNOSTIC ---
  
  df_forecast_rf <- read_csv("future_forecast_rf.csv", show_col_types = FALSE) %>%
    mutate(month_year = lubridate::parse_date_time(month_year, orders = date_formats))
  # --- DIAGNOSTIC: Check Random Forest forecast data ---
  print("Head of df_forecast_rf:")
  print(head(df_forecast_rf))
  # --- END DIAGNOSTIC ---
  
  df_eval_metrics <- read_csv("country_evaluation_metrics.csv", show_col_types = FALSE) %>%
    rename(Country = `...1`) %>% # Fix potential column name from default R export
    mutate(Country = map_country_names(Country)) # Apply mapping here
  
  df_feat_imp_xgb <- read_csv("xgb_feature_importance.csv", show_col_types = FALSE)
  df_feat_imp_rf <- read_csv("rf_feature_importance.csv", show_col_types = FALSE)
  
  df_gender_predictions <- read_csv("gender_predictions.csv", show_col_types = FALSE) %>%
    mutate(month_year = lubridate::parse_date_time(month_year, orders = date_formats))
  
  df_raw_edited <- read_csv("df_edited.csv", show_col_types = FALSE) %>%
    mutate(month_year = lubridate::parse_date_time(month_year, orders = date_formats)) # Assuming this df_edited has country column, if not, might need mapping as well.
  
}, error = function(e) {
  stop(paste("Error loading data files. Please ensure all required CSVs are in the app directory and correctly formatted:", e$message))
})


# --- 3. User Interface (UI) Definition ---
ui <- dashboardPage(
  skin = "blue", # Theme for the dashboard
  
  dashboardHeader(title = span("🌏 Teroka Trend Kita Dashboard", style = "font-weight:bold; font-size: 20px;")),
  
  dashboardSidebar(
    sidebarMenu(
      id = "sb_menu", # Assign an ID to the sidebarMenu for conditionalPanel
      menuItem("📈 Overall Forecast", tabName = "overall_forecast", icon = icon("chart-line")),
      menuItem("📍 Country Forecasts", tabName = "country_forecasts", icon = icon("globe-americas")),
      menuItem("📊 Model Performance", tabName = "model_performance", icon = icon("tachometer-alt")),
      menuItem("👥 Gender Trends", tabName = "gender_trends", icon = icon("venus-mars"))
    ),
    # Common inputs accessible across tabs if needed, or specific to each tab
    conditionalPanel(
      condition = "input.sb_menu == 'overall_forecast' || input.sb_menu == 'model_performance' || input.sb_menu == 'gender_trends'",
      radioButtons("selected_model", "🔮 Choose Forecast Model:",
                   choices = c("XGBoost", "Random Forest"), selected = "XGBoost")
    ),
    conditionalPanel(
      condition = "input.sb_menu == 'country_forecasts' || input.sb_menu == 'model_performance'",
      # Ensure all_countries_available is not NULL before using in choices
      uiOutput("country_select_ui")
    )
  ),
  
  dashboardBody(
    # Custom CSS for better aesthetics
    tags$head(
      tags$style(HTML("
        .content-wrapper { background-color: #f9f9f9; }
        .box-header .box-title { font-weight: bold; font-size: 18px; color: #34495E; }
        .box { border-radius: 8px; box-shadow: 0 4px 8px rgba(0,0,0,0.1); margin-bottom: 20px; }
        .small-box.bg-aqua { background-color: #3498DB !important; color: #fff !important; }
        .small-box.bg-green { background-color: #2ECC71 !important; color = #fff !important; }
        .small-box.bg-yellow { background-color: #F1C40F !important; color = #fff !important; }
        .small-box.bg-red { background-color: #E74C3C !important; color = #fff !important; }
        .form-group.shiny-input-container label { font-weight: bold; }
        .nav-tabs-custom > .nav-tabs > li.active { border-top-color: #3C8DBC; }
        /* Style for warning messages */
        .alert-warning-custom {
            color: #856404;
            background-color: #fff3cd;
            border-color: #ffeeba;
            padding: 10px;
            margin-bottom: 15px;
            border: 1px solid transparent;
            border-radius: .25rem;
        }
      "))
    ),
    
    tabItems(
      # --- Tab 1: Overall Forecast ---
      tabItem(tabName = "overall_forecast",
              h2("📈 Overall Tourist Arrivals Forecast (2022-2027)", style = "color: #34495E; font-weight: bold;"),
              fluidRow(
                box(
                  title = "Historical & Future Arrivals",
                  width = 12, solidHeader = TRUE, status = "primary",
                  plotlyOutput("overallForecastPlot", height = "500px")
                )
              )
      ),
      
      # --- Tab 2: Country-Specific Forecasts ---
      tabItem(tabName = "country_forecasts",
              h2("📍 Country-Specific Tourist Arrivals Forecasts (Top 10)", style = "color: #34495E; font-weight: bold;"),
              fluidRow(
                box(
                  title = "Top Countries by Total Predicted Arrivals (2025-2027)", # Updated title to reflect period
                  width = 12, solidHeader = TRUE, status = "primary",
                  plotOutput("topCountriesBarPlot", height = "400px")
                )
              ),
              fluidRow(
                box(
                  title = "Individual Country Forecast", # No longer plural as it will show only one
                  width = 12, solidHeader = TRUE, status = "info",
                  # This will now be a single plot based on dropdown selection
                  uiOutput("countryForecastPlotsUI")
                )
              )
      ),
      
      # --- Tab 3: Model Performance ---
      tabItem(tabName = "model_performance",
              h2("📊 Model Performance & Insights", style = "color: #34495E; font-weight: bold;"),
              fluidRow(
                box(
                  title = "Model Accuracy (R² Percentage)",
                  width = 12, solidHeader = TRUE, status = "success",
                  div(class = "alert-warning-custom",
                      "R-squared (R²) indicates how well the model predicts new data. A higher percentage is better."
                  ),
                  DTOutput("accuracyTableDT") # Using DT for interactive table
                )
              ),
              fluidRow(
                box(
                  title = "Feature Importance",
                  width = 12, solidHeader = TRUE, status = "warning",
                  div(class = "alert-warning-custom",
                      "Feature importance shows which input variables ('month_index', 'month', 'year', etc.) were most influential in the model's predictions."
                  ),
                  plotOutput("featureImportancePlot", height = "400px")
                )
              )
      ),
      
      # --- Tab 4: Gender Trends ---
      tabItem(tabName = "gender_trends",
              h2("👥 Predicted Dominant Gender & Historical Ratios", style = "color: #34495E; font-weight: bold;"),
              fluidRow(
                box(
                  title = "Predicted Dominant Gender (2025-2027)",
                  width = 12, solidHeader = TRUE, status = "primary",
                  plotlyOutput("predictedGenderPlot", height = "400px")
                )
              ),
              fluidRow(
                box(
                  title = "Historical Monthly Ratio of Male Tourist Arrivals (2022-2024)",
                  width = 6, solidHeader = TRUE, status = "info",
                  plotlyOutput("maleRatioPlot", height = "300px")
                ),
                box(
                  title = "Historical Monthly Ratio of Female Tourist Arrivals (2022-2024)",
                  width = 6, solidHeader = TRUE, status = "info",
                  plotlyOutput("femaleRatioPlot", height = "300px")
                )
              )
      ) # CLOSING THE TABITEM HERE
    ) # End tabItems
  ) # End dashboardBody
) # End dashboardPage


# --- 4. Server Logic ---
server <- function(input, output, session) {
  
  # UI for country select, populated only if all_countries_available is not NULL
  output$country_select_ui <- renderUI({
    # Ensure top_10_countries is available before creating the selectInput
    req(top_10_countries())
    
    if (length(top_10_countries()) > 0) {
      selectInput("selected_country_report", "🌍 Select Country for Report:",
                  choices = top_10_countries(), selected = top_10_countries()[1])
    } else {
      # Display an error message if no countries are available
      HTML("<div class='alert-warning-custom'>No top countries available from prediction data. Please check 'predictions_COUNTRY.csv' files.</div>")
    }
  })
  
  # Reactive to get all country predictions data from the individual CSVs
  all_country_predictions_data <- reactive({
    # req(all_countries_available) is implicitly handled by the UI, but it's good to re-validate here
    validate(
      need(!is.null(all_countries_available) && length(all_countries_available) > 0,
           "Initialization error: No countries available from historical data. Cannot load country predictions.")
    )
    
    data_list <- list()
    for (country_full_name_or_code in all_countries_available) { # This will iterate through full names
      # We need to map the full name back to the code for the filename, or keep the original codes list.
      # Assuming original Python output filenames use the 3-letter codes.
      # Finding the code from the full name if it's in the map, otherwise use the name directly
      original_code_for_file <- names(country_name_map)[country_name_map == country_full_name_or_code]
      
      # If the country is not in our map (e.g., if it was originally a full name that wasn't mapped)
      # or if map had duplicates, fallback to using the name directly (after sanitizing for filename)
      if (length(original_code_for_file) == 0 || is.null(original_code_for_file)) {
        file_country_name <- gsub(" ", "_", country_full_name_or_code) # Use full name sanitized for filename
      } else {
        file_country_name <- gsub(" ", "_", original_code_for_file[1]) # Use the first found code
      }
      
      filepath <- file.path(getwd(), paste0("predictions_", file_country_name, ".csv"))
      
      if (file.exists(filepath)) {
        df_country <- tryCatch({
          read_csv(filepath, show_col_types = FALSE)
        }, error = function(e) {
          message(paste("Error reading prediction file for", country_full_name_or_code, ":", e$message))
          NULL # Return NULL if read_csv fails
        })
        
        # Validate and convert columns in the loaded country file
        if (!is.null(df_country) && "month_year" %in% names(df_country) && "predicted_arrivals" %in% names(df_country)) {
          df_country <- df_country %>%
            mutate(month_year = lubridate::parse_date_time(month_year, orders = date_formats), # Use robust parser
                   country = country_full_name_or_code) # Store the full name here
          data_list[[country_full_name_or_code]] <- df_country
        } else if (!is.null(df_country)) { # File read, but columns are wrong
          message(paste("Prediction file for", country_full_name_or_code, "is missing 'month_year' or 'predicted_arrivals' columns, or they are malformed."))
        }
      } else {
        message(paste("Prediction file not found for:", country_full_name_or_code, "at", filepath))
      }
    }
    # Combine all country predictions into a single dataframe
    combined_df <- bind_rows(data_list)
    
    validate(
      need(nrow(combined_df) > 0, "No country-specific prediction data loaded. Please ensure 'predictions_COUNTRY.csv' files exist and are correctly formatted.")
    )
    
    combined_df
  })
  
  # Reactive to calculate total predicted arrivals per country (for bar plot and top countries list)
  df_total_predicted_arrivals <- reactive({
    req(all_country_predictions_data()) # This will ensure combined_df is not empty
    all_country_predictions_data() %>%
      group_by(country) %>%
      summarise(total_predicted_arrivals = sum(predicted_arrivals, na.rm = TRUE), .groups = "drop") %>%
      arrange(desc(total_predicted_arrivals))
  })
  
  # Reactive to get top 10 countries based on total predicted arrivals
  top_10_countries <- reactive({
    req(df_total_predicted_arrivals()) # Ensure data for total predicted arrivals is available
    
    countries_list <- df_total_predicted_arrivals() %>%
      head(10) %>%
      pull(country)
    
    validate(
      need(length(countries_list) > 0, "No top countries identified from prediction data. Check 'predictions_COUNTRY.csv' files and their contents.")
    )
    countries_list
  })
  
  
  # --- Overall Forecast Tab ---
  output$overallForecastPlot <- renderPlotly({
    req(df_monthly_historical, df_forecast_xgb, df_forecast_rf)
    
    # --- AGGREGATE HISTORICAL DATA FOR OVERALL PLOT ---
    # Sum arrivals by month_year from df_monthly_historical (which now has country column)
    df_historical_overall <- df_monthly_historical %>%
      group_by(month_year) %>%
      summarise(arrivals = sum(arrivals, na.rm = TRUE), .groups = "drop")
    # --- END AGGREGATION ---
    
    # Select the appropriate forecast dataframe based on user input
    df_forecast_selected <- if (input$selected_model == "XGBoost") {
      df_forecast_xgb %>% rename(predicted_arrivals = predicted_arrivals_xgb)
    } else {
      df_forecast_rf %>% rename(predicted_arrivals = predicted_arrivals_rf)
    }
    
    # Combine the aggregated historical data and the forecast data for plotting
    combined_data <- bind_rows(
      df_historical_overall %>% mutate(type = "Historical"), # Use the newly aggregated historical data
      df_forecast_selected %>% select(month_year, arrivals = predicted_arrivals) %>% mutate(type = "Forecast")
    )
    
    # --- DIAGNOSTIC: Print combined_data head/tail before plotting ---
    print("Head of combined_data for Overall Forecast Plot:")
    print(head(combined_data))
    print("Tail of combined_data for Overall Forecast Plot:")
    print(tail(combined_data))
    # --- END DIAGNOSTIC ---
    
    # Plotting with ggplot
    p <- ggplot(combined_data, aes(x = month_year, y = arrivals, color = type, group = 1)) +
      geom_line(aes(linetype = type), size = 1) +
      geom_point(data = filter(combined_data, type == "Historical"), size = 1.5, alpha = 0.7) + # Only show points for historical
      scale_color_manual(values = c("Historical" = "#3498DB", "Forecast" = "#E74C3C")) +
      scale_linetype_manual(values = c("Historical" = "solid", "Forecast" = "dashed")) +
      labs(title = paste("Overall Tourist Arrivals: Historical vs.", input$selected_model, "Forecast"),
           x = "Date", y = "Number of Arrivals", color = "Type", linetype = "Type") +
      scale_y_continuous(labels = scales::comma) + # Format y-axis numbers
      theme_minimal() +
      theme(legend.position = "bottom",
            plot.title = element_text(hjust = 0.5, face = "bold"))
    
    # Convert to plotly for interactivity
    ggplotly(p) %>%
      layout(hovermode = "x unified") # Nice hover effect
  })
  
  
  # --- Country-Specific Forecasts Tab ---
  
  output$topCountriesBarPlot <- renderPlot({
    req(df_total_predicted_arrivals()) # Ensure data for total predicted arrivals is available
    
    total_predicted_by_country <- df_total_predicted_arrivals() %>%
      filter(country %in% top_10_countries()) # Filter to only top 10 (ensures data is present)
    
    validate(
      need(nrow(total_predicted_by_country) > 0, "No country prediction data available to plot top countries. Check Python output files.")
    )
    
    # Identify the most contributing country for highlighting
    most_contributing_country_name <- total_predicted_by_country$country[1]
    
    # Create the bar chart
    ggplot(total_predicted_by_country, aes(x = reorder(country, total_predicted_arrivals),
                                           y = total_predicted_arrivals,
                                           fill = country == most_contributing_country_name)) +
      geom_col(show.legend = FALSE) +
      scale_fill_manual(values = c("TRUE" = "salmon", "FALSE" = "skyblue")) + # Highlight top country
      coord_flip() + # Horizontal bars
      labs(title = paste("Top Countries by Predicted Total Arrivals (2025-2027) - using", input$selected_model),
           x = "Country", y = "Predicted Total Arrivals") +
      scale_y_continuous(labels = scales::comma) + # Format y-axis numbers
      theme_minimal() +
      theme(plot.title = element_text(hjust = 0.5, face = "bold"),
            axis.text.y = element_text(face = "bold")) # Bold country names
    
  })
  
  # Render individual country plots
  output$countryForecastPlotsUI <- renderUI({
    # Dependency on input$selected_country_report and other data sources
    req(all_country_predictions_data(), df_monthly_historical, input$selected_country_report)
    
    # Ensure the selected country is valid and available in the top 10 list
    selected_country_name <- input$selected_country_report
    
    validate(
      need(selected_country_name %in% top_10_countries(),
           paste0("Selected country '", selected_country_name, "' is not among the top 10 or has no prediction data."))
    )
    
    # Get historical data for this specific country
    historical_data_country <- df_monthly_historical %>%
      filter(country == selected_country_name) %>%
      select(month_year, arrivals) %>%
      mutate(type = "Historical")
    
    # Get predicted data for this specific country
    predicted_data_country <- all_country_predictions_data() %>%
      filter(country == selected_country_name) %>%
      select(month_year, predicted_arrivals) %>%
      mutate(arrivals = predicted_arrivals, type = "Forecast")
    
    # Combine for plotting
    combined_plot_data <- bind_rows(historical_data_country, predicted_data_country) %>%
      arrange(month_year) # Ensure chronological order
    
    # Define plot name for dynamic output
    plot_name <- paste0("plot_", gsub(" ", "_", selected_country_name))
    
    # Generate the plot for the selected country
    output[[plot_name]] <- renderPlotly({
      # Validate for actual data within the country
      validate(
        need(nrow(combined_plot_data) > 0,
             paste0("No combined historical and prediction data available for ", selected_country_name, "."))
      )
      p <- ggplot(combined_plot_data, aes(x = month_year, y = arrivals, color = type, group = 1)) +
        geom_line(aes(linetype = type), size = 1) +
        geom_point(data = filter(combined_plot_data, type == "Historical"), size = 1.5, alpha = 0.7) +
        scale_color_manual(values = c("Historical" = "#3498DB", "Forecast" = "#E74C3C")) +
        scale_linetype_manual(values = c("Historical" = "solid", "Forecast" = "dashed")) +
        labs(title = paste("Forecast for", selected_country_name),
             x = "Date", y = "Arrivals", color = "Type", linetype = "Type") +
        scale_y_continuous(labels = scales::comma) +
        theme_minimal() +
        theme(legend.position = "none", # Hide legend for individual plots
              plot.title = element_text(hjust = 0.5, face = "bold", size = 12),
              axis.text.x = element_text(angle = 45, hjust = 1, size = 8),
              axis.title = element_text(size = 9))
      
      ggplotly(p) %>% layout(hovermode = "x unified")
    })
    
    # Return the plotlyOutput element for placement in UI
    # This will now just return ONE plot for the selected country
    column(12, # Occupy full width since only one plot is shown
           box(
             title = paste("Forecast for", selected_country_name), # Dynamic title for the box
             width = 12, # Box occupies full column width
             plotlyOutput(plot_name, height = "400px") # Increased height for single plot
           )
    )
  })
  
  
  # --- Model Performance Tab ---
  
  output$accuracyTableDT <- renderDT({
    req(df_eval_metrics, input$selected_country_report)
    
    # Filter to show only the selected country's accuracy data
    accuracy_data_display <- df_eval_metrics %>%
      filter(Country == input$selected_country_report) %>%
      mutate(`R-squared (%)` = round(`R-squared` * 100, 2)) %>%
      select(Country, `R-squared (%)`)
    
    validate(
      need(nrow(accuracy_data_display) > 0, paste0("No accuracy data available for '", input$selected_country_report, "'. Check 'country_evaluation_metrics.csv'."))
    )
    
    # Render as interactive DT table
    datatable(
      accuracy_data_display,
      options = list(pageLength = 1, dom = 't'), # Show only 1 row, hide pagination/search
      rownames = FALSE,
      caption = paste("Model Accuracy (R² %) for", input$selected_country_report, "(based on Random Forest).")
    )
  })
  
  
  output$featureImportancePlot <- renderPlot({
    req(df_feat_imp_xgb, df_feat_imp_rf, input$selected_model)
    
    df_selected_feat_imp <- if (input$selected_model == "XGBoost") {
      df_feat_imp_xgb
    } else {
      df_feat_imp_rf
    }
    
    # Ensure the dataframe has 'feature' and 'importance_score' columns
    validate(
      need(all(c("feature", "importance_score") %in% names(df_selected_feat_imp)),
           "Feature importance data missing 'feature' or 'importance_score' column.")
    )
    
    # For XGBoost, the importance_score column is 'Gain' in the output from xgb.importance
    # In Python, you store it as 'importance_score' in the CSV.
    # The Python code for RF also stores it as 'importance_score'.
    # So, 'importance_score' should be consistent.
    
    ggplot(df_selected_feat_imp, aes(x = reorder(feature, importance_score), y = importance_score)) +
      geom_bar(stat = "identity", fill = "#E67E22") + # Orange-ish color
      coord_flip() +
      labs(title = paste("Feature Importance (", input$selected_model, ")", sep = ""),
           x = "Feature", y = "Importance Score") +
      theme_minimal() +
      theme(plot.title = element_text(hjust = 0.5, face = "bold"),
            axis.text.y = element_text(face = "bold"))
  })
  
  
  # --- Gender Trends Tab ---
  
  output$predictedGenderPlot <- renderPlotly({
    req(df_gender_predictions)
    
    # Convert predicted_dominant_gender to a factor for ordered plotting if desired
    # or just use it as is for discrete Y-axis
    df_gender_predictions_plot <- df_gender_predictions %>%
      mutate(predicted_dominant_gender = factor(predicted_dominant_gender, levels = c("Female", "Male")))
    
    p <- ggplot(df_gender_predictions_plot, aes(x = month_year, y = predicted_dominant_gender, color = predicted_dominant_gender)) +
      geom_point(size = 3) +
      geom_line(aes(group = 1), alpha = 0.5) + # Add a subtle line to connect points over time
      scale_color_manual(values = c("Male" = "#3498DB", "Female" = "#E74C3C")) + # Blue for Male, Red for Female
      labs(title = "Predicted Dominant Gender for Tourist Arrivals (2025-2027)",
           x = "Month", y = "Dominant Gender", color = "Gender") +
      theme_minimal() +
      theme(legend.position = "bottom",
            plot.title = element_text(hjust = 0.5, face = "bold"),
            axis.text.x = element_text(angle = 45, hjust = 1))
    
    ggplotly(p) %>%
      layout(hovermode = "x unified")
  })
  
  
  output$maleRatioPlot <- renderPlotly({
    req(df_raw_edited)
    df_gender_trend <- df_raw_edited %>%
      group_by(month_year) %>%
      summarise(
        arrivals_male = sum(arrivals_male, na.rm = TRUE),
        arrivals_female = sum(arrivals_female, na.rm = TRUE),
        .groups = "drop"
      ) %>%
      mutate(
        total_gender_arrivals = arrivals_male + arrivals_female,
        male_ratio = ifelse(total_gender_arrivals > 0, arrivals_male / total_gender_arrivals, 0)
      )
    
    p <- ggplot(df_gender_trend, aes(x = month_year, y = male_ratio)) +
      geom_line(color = "#3498DB", size = 1) +
      geom_point(color = "#3498DB", size = 1.5, alpha = 0.7) +
      labs(title = "Historical Monthly Ratio of Male Tourist Arrivals (2022-2024)",
           x = "Month", y = "Proportion Male") +
      scale_y_continuous(labels = scales::percent) + # Format as percentage
      theme_minimal() +
      theme(plot.title = element_text(hjust = 0.5, face = "bold", size = 12),
            axis.text.x = element_text(angle = 45, hjust = 1, size = 8),
            axis.title = element_text(size = 9))
    
    ggplotly(p)
  })
  
  
  output$femaleRatioPlot <- renderPlotly({
    req(df_raw_edited)
    df_gender_trend <- df_raw_edited %>%
      group_by(month_year) %>%
      summarise(
        arrivals_male = sum(arrivals_male, na.rm = TRUE),
        arrivals_female = sum(arrivals_female, na.rm = TRUE),
        .groups = "drop"
      ) %>%
      mutate(
        total_gender_arrivals = arrivals_male + arrivals_female,
        female_ratio = ifelse(total_gender_arrivals > 0, arrivals_female / total_gender_arrivals, 0)
      )
    
    p <- ggplot(df_gender_trend, aes(x = month_year, y = female_ratio)) +
      geom_line(color = "#E74C3C", size = 1) +
      geom_point(color = "#E74C3C", size = 1.5, alpha = 0.7) +
      labs(title = "Historical Monthly Ratio of Female Tourist Arrivals (2022-2024)",
           x = "Month", y = "Proportion Female") +
      scale_y_continuous(labels = scales::percent) + # Format as percentage
      theme_minimal() +
      theme(plot.title = element_text(hjust = 0.5, face = "bold", size = 12),
            axis.text.x = element_text(angle = 45, hjust = 1, size = 8),
            axis.title = element_text(size = 9))
    
    ggplotly(p)
  })
  
}

# --- 5. Run the Shiny Application ---
shinyApp(ui = ui, server = server)
