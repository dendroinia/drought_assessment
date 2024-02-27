library(shiny)
library(DT)
library(data.table)
library(tidyverse)


droughtIndicators <- function(df, vname, threshold) {
  
  # dataframe empty? 
  if (nrow(df) == 0) {
    stop("Input dataframe 'df' is empty.")
  }
  
  # exist the specified variable? 
  if (!vname %in% colnames(df)) {
    stop("Specified variable name 'vname' does not exist in the dataframe.")
  }
  
  if (!is.numeric(df[[vname]])) {
    stop("Specified variable 'vname' must be numeric.")
  }
  
  # Check if the dataframe has columns for 'year' and 'month'
  if (!("year" %in% colnames(df)) || !("month" %in% colnames(df))) {
    stop("Input dataframe 'df' must have columns for 'year' and 'month'.")
  }
  
  require(data.table)
  
  # Add a new column indicating if the value is below the threshold and the following month is also below the threshold
  out <- df |>
    mutate(is_drought = ifelse(
      .data[[vname]] < threshold & lead(.data[[vname]], default = .data[[vname]][n()]) < threshold, 1, 0
    )) |>
    mutate(date = lubridate::make_date(year, month)) 
  
  # Compute the drought duration of the events
  drought_events_all <- out |>
    group_by(index_events = data.table::rleid(is_drought)) |>
    mutate(drought_duration = sum(is_drought)) |>
    as.data.frame() |> 
    dplyr::select(all_of(c("year", "month", vname, "is_drought",
                           "date","index_events","drought_duration")))
  
  # Filter events with drought duration > 1
  drought_events <- drought_events_all |>
    filter(drought_duration > 1) |>
    as.data.frame()
  
  
  # Compute several indicators (drought assessments)
  da <- drought_events |>
    group_by(index_events) |>
    summarise(
      d_duration = unique(drought_duration),
      d_intensity = round(mean(.data[[vname]], na.rm = TRUE),3),
      d_severity = round(sum(abs(.data[[vname]]), na.rm = TRUE),3),
      lowest_spei = round(min(.data[[vname]]),3),
      month_peak = month[which.min(.data[[vname]])],
      minyear = min(year),
      maxyear = max(year),
      rangeDate = paste(
        lubridate::month(min(date), label = TRUE), "-",
        (lubridate::month(max(date), label = TRUE))
      )
    ) |>
    as.data.frame()
  
  return(list(drought_months = drought_events_all, 
              drought_events = drought_events, 
              drought_assessment = da))
}



# Define UI
ui <- fluidPage(
  titlePanel("Drought Indicators Analysis"),
  
  sidebarLayout(
    sidebarPanel(
      fileInput("file", "Upload CSV File"),
      selectInput("variable", "Select Variable:", choices = NULL),
      numericInput("threshold", "Threshold:", value = 0, step = 0.1),
      actionButton("analyze", "Analyze")
    ),
    mainPanel(
      DTOutput("table")
    )
  )
)

# Define server logic
server <- function(input, output, session) {
  
  # Read uploaded file and update variable choices
  observeEvent(input$file, {
    df <- read.csv(input$file$datapath)
    updateSelectInput(session, "variable", choices = colnames(df))
  })
  
  # Perform analysis when 'Analyze' button is clicked
  observeEvent(input$analyze, {
    req(input$file)  # Ensure file is uploaded
    req(input$variable)  # Ensure variable is selected
    
    # Call the droughtIndicators function
    result <- droughtIndicators(df = read.csv(input$file$datapath),
                                vname = input$variable,
                                threshold = input$threshold)
    
    output$table <- renderDT({
      datatable(result$drought_assessment,
                options = list(lengthChange = FALSE, pageLength = 10))
    })
  })
}

# Run the application
shinyApp(ui = ui, server = server)
