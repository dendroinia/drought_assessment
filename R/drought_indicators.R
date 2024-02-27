drought_indicators <- function(df, vname, threshold) {
  
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

