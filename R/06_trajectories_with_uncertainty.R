create_vaccination_impact_plot <- function(
    no_vaccination_data,
    vaccination_data,
    y_columns = list(
      percentile_50 = "percentile_50_noro_1",
      percentile_2.5 = "percentile_2.5_noro_1",
      percentile_97.5 = "percentile_97.5_noro_1"
    ),
    title = "Impact Analysis",
    time_filter = 3535,
    vaccination_type = "under_5"  # New parameter to determine colors
) {
  # Determine colors based on vaccination type
  vaccination_color <- switch(vaccination_type,
                              "under_5" = "#FFA500",     # Orange for under 5
                              "over_65" = "#22A884FF",   # Green for over 65
                              "combined" = "#56B4E9",    # Blue for combined
                              "#FFA500"                  # Default to under 5 color
  )
  
  no_vaccination_color <- "#440154FF"  # Purple for no vaccination
  
  safe_column_sum <- function(data, cols) {
    if (length(cols) == 1) {
      as.numeric(data[[cols]])
    } else {
      as.matrix(data[, cols, with = FALSE]) %>% rowSums()
    }
  }
  
  # Convert to data.frame if data.table
  no_vaccination_filtered <- as.data.frame(no_vaccination_data)[no_vaccination_data$time >= time_filter, ]
  vaccination_filtered <- as.data.frame(vaccination_data)[vaccination_data$time >= time_filter, ]
  
  # Create seasonal mapping using no_vaccination_data
  # seasonal_mapping <- no_vaccination_data %>%
  #   select(time, season) %>%
  #   distinct() %>%
  #   arrange(time) %>%
  #   filter(row_number() %% 365 == 1)
  # 
  seasonal_mapping <- no_vaccination_data %>%
    select(time, season) %>%
    # Get unique season-time pairs
    distinct() %>%
    # Group by season
    group_by(season) %>%
    # Get first time point for each season
    slice_min(time, n = 1) %>%
    # Sort by time
    arrange(time)
  
  
  breaks <- seasonal_mapping$time
  labels <- seasonal_mapping$season
  
  # Create custom theme
  custom_theme <- theme_classic() +
    theme(
      plot.title = element_text(size = 11, face = "bold", hjust = 0.5),
      axis.title = element_text(size = 10),
      axis.text = element_text(size = 9),
      axis.text.x = element_text(angle = 45, hjust = 1),
      legend.position = "bottom",
      legend.box = "vertical",
      legend.margin = margin(t = 5, b = 5),
      legend.text = element_text(size = 9),
      legend.title = element_text(size = 10),
      panel.grid.major.y = element_line(color = "grey90", linetype = "dashed"),
      plot.margin = margin(t = 10, r = 10, b = 20, l = 10)
    )
  
  ggplot() +
    # No vaccination scenario - uncertainty ribbon
    geom_ribbon(
      data = no_vaccination_filtered,
      aes(
        x = time,
        ymin = safe_column_sum(no_vaccination_filtered, y_columns$percentile_2.5),
        ymax = safe_column_sum(no_vaccination_filtered, y_columns$percentile_97.5),
        fill = "No Vaccination"
      ),
      alpha = 0.2
    ) +
    
    # Vaccination scenario - uncertainty ribbon
    geom_ribbon(
      data = vaccination_filtered,
      aes(
        x = time,
        ymin = safe_column_sum(vaccination_filtered, y_columns$percentile_2.5),
        ymax = safe_column_sum(vaccination_filtered, y_columns$percentile_97.5),
        fill = "Vaccination"
      ),
      alpha = 0.2
    ) +
    
    # No vaccination scenario - line
    geom_line(
      data = no_vaccination_filtered,
      aes(
        x = time,
        y = safe_column_sum(no_vaccination_filtered, y_columns$percentile_50),
        color = "No Vaccination"
      ),
      size = 0.7
    ) +
    
    # Vaccination scenario - line
    geom_line(
      data = vaccination_filtered,
      aes(
        x = time,
        y = safe_column_sum(vaccination_filtered, y_columns$percentile_50),
        color = "Vaccination"
      ),
      size = 0.7
    ) +
    
    # Colors and legend configuration
    scale_color_manual(
      name = "Scenario",
      values = c(
        "No Vaccination" = no_vaccination_color,
        "Vaccination" = vaccination_color
      )
    ) +
    scale_fill_manual(
      name = "Uncertainty",
      values = c(
        "No Vaccination" = no_vaccination_color,
        "Vaccination" = vaccination_color
      )
    ) +
    
    # Axis formatting
    # scale_y_continuous(
    #   labels = scales::comma,
    #   breaks = scales::pretty_breaks(n = 6),
    #   expand = expansion(mult = c(0.05, 0.1))
    # ) +
    scale_y_continuous(
      labels = scales::comma,
      breaks = function(limits) {
        # Get the range and round up to the nearest thousand
        max_value <- ceiling(limits[2] / 1000) * 1000
        # Create 6 evenly spaced breaks from 0 to max
        seq(0, max_value, length.out = 6)
      },
      expand = expansion(mult = c(0.05, 0.1))
    ) +
    scale_x_continuous(
      breaks = breaks,
      labels = labels
    ) +
    
    # Labels
    labs(
      x = "Season",
      y = "Number of Cases",
      title = title
    ) +
    
    # Apply custom theme
    custom_theme
}


####

# Function to process vaccination uncertainty data
process_vaccination_uncertainty_data <- function(file_path) {
  data <- read_parquet(file_path)
  
  # Calculate combined metrics
  data <- data %>% 
    mutate(
      percentile_2.5_noro_2_3 = percentile_2.5_noro_2 + percentile_2.5_noro_3,
      percentile_50_noro_2_3 = percentile_50_noro_2 + percentile_50_noro_3,
      percentile_97.5_noro_2_3 = percentile_97.5_noro_2 + percentile_97.5_noro_3
    )
  
  # Add season calculation
  start_index <- which(data$time >= 3535)[1]
  data$count <- 0
  data$count[start_index:nrow(data)] <- seq_len(nrow(data) - start_index + 1)
  data$season <- ceiling(pmax(data$count, 1) / 365) - 1
  
  # Create season labels
  data <- data %>% 
    mutate(season = case_when(
      season == 0 & count <= 0 ~ "-S1",
      season == 0 & count > 0 ~ "S0",
      season >= 1 & season <= 10 ~ paste0("S", season),                                                 ####### Time horizon set to 10 years
      TRUE ~ as.character(season)  # This handles any other cases not covered explicitly
    ))
  
  return(data)
}



no_vaccination_uncertainty <- process_vaccination_uncertainty_data("data/no_vaccination_uncertainty.parquet")
under_5_vaccination_uncertainty <- process_vaccination_uncertainty_data("data/under_5_vaccination_uncertainty.parquet")
over65_vaccination_uncertainty <- process_vaccination_uncertainty_data("data/over65_vaccination_uncertainty.parquet")
under5_over65_vaccination_uncertainty <- process_vaccination_uncertainty_data("data/under5_over65_vaccination_uncertainty.parquet")

### under 5 vaccination

# For under 5 cases
under5_vaccination_under5_plot <- create_vaccination_impact_plot(
  no_vaccination_uncertainty,
  under_5_vaccination_uncertainty,
  y_columns = list(
    percentile_50 = "percentile_50_noro_1",
    percentile_2.5 = "percentile_2.5_noro_1",
    percentile_97.5 = "percentile_97.5_noro_1"
  ),
  title = "Under 5 vaccination Under 5 cases",
  vaccination_type = "under_5"
)

# For 5 to 64 cases
under5_vaccination_5to64_plot <- create_vaccination_impact_plot(
  no_vaccination_uncertainty,
  under_5_vaccination_uncertainty,
  y_columns = list(
    percentile_50 = c("percentile_50_noro_2_3"),
    percentile_2.5 = c("percentile_2.5_noro_2_3"),
    percentile_97.5 = c("percentile_97.5_noro_2_3")
  ),
  title = "Under 5 vaccination 5 to 64 cases",
  vaccination_type = "under_5"
)

# For over 65 cases
under5_vaccination_over65_plot <- create_vaccination_impact_plot(
  no_vaccination_uncertainty,
  under_5_vaccination_uncertainty,
  y_columns = list(
    percentile_50 = "percentile_50_noro_4",
    percentile_2.5 = "percentile_2.5_noro_4",
    percentile_97.5 = "percentile_97.5_noro_4"
  ),
  title = "Under 5 vaccination Over 65 cases",
  vaccination_type = "under_5"
)

### over 65 vaccination

# For under 5 cases
over65_vaccination_under5_plot <- create_vaccination_impact_plot(
  no_vaccination_uncertainty,
  over65_vaccination_uncertainty,
  y_columns = list(
    percentile_50 = "percentile_50_noro_1",
    percentile_2.5 = "percentile_2.5_noro_1",
    percentile_97.5 = "percentile_97.5_noro_1"
  ),
  title = "Over 65 vaccination Under 5 cases",
  vaccination_type = "over_65"
  
)

# For 5 to 64 cases
over65_vaccination_5to64_plot <- create_vaccination_impact_plot(
  no_vaccination_uncertainty,
  over65_vaccination_uncertainty,
  y_columns = list(
    percentile_50 = c("percentile_50_noro_2_3"),
    percentile_2.5 = c("percentile_2.5_noro_2_3"),
    percentile_97.5 = c("percentile_97.5_noro_2_3")
  ),
  title = "Over 65 vaccination 5 to 64 cases",
  vaccination_type = "over_65"
  
)

# For over 65 cases
over65_vaccination_over65_plot <- create_vaccination_impact_plot(
  no_vaccination_uncertainty,
  over65_vaccination_uncertainty,
  y_columns = list(
    percentile_50 = "percentile_50_noro_4",
    percentile_2.5 = "percentile_2.5_noro_4",
    percentile_97.5 = "percentile_97.5_noro_4"
  ),
  title = "Over 65 vaccination Over 65 cases",
  vaccination_type = "over_65"
  
)

### under 5 and over 65 vaccination

# For under 5 cases
under5_over65_vaccination_under5_plot <- create_vaccination_impact_plot(
  no_vaccination_uncertainty,
  under5_over65_vaccination_uncertainty ,
  y_columns = list(
    percentile_50 = "percentile_50_noro_1",
    percentile_2.5 = "percentile_2.5_noro_1",
    percentile_97.5 = "percentile_97.5_noro_1"
  ),
  title = "Combination vaccination Under 5 cases",
  vaccination_type = "combined"

)

# For 5 to 64 cases
under5_over65_vaccination_5to64_plot <- create_vaccination_impact_plot(
  no_vaccination_uncertainty,
  under5_over65_vaccination_uncertainty ,
  y_columns = list(
    percentile_50 = c("percentile_50_noro_2_3"),
    percentile_2.5 = c("percentile_2.5_noro_2_3"),
    percentile_97.5 = c("percentile_97.5_noro_2_3")
  ),
  title = "Combination vaccination 5 to 64 cases",
  vaccination_type = "combined"
)

# For over 65 cases
under5_over65_vaccination_over65_plot <- create_vaccination_impact_plot(
  no_vaccination_uncertainty,
  under5_over65_vaccination_uncertainty ,
  y_columns = list(
    percentile_50 = "percentile_50_noro_4",
    percentile_2.5 = "percentile_2.5_noro_4",
    percentile_97.5 = "percentile_97.5_noro_4"
  ),
  title = "Combination vaccination Over 65 cases",
  vaccination_type = "combined"
)

