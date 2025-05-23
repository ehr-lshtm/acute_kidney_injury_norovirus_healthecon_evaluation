# Create sequence of values
aki_values <- seq(0, 0.3, by = 0.005)

#######################
#######################
#######################

# Initialize an empty data frame to store results
threshold_under5_over65_df <- data.frame()

# Run the loop
for (i in seq_along(aki_values)) {

  # Run the model
  current_result <- cea_one_way_analysis_function(
    data = psa_mean_averted_combo_vaccination,
    aki_hosp_parameter_4 = aki_values[i]
  )
  
  # Create a row with just the aki_value and the result
  current_row <- data.frame(
    aki_value = aki_values[i],
    result = current_result[1]
  )
  
  # Append to results dataframe
  threshold_under5_over65_df <- rbind(threshold_under5_over65_df, current_row)

}

threshold_under5_over65_df <- threshold_under5_over65_df %>%
  mutate(strategy = "V3 under 5 and 65+")

#######################
#######################
#######################

# Initialize an empty data frame to store results
threshold_under5_df <- data.frame()

# Run the loop
for (i in seq_along(aki_values)) {
  params$aki_hospitalisation_4 <- aki_values[i]
  
  # Run the model
  current_result <- cea_one_way_analysis_function(
    data = psa_mean_averted_under5_vaccination,
    aki_hosp_parameter_4 = aki_values[i]
    )
  
  # Create a row with just the aki_value and the result
  current_row <- data.frame(
    aki_value = aki_values[i],
    result = current_result[1]
  )
  
  # Append to results dataframe
  threshold_under5_df <- rbind(threshold_under5_df, current_row)

}

threshold_under5_df <- threshold_under5_df %>% mutate(strategy = "V1 under 5")


#######################
#######################
#######################

# Initialize an empty data frame to store results
threshold_over65_df <- data.frame()

# Run the loop
for (i in seq_along(aki_values)) {
  params$aki_hospitalisation_4 <- aki_values[i]
  
  # Run the model
  current_result <- cea_one_way_analysis_function(
    data = psa_mean_averted_over65_vaccination,
    aki_hosp_parameter_4 = aki_values[i]
    )
  
  # Create a row with just the aki_value and the result
  current_row <- data.frame(
    aki_value = aki_values[i],
    result = current_result[1]
  )
  
  # Append to results dataframe
  threshold_over65_df <- rbind(threshold_over65_df, current_row)

}

threshold_over65_df <- threshold_over65_df %>% mutate(strategy = "V2 65+")


# plot

# Create enhanced plot
threshold_analysis_plot <- function (data) { 
  data %>%
  ggplot(aes(x = aki_value, y = result)) +
    annotate("rect", 
             xmin = -Inf, xmax = Inf,
             ymin = 13000, ymax = 30000,
             fill = "grey80",
             alpha = 0.3) +
  # Add main line with improved aesthetics
  geom_line(color = "#2C3E50", size = 1) +
  # Add cost-effectiveness threshold
  geom_hline(yintercept = 20000, linetype = "dashed", color = "#E74C3C", size = 0.8) +
  # Add points for actual data points
  geom_point(data = data, color = "#2C3E50", size = 3) +
  # Add annotation for threshold
  # annotate("text", x = max(aki_values), y = 20000, 
  #          label = "Cost-effectiveness threshold (£20,000)", 
  #          hjust = 1, size = 3.5, color = "#E74C3C") +
  # Improve labels
  labs(
    x = "Norovirus attributable AKI hospitalisation rate in over 65s",
    y = "Incremental cost-effectiveness ratio (ICER)") +
  # Enhanced theme
  theme_minimal() +
    theme(
      axis.title = element_text(size = 10),
      axis.text = element_text(size = 9),
      panel.grid.minor = element_line(color = "grey95"),  # Make minor grid lines lighter
      panel.grid.major = element_line(color = "grey90"),
      plot.caption = element_text(size = 8, color = "grey40"),
      plot.margin = margin(t = 20, r = 20, b = 20, l = 20)
    ) +
  # Format axis
    scale_x_continuous(
      # Major breaks (for labels) every 0.1
      breaks = seq(0, 0.3, by = 0.1),
      # Minor breaks (for gridlines) every 0.05
      minor_breaks = seq(0, 0.3, by = 0.05),
      labels = scales::number_format(accuracy = 0.1)
    ) +
  scale_y_continuous(
    labels = scales::comma_format(),
    breaks = seq(-20000, 700000, by = 40000),
    limits = c(-20000, 700000)
  )
}

threshold_under5_over65_plot <- threshold_analysis_plot(threshold_under5_over65_df)
threshold_under5_plot <- threshold_analysis_plot(threshold_under5_df)
threshold_over65_plot <- threshold_analysis_plot(threshold_over65_df)

# annotate_figure(
#   ggarrange(
#     threshold_under5_over65_plot + xlab(NULL) + ylab(NULL), 
#     threshold_under5_plot + xlab(NULL) + ylab(NULL), 
#     threshold_over65_plot + xlab(NULL) + ylab(NULL), 
#     ncol = 3, 
#     nrow = 1, 
#     common.legend = TRUE, 
#     legend = "none"
#   ),
#   bottom = text_grob("Proportion of symptomatic norovirus infections in over 65s with an AKI hospitalisation"),
#   left = text_grob("Incremental cost effectiveness ratio (ICER)", rot = 90)
# )

############################################################################################
####################### threshold analysis of vaccine cost#######################
############################################################################################

# Create sequence of values
vaccine_cost_values <- seq(0, 500, by = 50)

#######################
#######################
#######################

# Initialize an empty data frame to store results
vaccine_threshold_under5_over65_df <- data.frame()

# Run the loop
for (i in seq_along(vaccine_cost_values)) {
  cost_per_dose <- vaccine_cost_values[i]
  
  # Run the model
  current_result <- cea_one_way_analysis_function(
    cost_per_dose = cost_per_dose,
    data = psa_mean_averted_combo_vaccination,
  )
  
  # Create a row with just the aki_value and the result
  current_row <- data.frame(
    vaccine_cost_values = vaccine_cost_values[i],
    result = current_result[1]
  )
  
  # Append to results dataframe
  vaccine_threshold_under5_over65_df <- rbind(vaccine_threshold_under5_over65_df, current_row)

}

#######################
#######################
#######################

# Initialize an empty data frame to store results
vaccine_threshold_under5_df <- data.frame()

# Run the loop
for (i in seq_along(vaccine_cost_values)) {

  # Run the model
  current_result <- cea_one_way_analysis_function(
    cost_per_dose = vaccine_cost_values[i],
    data = psa_mean_averted_under5_vaccination
  )
  
  # Create a row with just the aki_value and the result
  current_row <- data.frame(
    vaccine_cost_values = vaccine_cost_values[i],
    result = current_result[1]
  )
  
  # Append to results dataframe
  vaccine_threshold_under5_df <- rbind(vaccine_threshold_under5_df, current_row)

}

#######################
#######################
#######################

# Initialize an empty data frame to store results
vaccine_threshold_over65_df <- data.frame()

# Run the loop
for (i in seq_along(vaccine_cost_values)) {

    # Run the model
  current_result <- cea_one_way_analysis_function(
    cost_per_dose = vaccine_cost_values[i],
    data = psa_mean_averted_over65_vaccination
    )
  
  # Create a row with just the aki_value and the result
  current_row <- data.frame(
    vaccine_cost_values = vaccine_cost_values[i],
    result = current_result[1]
  )
  
  # Append to results dataframe
  vaccine_threshold_over65_df <- rbind(vaccine_threshold_over65_df, current_row)

}

# plot

# Create enhanced plot
vaccine_threshold_analysis_plot <- function (data) { 
  data %>%
    ggplot(aes(x = vaccine_cost_values, y = result)) +
    # Add main line with improved aesthetics
    geom_line(color = "#2C3E50", size = 1) +
    # Add cost-effectiveness threshold
    geom_hline(yintercept = 20000, linetype = "dashed", color = "#E74C3C", size = 0.8) +
    # Add points for actual data points
    geom_point(data = data, color = "#2C3E50", size = 3) +
    # Add annotation for threshold
    # annotate("text", x = max(aki_values), y = 20000, 
    #          label = "Cost-effectiveness threshold (£20,000)", 
    #          hjust = 1, size = 3.5, color = "#E74C3C") +
    # Improve labels
    labs(
      x = "Vaccine cost",
      y = "Incremental cost-effectiveness ratio (ICER)") +
    # Enhanced theme
    theme_minimal() +
    theme(
      axis.title = element_text(size = 10),
      axis.text = element_text(size = 9),
      panel.grid.minor = element_blank(),
      panel.grid.major = element_line(color = "grey90"),
      plot.caption = element_text(size = 8, color = "grey40"),
      plot.margin = margin(t = 20, r = 20, b = 20, l = 20)
    ) +
    # Format axis
    scale_x_continuous(
      breaks = seq(0, 500, by = 50)
    ) +
    scale_y_continuous(
      labels = scales::comma_format(),
      breaks = seq(-20000, 60000, by = 10000),
      limits = c(-20000, 60000)
    )
}

Vaccine_threshold_under5_over65_plot <- vaccine_threshold_analysis_plot(vaccine_threshold_under5_over65_df)
vaccine_threshold_under5_plot <- vaccine_threshold_analysis_plot(vaccine_threshold_under5_df)
vaccine_threshold_over65_plot <- vaccine_threshold_analysis_plot(vaccine_threshold_over65_df)

annotate_figure(
  ggarrange(
    Vaccine_threshold_under5_over65_plot + xlab(NULL) + ylab(NULL), 
    vaccine_threshold_under5_plot + xlab(NULL) + ylab(NULL), 
    vaccine_threshold_over65_plot + xlab(NULL) + ylab(NULL), 
    ncol = 3, 
    nrow = 1, 
    common.legend = TRUE, 
    legend = "none"
  ),
  bottom = text_grob("Vaccine cost"),
  left = text_grob("Incremental cost effectiveness ratio (ICER)", rot = 90)
)


##############
############# full incremental threshold analysis A vs C

# Initialize an empty data frame to store results
full_incr_threshold_under5_over65_df <- data.frame()

# Run the loop
for (i in seq_along(aki_values)) {
  
  # Run the model
  current_result <- cea_one_way_analysis_function(
    data = psa_mean_averted_combo_vaccination,
    icer_only = FALSE,
    aki_hosp_parameter_4 = aki_values[i]
  )
  
  # Create a row with just the aki_value and the result
  current_row <- data.frame(
    aki_value = aki_values[i],
    incr_cost = current_result["incr_cost"],
    qaly_gain = current_result["qaly_gain"],
    row.names = NULL  
    )
  
  
  # Append to results dataframe
  full_incr_threshold_under5_over65_df <- rbind(full_incr_threshold_under5_over65_df, current_row)
  rownames(full_incr_threshold_under5_over65_df) <- NULL  # Reset row names after rbind
  
}

##############

# Initialize an empty data frame to store results
full_incr_threshold_under5_df <- data.frame()

# Run the loop
for (i in seq_along(aki_values)) {
  params$aki_hospitalisation_4 <- aki_values[i]
  
  # Run the model
  current_result <- cea_one_way_analysis_function(
    data = psa_mean_averted_under5_vaccination,
    icer_only = FALSE,
    aki_hosp_parameter_4 = aki_values[i]
  )
  
  
  # Create a row with just the aki_value and the result
  current_row <- data.frame(
    aki_value = aki_values[i],
    incr_cost = current_result["incr_cost"],
    qaly_gain = current_result["qaly_gain"],
    row.names = NULL  
  )
  
  # Append to results dataframe
  full_incr_threshold_under5_df <- rbind(full_incr_threshold_under5_df, current_row)
  rownames(full_incr_threshold_under5_df) <- NULL  # Reset row names after rbind
  
  
}

full_incr_threshold_df <- full_incr_threshold_under5_over65_df %>%
  rename(incr_cost_under_over65 = incr_cost, qaly_gain_under_over65 = qaly_gain) %>% 
  left_join(full_incr_threshold_under5_df %>% 
              rename(incr_cost_under5 = incr_cost, qaly_gain_under5 = qaly_gain), by = "aki_value") %>% 
  mutate(
    incr_cost_diff = incr_cost_under_over65 - incr_cost_under5,
    qaly_gain_diff = qaly_gain_under_over65 - qaly_gain_under5,
    result = incr_cost_diff / qaly_gain_diff,
    strategy = "Incremental threshold V3 vs V1"
  ) %>% 
  select(aki_value, result, strategy)

full_incr_threshold_plot <- threshold_analysis_plot(full_incr_threshold_df)


scenario_colors_threshold <- c(
  "V3 under 5 and 65+" = "#56B4E9",  
  "V1 under 5" =  "#FFA500",          
  "V2 65+" = "#22A884FF",
  "Incremental threshold V3 vs V1" = "#440154FF")

incremental_threshold <- threshold_under5_over65_df %>%
  rbind(threshold_under5_df, threshold_over65_df, full_incr_threshold_df) %>%
  filter(aki_value <= 0.30)  %>%
  ggplot(aes(x = aki_value, y = result, colour = strategy, linetype = strategy)) +
  annotate("rect", 
           xmin = -Inf, xmax = Inf,
           ymin = 13000, ymax = 30000,
           fill = "grey80",
           alpha = 0.3) +
  # Add main line with improved aesthetics
  geom_line(size = 1.1) +
  # Add cost-effectiveness threshold
  geom_hline(yintercept = 20000, linetype = "dashed", color = "black", size = 0.8) +
  # Add points for actual data points
  geom_point(color = "#2C3E50", size = 1) +
  # Improve labels
  labs(
    x = "Norovirus attributable AKI hospitalisation rate in over 65s",
    y = "Average incremental cost-effectiveness ratio (ICER)") +
  # Enhanced theme
  theme_minimal() +
  theme(
    axis.title = element_text(size = 12),
    axis.text = element_text(size = 9),
    # Add more spacing between title and text
    axis.title.x = element_text(margin = margin(t = 15, r = 0, b = 0, l = 0)),
    axis.title.y = element_text(margin = margin(t = 0, r = 15, b = 0, l = 0)),
    panel.grid.minor = element_line(color = "grey95"),  # Make minor grid lines lighter
    panel.grid.major = element_line(color = "grey90"),
    plot.caption = element_text(size = 8, color = "grey40"),
    plot.margin = margin(t = 20, r = 20, b = 20, l = 20),
    legend.position = "top",  # Legend at top
    legend.title = element_text(size = 10),
    legend.text = element_text(size = 12),
    # Improve legend spacing and layout
    legend.margin = margin(b = 15), # Add space below the legend
    legend.key.width = unit(2, "cm") # Make the legend keys wider to show line styles better
  ) +
  # Format axis
  scale_x_continuous(
    # Major breaks (for labels) every 0.1
    breaks = seq(0, 0.3, by = 0.05),
    # Minor breaks (for gridlines) every 0.05
    # minor_breaks = seq(0, 0.01, by = 0.0025),
    labels = scales::number_format(accuracy = 0.01)
  ) +
  scale_y_continuous(
    labels = scales::comma_format(),
    breaks = seq(-20000, 320000, by = 40000),
    limits = c(-20000, 320000)
  ) +
  scale_colour_manual(values = scenario_colors_threshold) +
  # Add different line types for each strategy
  scale_linetype_manual(values = c("solid", "dashed", "dotted", "longdash"))

incremental_threshold
