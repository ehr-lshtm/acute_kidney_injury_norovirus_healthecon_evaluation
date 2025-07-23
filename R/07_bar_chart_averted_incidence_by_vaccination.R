### Restructured bar charts by vaccination strategy

# Age group colors (updated labels)
age_group_colors <- c(
  "0-4" = "#FDE725FF",  
  "5-64" = "#2A788EFF",   
  "65+" = "#440154FF"   
)

# Population data for each age group (actual values from your model)
population_0_4 <- 3857263      
population_5_64 <- 50564583      
population_over_65 <- 12374961

sum_time_series_function <- function (data) {
  data %>% 
    filter(time >= 3535 & time <11000) %>%                                                      
    summarise(across(where(is.numeric), sum, na.rm = TRUE))
}

no_vac_sum <- sum_time_series_function(no_vaccination_uncertainty)
under_5_vac_sum <- sum_time_series_function(under_5_vaccination_uncertainty)
over_65_vac_sum <- sum_time_series_function(over65_vaccination_uncertainty)
under_5_over_65_vac_sum <- sum_time_series_function(under5_over65_vaccination_uncertainty)

# Function to create averted cases data for each age group
create_averted_cases_data <- function(noro_column) {
  data.frame(
    # No vaccination baseline
    sum_data_no_vac = no_vac_sum[[paste0("percentile_50_", noro_column)]],
    sum_data_no_vac_97.5 = no_vac_sum[[paste0("percentile_97.5_", noro_column)]],
    sum_data_no_vac_2.5 = no_vac_sum[[paste0("percentile_2.5_", noro_column)]],
    
    # Under 5 vaccination
    sum_data_under_5 = under_5_vac_sum[[paste0("percentile_50_", noro_column)]],
    sum_data_under_5_97.5 = under_5_vac_sum[[paste0("percentile_97.5_", noro_column)]],
    sum_data_under_5_2.5 = under_5_vac_sum[[paste0("percentile_2.5_", noro_column)]],
    
    # Over 65 vaccination
    sum_data_over65 = over_65_vac_sum[[paste0("percentile_50_", noro_column)]],
    sum_data_over65_97.5 = over_65_vac_sum[[paste0("percentile_97.5_", noro_column)]],
    sum_data_over65_2.5 = over_65_vac_sum[[paste0("percentile_2.5_", noro_column)]],
    
    # Both age groups vaccination
    sum_data_under5_over65 = under_5_over_65_vac_sum[[paste0("percentile_50_", noro_column)]],
    sum_data_under5_over65_97.5 = under_5_over_65_vac_sum[[paste0("percentile_97.5_", noro_column)]],
    sum_data_under5_over65_2.5 = under_5_over_65_vac_sum[[paste0("percentile_2.5_", noro_column)]]
  )
}

# Create data for each age group
age_0_4_data <- create_averted_cases_data("noro_1")
age_5_64_data <- create_averted_cases_data("noro_2_3") 
over_65_data <- create_averted_cases_data("noro_4")

# Chart 1: V1 (0-4 vaccination) incidence per 1000 averted across age groups
v1_chart_data <- data.frame(
  age_group = c("0-4", "5-64", "65+"),
  
  # Calculate incidence per 1000 averted for V1 strategy
  averted = c(
    ((age_0_4_data$sum_data_no_vac - age_0_4_data$sum_data_under_5) / population_0_4) * 1000,
    ((age_5_64_data$sum_data_no_vac - age_5_64_data$sum_data_under_5) / population_5_64) * 1000,
    ((over_65_data$sum_data_no_vac - over_65_data$sum_data_under_5) / population_over_65) * 1000
  ),
  
  # Calculate confidence intervals for incidence per 1000
  upper = c(
    ((age_0_4_data$sum_data_no_vac - age_0_4_data$sum_data_under_5_2.5) / population_0_4) * 1000,
    ((age_5_64_data$sum_data_no_vac - age_5_64_data$sum_data_under_5_2.5) / population_5_64) * 1000,
    ((over_65_data$sum_data_no_vac - over_65_data$sum_data_under_5_2.5) / population_over_65) * 1000
  ),
  
  lower = c(
    ((age_0_4_data$sum_data_no_vac - age_0_4_data$sum_data_under_5_97.5) / population_0_4) * 1000,
    ((age_5_64_data$sum_data_no_vac - age_5_64_data$sum_data_under_5_97.5) / population_5_64) * 1000,
    ((over_65_data$sum_data_no_vac - over_65_data$sum_data_under_5_97.5) / population_over_65) * 1000
  )
) %>%
  mutate(age_group = factor(age_group, levels = c("0-4", "5-64", "65+")))

# Find the maximum value across all charts for consistent y-axis scaling
# (This needs to be calculated AFTER all data is created but BEFORE charts)
max_value_v1 <- max(c(v1_chart_data$upper, v1_chart_data$averted), na.rm = TRUE)
max_value_v2 <- max(c(v2_chart_data$upper, v2_chart_data$averted), na.rm = TRUE)
max_value_v3 <- max(c(v3_chart_data$upper, v3_chart_data$averted), na.rm = TRUE)

# Find the minimum value across all charts (in case there are negative values)
min_value_v1 <- min(c(v1_chart_data$lower, v1_chart_data$averted), na.rm = TRUE)
min_value_v2 <- min(c(v2_chart_data$lower, v2_chart_data$averted), na.rm = TRUE)
min_value_v3 <- min(c(v3_chart_data$lower, v3_chart_data$averted), na.rm = TRUE)

# Set common scale limits (starting from 0)
y_max <- max(max_value_v1, max_value_v2, max_value_v3)

# Add some padding (5% above max)
y_max_padded <- y_max + (y_max * 0.05)

v1_effectiveness_chart_incidence <- v1_chart_data %>%
  ggplot(aes(x = age_group, y = averted, fill = age_group)) +
  geom_bar(stat = "identity") +
  geom_errorbar(
    aes(ymin = pmax(0, lower), ymax = upper),
    width = 0.2,
    color = "black"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(size = 11),
    axis.text.y = element_text(size = 11),
    axis.title.y = element_text(size = 16),
    legend.text = element_text(size = 14),
    legend.title = element_text(size = 16)
  ) +
  labs(
    title = "V1: 0-4 Vaccination Strategy",
    x = "Age Group",
    y = "Cases averted per 1000 population"
  ) +
  scale_fill_manual(values = age_group_colors) +
  scale_y_continuous(labels = scales::comma, limits = c(0, y_max_padded))

# Chart 2: V2 (65+ vaccination) incidence per 1000 averted across age groups
v2_chart_data <- data.frame(
  age_group = c("0-4", "5-64", "65+"),
  
  # Calculate incidence per 1000 averted for V2 strategy
  averted = c(
    ((age_0_4_data$sum_data_no_vac - age_0_4_data$sum_data_over65) / population_0_4) * 1000,
    ((age_5_64_data$sum_data_no_vac - age_5_64_data$sum_data_over65) / population_5_64) * 1000,
    ((over_65_data$sum_data_no_vac - over_65_data$sum_data_over65) / population_over_65) * 1000
  ),
  
  # Calculate confidence intervals for incidence per 1000
  upper = c(
    ((age_0_4_data$sum_data_no_vac - age_0_4_data$sum_data_over65_2.5) / population_0_4) * 1000,
    ((age_5_64_data$sum_data_no_vac - age_5_64_data$sum_data_over65_2.5) / population_5_64) * 1000,
    ((over_65_data$sum_data_no_vac - over_65_data$sum_data_over65_2.5) / population_over_65) * 1000
  ),
  
  lower = c(
    ((age_0_4_data$sum_data_no_vac - age_0_4_data$sum_data_over65_97.5) / population_0_4) * 1000,
    ((age_5_64_data$sum_data_no_vac - age_5_64_data$sum_data_over65_97.5) / population_5_64) * 1000,
    ((over_65_data$sum_data_no_vac - over_65_data$sum_data_over65_97.5) / population_over_65) * 1000
  )
) %>%
  mutate(age_group = factor(age_group, levels = c("0-4", "5-64", "65+")))

v2_effectiveness_chart_incidence <- v2_chart_data %>%
  ggplot(aes(x = age_group, y = averted, fill = age_group)) +
  geom_bar(stat = "identity") +
  geom_errorbar(
    aes(ymin = pmax(0, lower), ymax = upper),
    width = 0.2,
    color = "black"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(size = 11),
    axis.text.y = element_text(size = 11),
    axis.title.y = element_text(size = 16),
    legend.text = element_text(size = 14),
    legend.title = element_text(size = 16)
  ) +
  labs(
    title = "V2: 65+ Vaccination Strategy",
    x = "Age Group",
    y = "Cases averted per 1000 population"
  ) +
  scale_fill_manual(values = age_group_colors) +
  scale_y_continuous(labels = scales::comma, limits = c(0, y_max_padded))

# Chart 3: V3 (0-4 and 65+ vaccination) incidence per 1000 averted across age groups
v3_chart_data <- data.frame(
  age_group = c("0-4", "5-64", "65+"),
  
  # Calculate incidence per 1000 averted for V3 strategy
  averted = c(
    ((age_0_4_data$sum_data_no_vac - age_0_4_data$sum_data_under5_over65) / population_0_4) * 1000,
    ((age_5_64_data$sum_data_no_vac - age_5_64_data$sum_data_under5_over65) / population_5_64) * 1000,
    ((over_65_data$sum_data_no_vac - over_65_data$sum_data_under5_over65) / population_over_65) * 1000
  ),
  
  # Calculate confidence intervals for incidence per 1000
  upper = c(
    ((age_0_4_data$sum_data_no_vac - age_0_4_data$sum_data_under5_over65_2.5) / population_0_4) * 1000,
    ((age_5_64_data$sum_data_no_vac - age_5_64_data$sum_data_under5_over65_2.5) / population_5_64) * 1000,
    ((over_65_data$sum_data_no_vac - over_65_data$sum_data_under5_over65_2.5) / population_over_65) * 1000
  ),
  
  lower = c(
    ((age_0_4_data$sum_data_no_vac - age_0_4_data$sum_data_under5_over65_97.5) / population_0_4) * 1000,
    ((age_5_64_data$sum_data_no_vac - age_5_64_data$sum_data_under5_over65_97.5) / population_5_64) * 1000,
    ((over_65_data$sum_data_no_vac - over_65_data$sum_data_under5_over65_97.5) / population_over_65) * 1000
  )
) %>%
  mutate(age_group = factor(age_group, levels = c("0-4", "5-64", "65+")))

v3_effectiveness_chart_incidence <- v3_chart_data %>%
  ggplot(aes(x = age_group, y = averted, fill = age_group)) +
  geom_bar(stat = "identity") +
  geom_errorbar(
    aes(ymin = pmax(0, lower), ymax = upper),
    width = 0.2,
    color = "black"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(size = 11),
    axis.text.y = element_text(size = 11),
    axis.title.y = element_text(size = 16),
    legend.text = element_text(size = 14),
    legend.title = element_text(size = 16)
  ) +
  labs(
    title = "V3: 0-4 and 65+ Vaccination Strategy",
    x = "Age Group",
    y = "Cases averted per 1000 population"
  ) +
  scale_fill_manual(values = age_group_colors) +
  scale_y_continuous(labels = scales::comma, limits = c(0, y_max_padded))

# Display all three charts
print(v1_effectiveness_chart_incidence)
print(v2_effectiveness_chart_incidence)
print(v3_effectiveness_chart_incidence)

# Clean up memory
# remove(no_vaccination_uncertainty)
# remove(under_5_vaccination_uncertainty) 
# remove(over65_vaccination_uncertainty)
# remove(under5_over65_vaccination_uncertainty)