### Restructured bar charts by vaccination strategy

# Age group colors
age_group_colors <- c(
  "0-4" = "#FDE725FF",  
  "5-64" = "#2A788EFF",   
  "65+" = "#440154FF"   
)

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
under_5_data <- create_averted_cases_data("noro_1")
age_15_64_data <- create_averted_cases_data("noro_2_3") 
over_65_data <- create_averted_cases_data("noro_4")

# Chart 1: V1 (Under 5 vaccination) effectiveness across age groups
v1_chart_data <- data.frame(
  age_group = c("0-4", "5-64", "65+"),
  
  # Calculate averted cases for V1 strategy
  averted = c(
    (under_5_data$sum_data_no_vac - under_5_data$sum_data_under_5) / under_5_data$sum_data_no_vac,
    (age_15_64_data$sum_data_no_vac - age_15_64_data$sum_data_under_5) / age_15_64_data$sum_data_no_vac,
    (over_65_data$sum_data_no_vac - over_65_data$sum_data_under_5) / over_65_data$sum_data_no_vac
  ),
  
  # Calculate confidence intervals
  upper = c(
    (under_5_data$sum_data_no_vac - under_5_data$sum_data_under_5_2.5) / under_5_data$sum_data_no_vac,
    (age_15_64_data$sum_data_no_vac - age_15_64_data$sum_data_under_5_2.5) / age_15_64_data$sum_data_no_vac,
    (over_65_data$sum_data_no_vac - over_65_data$sum_data_under_5_2.5) / over_65_data$sum_data_no_vac
  ),
  
  lower = c(
    (under_5_data$sum_data_no_vac - under_5_data$sum_data_under_5_97.5) / under_5_data$sum_data_no_vac,
    (age_15_64_data$sum_data_no_vac - age_15_64_data$sum_data_under_5_97.5) / age_15_64_data$sum_data_no_vac,
    (over_65_data$sum_data_no_vac - over_65_data$sum_data_under_5_97.5) / over_65_data$sum_data_no_vac
  )
) %>%
  mutate(age_group = factor(age_group, levels = c("0-4", "5-64", "65+")))

v1_effectiveness_chart <- v1_chart_data %>%
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
    title = "V1: Under 5 Vaccination Strategy",
    x = "Age Group",
    y = "Percentage symptomatic cases averted"
  ) +
  scale_fill_manual(values = age_group_colors) +
  scale_y_continuous(labels = scales::percent, limits = c(0, 1.0))

# Chart 2: V2 (65+ vaccination) effectiveness across age groups
v2_chart_data <- data.frame(
  age_group = c("0-4", "5-64", "65+"),
  
  # Calculate averted cases for V2 strategy
  averted = c(
    (under_5_data$sum_data_no_vac - under_5_data$sum_data_over65) / under_5_data$sum_data_no_vac,
    (age_15_64_data$sum_data_no_vac - age_15_64_data$sum_data_over65) / age_15_64_data$sum_data_no_vac,
    (over_65_data$sum_data_no_vac - over_65_data$sum_data_over65) / over_65_data$sum_data_no_vac
  ),
  
  # Calculate confidence intervals
  upper = c(
    (under_5_data$sum_data_no_vac - under_5_data$sum_data_over65_2.5) / under_5_data$sum_data_no_vac,
    (age_15_64_data$sum_data_no_vac - age_15_64_data$sum_data_over65_2.5) / age_15_64_data$sum_data_no_vac,
    (over_65_data$sum_data_no_vac - over_65_data$sum_data_over65_2.5) / over_65_data$sum_data_no_vac
  ),
  
  lower = c(
    (under_5_data$sum_data_no_vac - under_5_data$sum_data_over65_97.5) / under_5_data$sum_data_no_vac,
    (age_15_64_data$sum_data_no_vac - age_15_64_data$sum_data_over65_97.5) / age_15_64_data$sum_data_no_vac,
    (over_65_data$sum_data_no_vac - over_65_data$sum_data_over65_97.5) / over_65_data$sum_data_no_vac
  )
) %>%
  mutate(age_group = factor(age_group, levels = c("0-4", "5-64", "65+")))

v2_effectiveness_chart <- v2_chart_data %>%
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
    y = "Percentage symptomatic cases averted"
  ) +
  scale_fill_manual(values = age_group_colors) +
  scale_y_continuous(labels = scales::percent, limits = c(0, 1.0))

# Chart 3: V3 (Under 5 and 65+ vaccination) effectiveness across age groups
v3_chart_data <- data.frame(
  age_group = c("0-4", "5-64", "65+"),
  
  # Calculate averted cases for V3 strategy
  averted = c(
    (under_5_data$sum_data_no_vac - under_5_data$sum_data_under5_over65) / under_5_data$sum_data_no_vac,
    (age_15_64_data$sum_data_no_vac - age_15_64_data$sum_data_under5_over65) / age_15_64_data$sum_data_no_vac,
    (over_65_data$sum_data_no_vac - over_65_data$sum_data_under5_over65) / over_65_data$sum_data_no_vac
  ),
  
  # Calculate confidence intervals
  upper = c(
    (under_5_data$sum_data_no_vac - under_5_data$sum_data_under5_over65_2.5) / under_5_data$sum_data_no_vac,
    (age_15_64_data$sum_data_no_vac - age_15_64_data$sum_data_under5_over65_2.5) / age_15_64_data$sum_data_no_vac,
    (over_65_data$sum_data_no_vac - over_65_data$sum_data_under5_over65_2.5) / over_65_data$sum_data_no_vac
  ),
  
  lower = c(
    (under_5_data$sum_data_no_vac - under_5_data$sum_data_under5_over65_97.5) / under_5_data$sum_data_no_vac,
    (age_15_64_data$sum_data_no_vac - age_15_64_data$sum_data_under5_over65_97.5) / age_15_64_data$sum_data_no_vac,
    (over_65_data$sum_data_no_vac - over_65_data$sum_data_under5_over65_97.5) / over_65_data$sum_data_no_vac
  )
) %>%
  mutate(age_group = factor(age_group, levels = c("0-4", "5-64", "65+")))

v3_effectiveness_chart <- v3_chart_data %>%
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
    title = "V3: Under 5 and 65+ Vaccination Strategy",
    x = "Age Group",
    y = "Percentage symptomatic cases averted"
  ) +
  scale_fill_manual(values = age_group_colors) +
  scale_y_continuous(labels = scales::percent, limits = c(0, 1.0))

# Clean up memory
# remove(no_vaccination_uncertainty)
# remove(under_5_vaccination_uncertainty) 
# remove(over65_vaccination_uncertainty)
# remove(under5_over65_vaccination_uncertainty)