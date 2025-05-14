### bar chart

scenario_colors_bars <- c(
  "V3 under 5 and 65+" = "#56B4E9",  
  "V1 under 5" =  "#FFA500",          
  "V2 65+" = "#22A884FF"          
)

sum_time_series_function <- function (data) {
  data %>% 
    filter(time >= 3535 & time <11000) %>%                                                      #### Time horizon
    summarise(across(where(is.numeric), sum, na.rm = TRUE))
}

no_vac_sum <- sum_time_series_function(no_vaccination_uncertainty)
under_5_vac_sum <- sum_time_series_function(under_5_vaccination_uncertainty)
over_65_vac_sum <- sum_time_series_function(over65_vaccination_uncertainty)
under_5_over_65_vac_sum <- sum_time_series_function(under5_over65_vaccination_uncertainty)

# averted cases under 5

averted_cases_under5_bar_chart <- data.frame(
  sum_data_no_vac = no_vac_sum$percentile_50_noro_1,
  sum_data_no_vac_97.5 = no_vac_sum$percentile_97.5_noro_1,
  sum_data_no_vac_2.5 = no_vac_sum$percentile_2.5_noro_1,
  
  sum_data_under_5 = under_5_vac_sum$percentile_50_noro_1,
  sum_data_under_5_97.5 = under_5_vac_sum$percentile_97.5_noro_1,
  sum_data_under_5_2.5 = under_5_vac_sum$percentile_2.5_noro_1,
  
  sum_data_over65 = over_65_vac_sum$percentile_50_noro_1,
  sum_data_over65_97.5 = over_65_vac_sum$percentile_97.5_noro_1,
  sum_data_over65_2.5 = over_65_vac_sum$percentile_2.5_noro_1,
  
  sum_data_under5_over65 = under_5_over_65_vac_sum$percentile_50_noro_1,
  sum_data_under5_over65_97.5 = under_5_over_65_vac_sum$percentile_97.5_noro_1,
  sum_data_under5_over65_2.5 = under_5_over_65_vac_sum$percentile_2.5_noro_1
) %>% 
  mutate(
    `V1 under 5` = (sum_data_no_vac - sum_data_under_5) / sum_data_no_vac,
    `V1 under 5 lower` = (sum_data_no_vac - sum_data_under_5_97.5) / sum_data_no_vac,
    `V1 under 5 upper` = (sum_data_no_vac - sum_data_under_5_2.5) / sum_data_no_vac,
    
    `V2 65+` = (sum_data_no_vac - sum_data_over65) / sum_data_no_vac,
    `V2 65+ lower` = (sum_data_no_vac - sum_data_over65_97.5) / sum_data_no_vac,
    `V2 65+ upper` = (sum_data_no_vac - sum_data_over65_2.5) / sum_data_no_vac,
    
    `V3 under 5 and 65+` = (sum_data_no_vac - sum_data_under5_over65) / sum_data_no_vac,
    `V3 under 5 and 65+ lower` = (sum_data_no_vac - sum_data_under5_over65_97.5) / sum_data_no_vac,
    `V3 under 5 and 65+ upper` = (sum_data_no_vac - sum_data_under5_over65_2.5) / sum_data_no_vac
  ) %>%
  pivot_longer(
    cols = c("V1 under 5", "V2 65+", "V3 under 5 and 65+"),
    names_to = "category",
    values_to = "sum"
  ) %>%
  mutate(
    upper = case_when(
      category == "V1 under 5" ~ `V1 under 5 upper`,
      category == "V2 65+" ~ `V2 65+ upper`,
      category == "V3 under 5 and 65+" ~ `V3 under 5 and 65+ upper`
    ),
    lower = case_when(
      category == "V1 under 5" ~ `V1 under 5 lower`,
      category == "V2 65+" ~ `V2 65+ lower`,
      category == "V3 under 5 and 65+" ~ `V3 under 5 and 65+ lower`
    ),
    category = factor(category, levels = c(
      "V1 under 5",
      "V2 65+",
      "V3 under 5 and 65+"
    ))
  ) %>%
  ggplot(aes(x = category, y = sum, fill = category)) +
  geom_bar(stat = "identity") +
  geom_errorbar(
    aes(ymin = pmax(0, lower), ymax = upper),  # Ensure lower bound doesn't go below 0
    width = 0.2,
    color = "black",
    position = position_dodge(width = 0.9)  # Add this to ensure proper alignment
  ) +
  theme_minimal() +
  labs(
    title = "Averted under 5 cases",
    x = NULL,
    y = "Percentage symptomatic cases averted"
  ) +
  scale_fill_manual(
    values = scenario_colors_bars,
    labels = c(
      "V3 under 5 and 65+" = "V3 under 5 and 65+",
      "V1 under 5" = "V1 under 5", 
      "V2 65+" = "V2 65+"
    )
  ) +
  scale_y_continuous(labels = scales::percent, limits = c(0, 1.0))

## averted cases 15-64

averted_cases_15_64_bar_chart <- data.frame(
  sum_data_no_vac = no_vac_sum$percentile_50_noro_2_3,
  sum_data_under_5 = under_5_vac_sum$percentile_50_noro_2_3,
  sum_data_over65 = over_65_vac_sum$percentile_50_noro_2_3,
  sum_data_under5_over65 = under_5_over_65_vac_sum$percentile_50_noro_2_3,
  sum_data_no_vac_97.5 = no_vac_sum$percentile_97.5_noro_2_3,
  sum_data_under_5_97.5 = under_5_vac_sum$percentile_97.5_noro_2_3,
  sum_data_over65_97.5 = over_65_vac_sum$percentile_97.5_noro_2_3,
  sum_data_under5_over65_97.5 = under_5_over_65_vac_sum$percentile_97.5_noro_2_3,
  sum_data_no_vac_2.5 = no_vac_sum$percentile_2.5_noro_2_3,
  sum_data_under_5_2.5 = under_5_vac_sum$percentile_2.5_noro_2_3,
  sum_data_over65_2.5 = over_65_vac_sum$percentile_2.5_noro_2_3,
  sum_data_under5_over65_2.5 = under_5_over_65_vac_sum$percentile_2.5_noro_2_3
) %>% 
  mutate(
    `V1 under 5` = (sum_data_no_vac - sum_data_under_5) / sum_data_no_vac,
    `V1 under 5 lower` = (sum_data_no_vac - sum_data_under_5_97.5) / sum_data_no_vac,
    `V1 under 5 upper` = (sum_data_no_vac - sum_data_under_5_2.5) / sum_data_no_vac,
    
    `V2 65+` = (sum_data_no_vac - sum_data_over65) / sum_data_no_vac,
    `V2 65+ lower` = (sum_data_no_vac - sum_data_over65_97.5) / sum_data_no_vac,
    `V2 65+ upper` = (sum_data_no_vac - sum_data_over65_2.5) / sum_data_no_vac,
    
    `V3 under 5 and 65+` = (sum_data_no_vac - sum_data_under5_over65) / sum_data_no_vac,
    `V3 under 5 and 65+ lower` = (sum_data_no_vac - sum_data_under5_over65_97.5) / sum_data_no_vac,
    `V3 under 5 and 65+ upper` = (sum_data_no_vac - sum_data_under5_over65_2.5) / sum_data_no_vac
  ) %>%
  pivot_longer(
    cols = c("V1 under 5", "V2 65+", "V3 under 5 and 65+"),
    names_to = "category",
    values_to = "sum"
  ) %>%
  mutate(
    upper = case_when(
      category == "V1 under 5" ~ `V1 under 5 upper`,
      category == "V2 65+" ~ `V2 65+ upper`,
      category == "V3 under 5 and 65+" ~ `V3 under 5 and 65+ upper`
    ),
    lower = case_when(
      category == "V1 under 5" ~ `V1 under 5 lower`,
      category == "V2 65+" ~ `V2 65+ lower`,
      category == "V3 under 5 and 65+" ~ `V3 under 5 and 65+ lower`
    ),
    category = factor(category, levels = c(
      "V1 under 5",
      "V2 65+",
      "V3 under 5 and 65+"
    ))
  ) %>%
  ggplot(aes(x = category, y = sum, fill = category)) +
  geom_bar(stat = "identity") +
  geom_errorbar(
    aes(ymin = pmax(0, lower), ymax = upper),  # Ensure lower bound doesn't go below 0
    width = 0.2,
    color = "black"
  ) +
  theme_minimal() +
  labs(
    title = "Averted 15-64 cases",
    x = NULL,
    y = "Percentage symptomatic cases averted"
  ) +
  scale_fill_manual(
    values = scenario_colors_bars,
    labels = c(
      "V3 under 5 and 65+" = "V3 under 5 and 65+",
      "V1 under 5" = "V1 under 5", 
      "V2 65+" = "V2 65+"
    )
  ) +
  scale_y_continuous(labels = scales::percent, limits = c(0, 1.0))

## averted cases 65+
averted_cases_over65_bar_chart <- data.frame(
  sum_data_no_vac = no_vac_sum$percentile_50_noro_4,
  sum_data_under_5 = under_5_vac_sum$percentile_50_noro_4,
  sum_data_over65 = over_65_vac_sum$percentile_50_noro_4,
  sum_data_under5_over65 = under_5_over_65_vac_sum$percentile_50_noro_4,
  sum_data_no_vac_97.5 = no_vac_sum$percentile_97.5_noro_4,
  sum_data_under_5_97.5 = under_5_vac_sum$percentile_97.5_noro_4,
  sum_data_over65_97.5 = over_65_vac_sum$percentile_97.5_noro_4,
  sum_data_under5_over65_97.5 = under_5_over_65_vac_sum$percentile_97.5_noro_4,
  sum_data_no_vac_2.5 = no_vac_sum$percentile_2.5_noro_4,
  sum_data_under_5_2.5 = under_5_vac_sum$percentile_2.5_noro_4,
  sum_data_over65_2.5 = over_65_vac_sum$percentile_2.5_noro_4,
  sum_data_under5_over65_2.5 = under_5_over_65_vac_sum$percentile_2.5_noro_4
) %>% 
  mutate(
    `V1 under 5` = (sum_data_no_vac - sum_data_under_5) / sum_data_no_vac,
    `V1 under 5 lower` = (sum_data_no_vac - sum_data_under_5_97.5) / sum_data_no_vac,
    `V1 under 5 upper` = (sum_data_no_vac - sum_data_under_5_2.5) / sum_data_no_vac,
    
    `V2 65+` = (sum_data_no_vac - sum_data_over65) / sum_data_no_vac,
    `V2 65+ lower` = (sum_data_no_vac - sum_data_over65_97.5) / sum_data_no_vac,
    `V2 65+ upper` = (sum_data_no_vac - sum_data_over65_2.5) / sum_data_no_vac,
    
    `V3 under 5 and 65+` = (sum_data_no_vac - sum_data_under5_over65) / sum_data_no_vac,
    `V3 under 5 and 65+ lower` = (sum_data_no_vac - sum_data_under5_over65_97.5) / sum_data_no_vac,
    `V3 under 5 and 65+ upper` = (sum_data_no_vac - sum_data_under5_over65_2.5) / sum_data_no_vac
  ) %>%
  pivot_longer(
    cols = c("V1 under 5", "V2 65+", "V3 under 5 and 65+"),
    names_to = "category",
    values_to = "sum"
  ) %>%
  mutate(
    upper = case_when(
      category == "V1 under 5" ~ `V1 under 5 upper`,
      category == "V2 65+" ~ `V2 65+ upper`,
      category == "V3 under 5 and 65+" ~ `V3 under 5 and 65+ upper`
    ),
    lower = case_when(
      category == "V1 under 5" ~ `V1 under 5 lower`,
      category == "V2 65+" ~ `V2 65+ lower`,
      category == "V3 under 5 and 65+" ~ `V3 under 5 and 65+ lower`
    ),
    category = factor(category, levels = c(
      "V1 under 5",
      "V2 65+",
      "V3 under 5 and 65+"
    ))
  ) %>%
  ggplot(aes(x = category, y = sum, fill = category)) +
  geom_bar(stat = "identity") +
  geom_errorbar(
    aes(ymin = pmax(0, lower), ymax = upper),  # Ensure lower bound doesn't go below 0
    width = 0.2,
    color = "black"
  ) +
  theme_minimal() +
  labs(
    title = "Averted over 65 cases",
    x = NULL,
    y = "Percentage symptomatic cases averted"
  ) +
  scale_fill_manual(
    values = scenario_colors_bars,
    labels = c(
      "V3 under 5 and 65+" = "V3 under 5 and 65+",
      "V1 under 5" = "V1 under 5", 
      "V2 65+" = "V2 65+"
    )
  ) +
  scale_y_continuous(labels = scales::percent, limits = c(0, 1.0))

remove(no_vaccination_uncertainty)
remove(under_5_vaccination_uncertainty) 
remove(over65_vaccination_uncertainty)
remove(under5_over65_vaccination_uncertainty)