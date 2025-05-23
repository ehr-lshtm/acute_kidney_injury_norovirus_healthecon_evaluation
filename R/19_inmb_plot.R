# Create an empty data frame to store the results
results_df <- data.frame()

# Define a vector of wtp values to loop over
wtp_values <- seq(0, 80000, by = 5000)

wtp_values_no_aki <- seq(0, 400000, by = 5000)

inmb_under5_over65 <- cea_one_way_analysis_function(
  wtp = wtp_values,
  data = psa_mean_averted_combo_vaccination,
  inmb_output = TRUE)

inmb_under5 <- cea_one_way_analysis_function(
  wtp = wtp_values,
  data = psa_mean_averted_under5_vaccination,
  inmb_output = TRUE
)

inmb_over65 <- cea_one_way_analysis_function(
  wtp = wtp_values,
  data = psa_mean_averted_over65_vaccination,
  inmb_output = TRUE
)

no_aki_inmb_under5_over65 <- cea_one_way_analysis_function(
  wtp = wtp_values_no_aki,
  data = psa_mean_averted_combo_vaccination,
  inmb_output = TRUE,
  aki_no_outcome = TRUE
)

no_aki_inmb_under5 <- cea_one_way_analysis_function(
  wtp = wtp_values_no_aki,
  data = psa_mean_averted_under5_vaccination,
  inmb_output = TRUE,
  aki_no_outcome = TRUE
)

no_aki_inmb_over65 <- cea_one_way_analysis_function(
  wtp = wtp_values_no_aki,
  data = psa_mean_averted_over65_vaccination,
  inmb_output = TRUE,
  aki_no_outcome = TRUE
)

inmb_df <- data.frame(
  wtp = wtp_values,
  inmb_under5 = inmb_under5/1000000,
  inmb_over65 = inmb_over65/1000000,
  inmb_under5_over65 = inmb_under5_over65/1000000
)

inmb_no_aki_df <- data.frame(
  wtp = wtp_values_no_aki,
  no_aki_inmb_under5 = no_aki_inmb_under5/1000000,
  no_aki_inmb_over65 = no_aki_inmb_over65/1000000,
  no_aki_inmb_under5_over65 = no_aki_inmb_under5_over65/1000000
)


inmb_long <- inmb_df %>%
  pivot_longer(cols = c(inmb_over65, inmb_under5, inmb_under5_over65),
               names_to = "strategy", values_to = "inmb") %>% 
  mutate(strategy = case_when(
    strategy == "inmb_over65" ~ "V2 65+",
    strategy == "inmb_under5" ~ "V1 under 5",
    strategy == "inmb_under5_over65" ~ "V3 under 5 and 65+"
  )) %>% 
  mutate(strategy = factor(strategy, 
                           levels = c("V1 under 5", 
                                      "V2 65+", 
                                      "V3 under 5 and 65+")))


no_aki_inmb_long <- inmb_no_aki_df %>%
  pivot_longer(cols = c(no_aki_inmb_over65, no_aki_inmb_under5, no_aki_inmb_under5_over65),
               names_to = "strategy", values_to = "inmb") %>% 
  mutate(strategy = case_when(
    strategy == "no_aki_inmb_over65" ~ "V2 65+", 
    strategy == "no_aki_inmb_under5" ~ "V1 under 5",
    strategy == "no_aki_inmb_under5_over65" ~ "V3 under 5 and 65+"
  )) %>% 
  mutate(strategy = factor(strategy, 
                           levels = c("V1 under 5", 
                                      "V2 65+", 
                                      "V3 under 5 and 65+")))


# define colours

scenario_colors <- c(
  "V3 under 5 and 65+" = "#56B4E9",  
  "V1 under 5" =  "#FFA500",          
  "V2 65+" = "#22A884FF"          
)

inmb_plot <- ggplot(inmb_long, aes(x = wtp, y = inmb, color = strategy)) +
  geom_line(linewidth = 1) +
  scale_color_manual(
    values = scenario_colors,
    labels = c(
      "V3 under 5 and 65+" = "V3 under 5 and 65+",
      "V1 under 5" = "V1 under 5",
      "V2 65+" = "V2 65+"
    )
  ) +
  theme_minimal() +
  scale_x_continuous(labels = scales::comma, 
                     # expand = expansion(mult = 0.1),
                     breaks = seq(0, 80000, by = 20000),
                     limits = c(0, 80000)
                     ) +
  scale_y_continuous(labels = scales::comma, 
                     # expand = expansion(mult = 0.3)
                     ) +
  geom_abline(slope = 0, intercept = 0, color = "red", linetype = "dashed")
  

no_aki_inmb_plot <- ggplot(no_aki_inmb_long, aes(x = wtp, y = inmb, color = strategy)) +
  geom_line(linewidth = 1) +
  scale_color_manual(
    values = scenario_colors,
    labels = c(
      "V3 under 5 and 65+" = "V3 under 5 and 65+",
      "V1 under 5" = "V1 under 5",
      "V2 65+" = "V2 65+"
    )
  ) +
  theme_minimal() +
  # scale_x_continuous(labels = scales::comma, 
  #                    expand = expansion(mult = 0.1),
  #                    breaks = seq(0, 75000, by = 10000),
  #                    limits = c(0, 75000)
  #                    ) +
  scale_x_continuous(labels = scales::comma, 
                     # expand = expansion(mult = 0.1),
                     breaks = seq(0, 400000, by = 50000),
                     limits = c(0, 400000)
  ) +
  scale_y_continuous(labels = scales::comma, 
                     expand = expansion(mult = 0.3)) +
  geom_abline(slope = 0, intercept = 0, color = "red", linetype = "dashed")

