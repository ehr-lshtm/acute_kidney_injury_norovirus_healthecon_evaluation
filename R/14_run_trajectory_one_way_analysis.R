### one way analysis trajectory generation

### mean trajectory for each vaccination scenario

psa_mean_averted_under5_vaccination <- averted_under5_vaccination |> 
  group_by(season) |>
  summarize(
    averted_under5 = mean(averted_under5),
    averted_5_14 = mean(averted_5_14),
    averted_15_64 = mean(averted_15_64),
    averted_over65 = mean(averted_over65),
    total_under5_vaccinated = mean(total_under5_vaccinated),
    total_5_14_vaccinated = mean(total_5_14_vaccinated),
    total_15_64_vaccinated = mean(total_15_64_vaccinated),
    total_65_plus_vaccinated = mean(total_65_plus_vaccinated)
  ) |> 
  ungroup()

psa_mean_averted_over65_vaccination <- averted_over65_vaccination |> 
  group_by(season) |>
  summarize(
    averted_under5 = mean(averted_under5),
    averted_5_14 = mean(averted_5_14),
    averted_15_64 = mean(averted_15_64),
    averted_over65 = mean(averted_over65),
    total_under5_vaccinated = mean(total_under5_vaccinated),
    total_5_14_vaccinated = mean(total_5_14_vaccinated),
    total_15_64_vaccinated = mean(total_15_64_vaccinated),
    total_65_plus_vaccinated = mean(total_65_plus_vaccinated)
  ) |> 
  ungroup()

psa_mean_averted_combo_vaccination <- averted_combo_vaccination |> 
  group_by(season) |>
  summarize(
    averted_under5 = mean(averted_under5),
    averted_5_14 = mean(averted_5_14),
    averted_15_64 = mean(averted_15_64),
    averted_over65 = mean(averted_over65),
    total_under5_vaccinated = mean(total_under5_vaccinated),
    total_5_14_vaccinated = mean(total_5_14_vaccinated),
    total_15_64_vaccinated = mean(total_15_64_vaccinated),
    total_65_plus_vaccinated = mean(total_65_plus_vaccinated)
  ) |> 
  ungroup()

#####

run_and_save_scenario <- function(vaccination_rates, vaccination_effect = 0.6, immunity_waning, scenario_label, filename) {
  # Run the scenario (no suffix parameter)
  result <- run_vaccination_scenarios(
    params = default_parameters(),
    vaccination_effect = vaccination_effect,
    vaccination_rate = vaccination_rates,
    immunity_waning = immunity_waning,
    scenario_label = scenario_label,
    n_iterations = n_iter
  )
  
  # Save to file
  write_parquet(result, paste0("data/", filename, ".parquet"))
  
  # Clean up memory
  remove(result)
  gc()
  
  invisible(NULL)
}

# Calculate immunity waning rates once
immunity_6mo <- c(1/(0.5*365), 0)
immunity_9yr <- c(1/(mean(parameter_probabilistic_samples$probabilistic$seasonality_parameters$D_immun)*365), 0)
immunity_2yr <- c((1/(2*365)), 0)

# 6 month immunity scenarios
run_and_save_scenario(
  vaccination_rates = c(0, 0, 0, 0),
  immunity_waning = immunity_6mo,
  scenario_label = "No Vaccination 6mo immunity",
  filename = "mean_no_vaccination_6mo_immunity"
)

run_and_save_scenario(
  vaccination_rates = c(target_coverage_under5/days_to_target, 0, 0, 0),
  immunity_waning = immunity_6mo,
  scenario_label = "Under 5 vaccination 6mo immunity",
  filename = "mean_under_5_vaccination_6mo_immunity"
)

run_and_save_scenario(
  vaccination_rates = c(0, 0, 0, target_coverage_over65/days_to_target),
  immunity_waning = immunity_6mo,
  scenario_label = "Over 65 vaccination 6mo immunity",
  filename = "mean_over65_vaccination_6mo_immunity"
)

run_and_save_scenario(
  vaccination_rates = c(target_coverage_under5/days_to_target, 0, 0, target_coverage_over65/days_to_target),
  immunity_waning = immunity_6mo,
  scenario_label = "Under 5 and 65+ vaccination 6mo immunity",
  filename = "mean_under5_over65_vaccination_6mo_immunity"
)

# 9 year immunity scenarios
run_and_save_scenario(
  vaccination_rates = c(0, 0, 0, 0),
  immunity_waning = immunity_9yr,
  scenario_label = "No Vaccination 9yr immunity",
  filename = "mean_no_vaccination_9yr_immunity"
)

run_and_save_scenario(
  vaccination_rates = c(target_coverage_under5/days_to_target, 0, 0, 0),
  immunity_waning = immunity_9yr,
  scenario_label = "Under 5 vaccination 9yr immunity",
  filename = "mean_under_5_vaccination_9yr_immunity"
)

run_and_save_scenario(
  vaccination_rates = c(0, 0, 0, target_coverage_over65/days_to_target),
  immunity_waning = immunity_9yr,
  scenario_label = "Over 65 vaccination 9yr immunity",
  filename = "mean_over65_vaccination_9yr_immunity"
)

run_and_save_scenario(
  vaccination_rates = c(target_coverage_under5/days_to_target, 0, 0, target_coverage_over65/days_to_target),
  immunity_waning = immunity_9yr,
  scenario_label = "Under 5 and 65+ vaccination 9yr immunity",
  filename = "mean_under5_over65_vaccination_9yr_immunity"
)

# 30% vaccine efficacy scenarios (2 year immunity) - with baseline
run_and_save_scenario(
  vaccination_rates = c(0, 0, 0, 0),
  vaccination_effect = 0.3,
  immunity_waning = immunity_2yr,
  scenario_label = "No Vaccination 2yr immunity",
  filename = "mean_no_vaccination_2yr_immunity"
)

run_and_save_scenario(
  vaccination_rates = c(target_coverage_under5/days_to_target, 0, 0, 0),
  vaccination_effect = 0.3,
  immunity_waning = immunity_2yr,
  scenario_label = "Under 5 vaccination",
  filename = "mean_under_5_vaccination_30_VE"
)

run_and_save_scenario(
  vaccination_rates = c(0, 0, 0, target_coverage_over65/days_to_target),
  vaccination_effect = 0.3,
  immunity_waning = immunity_2yr,
  scenario_label = "Over 65 vaccination",
  filename = "mean_over65_vaccination_30_VE"
)

run_and_save_scenario(
  vaccination_rates = c(target_coverage_under5/days_to_target, 0, 0, target_coverage_over65/days_to_target),
  vaccination_effect = 0.3,
  immunity_waning = immunity_2yr,
  scenario_label = "Under 5 and 65+ vaccination",
  filename = "mean_under5_over65_vaccination_30_VE"
)

# 90% vaccine efficacy scenarios (2 year immunity)
run_and_save_scenario(
  vaccination_rates = c(target_coverage_under5/days_to_target, 0, 0, 0),
  vaccination_effect = 0.9,
  immunity_waning = immunity_2yr,
  scenario_label = "Under 5 vaccination",
  filename = "mean_under_5_vaccination_90_VE"
)

run_and_save_scenario(
  vaccination_rates = c(0, 0, 0, target_coverage_over65/days_to_target),
  vaccination_effect = 0.9,
  immunity_waning = immunity_2yr,
  scenario_label = "Over 65 vaccination",
  filename = "mean_over65_vaccination_90_VE"
)

run_and_save_scenario(
  vaccination_rates = c(target_coverage_under5/days_to_target, 0, 0, target_coverage_over65/days_to_target),
  vaccination_effect = 0.9,
  immunity_waning = immunity_2yr,
  scenario_label = "Under 5 and 65+ vaccination",
  filename = "mean_under5_over65_vaccination_90_VE"
)

##############
#### annualise data for mean trajectory
################

calculate_averted_cases <- function(baseline_file, intervention_file, suffix = NULL) {
  # Load baseline (no vaccination) data
  baseline_data <- process_vaccination_data(baseline_file)
  
  # Load intervention data - only pass suffix if it's not NULL
  if (is.null(suffix)) {
    intervention_data <- process_vaccination_data(intervention_file)
  } else {
    intervention_data <- process_vaccination_data(intervention_file, suffix = suffix)
  }
  
  # Join datasets and calculate averted cases
  if (is.null(suffix)) {
    # For baseline scenarios (no vaccination), averted cases are 0
    result <- baseline_data %>%
      mutate(
        averted_under5 = 0,
        averted_5_14 = 0,
        averted_15_64 = 0,
        averted_over65 = 0
      ) %>% 
      select(season, Iteration, starts_with("averted"), ends_with("vaccinated")) %>% 
      group_by(season) %>%
      summarize(
        averted_under5 = mean(averted_under5, na.rm = TRUE),
        averted_5_14 = mean(averted_5_14, na.rm = TRUE),
        averted_15_64 = mean(averted_15_64, na.rm = TRUE),
        averted_over65 = mean(averted_over65, na.rm = TRUE),
        total_under5_vaccinated = mean(total_under5_vaccinated, na.rm = TRUE),
        total_5_14_vaccinated = mean(total_5_14_vaccinated, na.rm = TRUE),
        total_15_64_vaccinated = mean(total_15_64_vaccinated, na.rm = TRUE),
        total_65_plus_vaccinated = mean(total_65_plus_vaccinated, na.rm = TRUE),
        .groups = 'drop'
      )
  } else {
    # For intervention scenarios, calculate averted cases
    result <- baseline_data %>% 
      select(season, Iteration, starts_with("total_Is")) %>% 
      left_join(intervention_data, by = c("season", "Iteration")) %>%
      mutate(
        averted_under5 = total_Is1 - get(paste0("total_Is1_", suffix)),
        averted_5_14 = total_Is2 - get(paste0("total_Is2_", suffix)),
        averted_15_64 = total_Is3 - get(paste0("total_Is3_", suffix)),
        averted_over65 = total_Is4 - get(paste0("total_Is4_", suffix))
      ) %>% 
      select(season, Iteration, starts_with("averted"), ends_with("vaccinated")) %>% 
      group_by(season) %>%
      summarize(
        averted_under5 = mean(averted_under5, na.rm = TRUE),
        averted_5_14 = mean(averted_5_14, na.rm = TRUE),
        averted_15_64 = mean(averted_15_64, na.rm = TRUE),
        averted_over65 = mean(averted_over65, na.rm = TRUE),
        total_under5_vaccinated = mean(total_under5_vaccinated, na.rm = TRUE),
        total_5_14_vaccinated = mean(total_5_14_vaccinated, na.rm = TRUE),
        total_15_64_vaccinated = mean(total_15_64_vaccinated, na.rm = TRUE),
        total_65_plus_vaccinated = mean(total_65_plus_vaccinated, na.rm = TRUE),
        .groups = 'drop'
      )
  }
  
  return(result)
}

# 6 month immunity scenarios
mean_total_mean_no_vaccination_6mo_immunity <- calculate_averted_cases(
  "data/mean_no_vaccination_6mo_immunity.parquet",
  "data/mean_no_vaccination_6mo_immunity.parquet"
)

mean_averted_under5_vaccination_6mo_immunity <- calculate_averted_cases(
  "data/mean_no_vaccination_6mo_immunity.parquet",
  "data/mean_under_5_vaccination_6mo_immunity.parquet",
  suffix = "vacc_u5"
)

mean_averted_over65_vaccination_6mo_immunity <- calculate_averted_cases(
  "data/mean_no_vaccination_6mo_immunity.parquet",
  "data/mean_over65_vaccination_6mo_immunity.parquet",
  suffix = "vacc_over65"
)

mean_averted_under5_over65_vaccination_6mo_immunity <- calculate_averted_cases(
  "data/mean_no_vaccination_6mo_immunity.parquet",
  "data/mean_under5_over65_vaccination_6mo_immunity.parquet",
  suffix = "vacc_combo"
)

# 9 year immunity scenarios
mean_total_mean_no_vaccination_9yr_immunity <- calculate_averted_cases(
  "data/mean_no_vaccination_9yr_immunity.parquet",
  "data/mean_no_vaccination_9yr_immunity.parquet"
)

mean_averted_under5_vaccination_9yr_immunity <- calculate_averted_cases(
  "data/mean_no_vaccination_9yr_immunity.parquet",
  "data/mean_under_5_vaccination_9yr_immunity.parquet",
  suffix = "vacc_u5"
)

mean_averted_over65_vaccination_9yr_immunity <- calculate_averted_cases(
  "data/mean_no_vaccination_9yr_immunity.parquet",
  "data/mean_over65_vaccination_9yr_immunity.parquet",
  suffix = "vacc_over65"
)

mean_averted_under5_over65_vaccination_9yr_immunity <- calculate_averted_cases(
  "data/mean_no_vaccination_9yr_immunity.parquet",
  "data/mean_under5_over65_vaccination_9yr_immunity.parquet",
  suffix = "vacc_combo"
)

# 2 year immunity baseline
mean_total_mean_no_vaccination_2yr_immunity <- calculate_averted_cases(
  "data/mean_no_vaccination_2yr_immunity.parquet",
  "data/mean_no_vaccination_2yr_immunity.parquet"
)

# 30% vaccine efficacy scenarios
mean_averted_under5_vaccination_30_VE <- calculate_averted_cases(
  "data/mean_no_vaccination_2yr_immunity.parquet",
  "data/mean_under_5_vaccination_30_VE.parquet",
  suffix = "vacc_u5"
)

mean_averted_over65_vaccination_30_VE <- calculate_averted_cases(
  "data/mean_no_vaccination_2yr_immunity.parquet",
  "data/mean_over65_vaccination_30_VE.parquet",
  suffix = "vacc_over65"
)

mean_averted_under5_over65_vaccination_30_VE <- calculate_averted_cases(
  "data/mean_no_vaccination_2yr_immunity.parquet",
  "data/mean_under5_over65_vaccination_30_VE.parquet",
  suffix = "vacc_combo"
)

# 90% vaccine efficacy scenarios
mean_averted_under5_vaccination_90_VE <- calculate_averted_cases(
  "data/mean_no_vaccination_2yr_immunity.parquet",
  "data/mean_under_5_vaccination_90_VE.parquet",
  suffix = "vacc_u5"
)

mean_averted_over65_vaccination_90_VE <- calculate_averted_cases(
  "data/mean_no_vaccination_2yr_immunity.parquet",
  "data/mean_over65_vaccination_90_VE.parquet",
  suffix = "vacc_over65"
)

mean_averted_under5_over65_vaccination_90_VE <- calculate_averted_cases(
  "data/mean_no_vaccination_2yr_immunity.parquet",
  "data/mean_under5_over65_vaccination_90_VE.parquet",
  suffix = "vacc_combo"
)