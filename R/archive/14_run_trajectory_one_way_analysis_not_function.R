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

### run PSA for mean trajectories for waning immunity and VE

n_iter <- 100

### 6 mo immunity scenarios

mean_no_vaccination_6mo_immunity <- run_vaccination_scenarios(
  params = default_parameters(),
  vaccination_effect = 0.6,
  vaccination_rate = c(0, 0, 0, 0),  # Under 5 vaccination
  immunity_waning = c(1/(0.5*365), 0),     # 6 month immunity
  scenario_label = "No Vaccination 6mo immunity")

write_parquet(mean_no_vaccination_6mo_immunity, "data/mean_no_vaccination_6mo_immunity.parquet")
remove(mean_no_vaccination_6mo_immunity)
gc()

mean_under_5_vaccination_6mo_immunity <- run_vaccination_scenarios(
  params = default_parameters(),
  vaccination_effect = 0.6,
  vaccination_rate = c(target_coverage_under5/days_to_target, 0, 0, 0),  # Under 5 vaccination
  immunity_waning = c(1/(0.5*365), 0),     # 6 month immunity
  scenario_label = "Under 5 vaccination 6mo immunity"
)

write_parquet(mean_under_5_vaccination_6mo_immunity, "data/mean_under_5_vaccination_6mo_immunity.parquet")
remove(mean_under_5_vaccination_6mo_immunity)
gc()

mean_over65_vaccination_6mo_immunity <- run_vaccination_scenarios(
  params = default_parameters(),
  vaccination_effect = 0.6,
  vaccination_rate = c(0, 0, 0, target_coverage_over65/days_to_target),  # Over 65 vaccination
  immunity_waning = c(1/(0.5*365), 0),     # 6 month immunity
  scenario_label = "Over 65 vaccination 6mo immunity"
)

write_parquet(mean_over65_vaccination_6mo_immunity, "data/mean_over65_vaccination_6mo_immunity.parquet")
remove(mean_over65_vaccination_6mo_immunity)
gc()

mean_under5_over65_vaccination_6mo_immunity <- run_vaccination_scenarios(
  params = default_parameters(),
  vaccination_effect = 0.6,
  vaccination_rate = c(target_coverage_under5/days_to_target, 0, 0, target_coverage_over65/days_to_target),  # Over 65 vaccination
  immunity_waning = c(1/(0.5*365), 0),     # 6 month immunity
  scenario_label = "Under 5 and 65+ vaccination 6mo immunity"
)

write_parquet(mean_under5_over65_vaccination_6mo_immunity, "data/mean_under5_over65_vaccination_6mo_immunity.parquet")
remove(mean_under5_over65_vaccination_6mo_immunity)
gc()


### 9 yrs immunity scenarios
mean_no_vaccination_9yr_immunity <- run_vaccination_scenarios(
  params = default_parameters(),
  vaccination_effect = 0.6,
  vaccination_rate = c(0, 0, 0, 0),  # Under 5 vaccination
  immunity_waning = c(1/(mean(parameter_probabilistic_samples$probabilistic$seasonality_parameters$D_immun)*365), 0),     
  scenario_label = "No Vaccination 9yr immunity")

write_parquet(mean_no_vaccination_9yr_immunity, "data/mean_no_vaccination_9yr_immunity.parquet")
remove(mean_no_vaccination_9yr_immunity)
gc()

mean_under_5_vaccination_9yr_immunity <- run_vaccination_scenarios(
  params = default_parameters(),
  vaccination_effect = 0.6,
  vaccination_rate = c(target_coverage_under5/days_to_target, 0, 0, 0),  # Under 5 vaccination
  immunity_waning = c(1/(mean(parameter_probabilistic_samples$probabilistic$seasonality_parameters$D_immun)*365), 0),     
  scenario_label = "Under 5 vaccination 9yr immunity"
)
write_parquet(mean_under_5_vaccination_9yr_immunity, "data/mean_under_5_vaccination_9yr_immunity.parquet")
remove(mean_under_5_vaccination_9yr_immunity)
gc()

mean_over65_vaccination_9yr_immunity <- run_vaccination_scenarios(
  params = default_parameters(),
  vaccination_effect = 0.6,
  vaccination_rate = c(0, 0, 0, target_coverage_over65/days_to_target),  # Over 65 vaccination
  immunity_waning = c(1/(mean(parameter_probabilistic_samples$probabilistic$seasonality_parameters$D_immun)*365), 0),     
  scenario_label = "Over 65 vaccination 9yr immunity"
)
write_parquet(mean_over65_vaccination_9yr_immunity, "data/mean_over65_vaccination_9yr_immunity.parquet")
remove(mean_over65_vaccination_9yr_immunity)
gc()

mean_under5_over65_vaccination_9yr_immunity <- run_vaccination_scenarios(
  params = default_parameters(),
  vaccination_effect = 0.6,
  vaccination_rate = c(target_coverage_under5/days_to_target, 0, 0, target_coverage_over65/days_to_target),  # Under 5 vaccination and over 65 vaccination
  immunity_waning = c(1/(mean(parameter_probabilistic_samples$probabilistic$seasonality_parameters$D_immun)*365), 0),     
  scenario_label = "Under 5 and 65+ vaccination 9yr immunity"
)
write_parquet(mean_under5_over65_vaccination_9yr_immunity, "data/mean_under5_over65_vaccination_9yr_immunity.parquet")
remove(mean_under5_over65_vaccination_9yr_immunity)
gc()

### 30% vaccine efficacy scenarios
mean_under_5_vaccination_30_VE <- run_vaccination_scenarios(
  params = default_parameters(),
  vaccination_effect = 0.3,
  vaccination_rate = c(target_coverage_under5/days_to_target, 0, 0, 0),  # Under 5 vaccination
  immunity_waning = c((1/(2*365)), 0),     
  scenario_label = "Under 5 vaccination"
)
write_parquet(mean_under_5_vaccination_30_VE, "data/mean_under_5_vaccination_30_VE.parquet")
remove(mean_under_5_vaccination_30_VE)
gc()

mean_over65_vaccination_30_VE <- run_vaccination_scenarios(
  params = default_parameters(),
  vaccination_effect = 0.3,
  vaccination_rate = c(0, 0, 0, target_coverage_over65/days_to_target),  # Over 65 vaccination
  immunity_waning = c((1/(2*365)), 0),     
  scenario_label = "Over 65 vaccination"
)
write_parquet(mean_over65_vaccination_30_VE, "data/mean_over65_vaccination_30_VE.parquet")
remove(mean_over65_vaccination_30_VE)
gc()

mean_under5_over65_vaccination_30_VE <- run_vaccination_scenarios(
  params = default_parameters(),
  vaccination_effect = 0.3,
  vaccination_rate = c(target_coverage_under5/days_to_target, 0, 0, target_coverage_over65/days_to_target),  # Over 65 vaccination
  immunity_waning = c((1/(2*365)), 0),    
  scenario_label = "Under 5 and 65+ vaccination"
)
write_parquet(mean_under5_over65_vaccination_30_VE, "data/mean_under5_over65_vaccination_30_VE.parquet")
remove(mean_under5_over65_vaccination_30_VE)
gc()

### 90% vaccine efficacy scenarios
mean_under_5_vaccination_90_VE <- run_vaccination_scenarios(
  params = default_parameters(),
  vaccination_effect = 0.9,
  vaccination_rate = c(target_coverage_under5/days_to_target, 0, 0, 0),  # Under 5 vaccination
  immunity_waning = c((1/(2*365)), 0),     
  scenario_label = "Under 5 vaccination"
)
write_parquet(mean_under_5_vaccination_90_VE, "data/mean_under_5_vaccination_90_VE.parquet")
remove(mean_under_5_vaccination_90_VE)
gc()

mean_over65_vaccination_90_VE <- run_vaccination_scenarios(
  params = default_parameters(),
  vaccination_effect = 0.9,
  vaccination_rate = c(0, 0, 0, target_coverage_over65/days_to_target),  # Over 65 vaccination
  immunity_waning = c((1/(2*365)), 0),     
  scenario_label = "Over 65 vaccination"
)
write_parquet(mean_over65_vaccination_90_VE, "data/mean_over65_vaccination_90_VE.parquet")
remove(mean_over65_vaccination_90_VE)
gc()

mean_under5_over65_vaccination_90_VE <- run_vaccination_scenarios(
  params = default_parameters(),
  vaccination_effect = 0.9,
  vaccination_rate = c(target_coverage_under5/days_to_target, 0, 0, target_coverage_over65/days_to_target),  # Over 65 vaccination
  immunity_waning = c((1/(2*365)), 0),    
  scenario_label = "Under 5 and 65+ vaccination"
)
write_parquet(mean_under5_over65_vaccination_90_VE, "data/mean_under5_over65_vaccination_90_VE.parquet")
remove(mean_under5_over65_vaccination_90_VE)
gc()

#######################################
### annualise vacciantion scenarios
#######################################


### 6 year immunity scenarios

mean_no_vaccination_6mo_immunity <- process_vaccination_data("data/mean_no_vaccination_6mo_immunity.parquet", suffix = "")

mean_total_mean_no_vaccination_6mo_immunity <- mean_no_vaccination_6mo_immunity %>%
  mutate(
    averted_under5 = total_Is1 - total_Is1,
    averted_5_14 = total_Is2 - total_Is2,
    averted_15_64 = total_Is3 - total_Is3,
    averted_over65 = total_Is4 - total_Is4
  ) %>% 
  select(season, Iteration, starts_with("averted"))

mean_under_5_vaccination_6mo_immunity <- process_vaccination_data("data/mean_under_5_vaccination_6mo_immunity.parquet", , suffix = "vacc_u5")

mean_averted_under5_vaccination_6mo_immunity <- mean_no_vaccination_6mo_immunity %>% 
  select(season, Iteration, starts_with("total_Is")) %>% 
  left_join(mean_under_5_vaccination_6mo_immunity, by = c("season", "Iteration")) %>%
  mutate(
    averted_under5 = total_Is1 - total_Is1_vacc_u5,
    averted_5_14 = total_Is2 - total_Is2_vacc_u5,
    averted_15_64 = total_Is3 - total_Is3_vacc_u5,
    averted_over65 = total_Is4 - total_Is4_vacc_u5
  ) %>% 
  select(season, Iteration, starts_with("averted"), ends_with("vaccinated")) |> 
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

#######################################
### annualise vaccination scenarios
#######################################

### 6 month immunity scenarios
mean_no_vaccination_6mo_immunity <- process_vaccination_data("data/mean_no_vaccination_6mo_immunity.parquet")

mean_under_5_vaccination_6mo_immunity <- process_vaccination_data("data/mean_under_5_vaccination_6mo_immunity.parquet", , suffix = "vacc_u5")
mean_averted_under5_vaccination_6mo_immunity <- mean_no_vaccination_6mo_immunity %>% 
  select(season, Iteration, starts_with("total_Is")) %>% 
  left_join(mean_under_5_vaccination_6mo_immunity, by = c("season", "Iteration")) %>%
  mutate(
    averted_under5 = total_Is1 - total_Is1_vacc_u5,
    averted_5_14 = total_Is2 - total_Is2_vacc_u5,
    averted_15_64 = total_Is3 - total_Is3_vacc_u5,
    averted_over65 = total_Is4 - total_Is4_vacc_u5
  ) %>% 
  select(season, Iteration, starts_with("averted"), ends_with("vaccinated")) |> 
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

mean_over65_vaccination_6mo_immunity <- process_vaccination_data("data/mean_over65_vaccination_6mo_immunity.parquet", , suffix = "vacc_over65")
mean_averted_over65_vaccination_6mo_immunity <- mean_no_vaccination_6mo_immunity %>% 
  select(season, Iteration, starts_with("total_Is")) %>% 
  left_join(mean_over65_vaccination_6mo_immunity, by = c("season", "Iteration")) %>%
  mutate(
    averted_under5 = total_Is1 - total_Is1_vacc_over65,
    averted_5_14 = total_Is2 - total_Is2_vacc_over65,
    averted_15_64 = total_Is3 - total_Is3_vacc_over65,
    averted_over65 = total_Is4 - total_Is4_vacc_over65
  ) %>% 
  select(season, Iteration, starts_with("averted"), ends_with("vaccinated")) |> 
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

mean_under5_over65_vaccination_6mo_immunity <- process_vaccination_data("data/mean_under5_over65_vaccination_6mo_immunity.parquet", , suffix = "vacc_combo")
mean_averted_combo_vaccination_6mo_immunity <- mean_no_vaccination_6mo_immunity %>% 
  select(season, Iteration, starts_with("total_Is")) %>% 
  left_join(mean_under5_over65_vaccination_6mo_immunity, by = c("season", "Iteration")) %>%
  mutate(
    averted_under5 = total_Is1 - total_Is1_vacc_combo,
    averted_5_14 = total_Is2 - total_Is2_vacc_combo,
    averted_15_64 = total_Is3 - total_Is3_vacc_combo,
    averted_over65 = total_Is4 - total_Is4_vacc_combo
  ) %>% 
  select(season, Iteration, starts_with("averted"), ends_with("vaccinated")) |> 
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

### 9 year immunity scenarios
mean_no_vaccination_9yr_immunity <- process_vaccination_data("data/mean_no_vaccination_9yr_immunity.parquet")

mean_under_5_vaccination_9yr_immunity <- process_vaccination_data("data/mean_under_5_vaccination_9yr_immunity.parquet", , suffix = "vacc_u5")
mean_averted_under5_vaccination_9yr_immunity <- mean_no_vaccination_9yr_immunity %>% 
  select(season, Iteration, starts_with("total_Is")) %>% 
  left_join(mean_under_5_vaccination_9yr_immunity, by = c("season", "Iteration")) %>%
  mutate(
    averted_under5 = total_Is1 - total_Is1_vacc_u5_9yr,
    averted_5_14 = total_Is2 - total_Is2_vacc_u5_9yr,
    averted_15_64 = total_Is3 - total_Is3_vacc_u5_9yr,
    averted_over65 = total_Is4 - total_Is4_vacc_u5_9yr
  ) %>% 
  select(season, Iteration, starts_with("averted"), ends_with("vaccinated")) |> 
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

mean_over65_vaccination_9yr_immunity <- process_vaccination_data("data/mean_over65_vaccination_9yr_immunity.parquet", , suffix = "vacc_over65")
mean_averted_over65_vaccination_9yr_immunity <- mean_no_vaccination_9yr_immunity %>% 
  select(season, Iteration, starts_with("total_Is")) %>% 
  left_join(mean_over65_vaccination_9yr_immunity, by = c("season", "Iteration")) %>%
  mutate(
    averted_under5 = total_Is1 - total_Is1_vacc_over65_9yr,
    averted_5_14 = total_Is2 - total_Is2_vacc_over65_9yr,
    averted_15_64 = total_Is3 - total_Is3_vacc_over65_9yr,
    averted_over65 = total_Is4 - total_Is4_vacc_over65_9yr
  ) %>% 
  select(season, Iteration, starts_with("averted"), ends_with("vaccinated")) |> 
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

mean_under5_over65_vaccination_9yr_immunity <- process_vaccination_data("data/mean_under5_over65_vaccination_9yr_immunity.parquet", , suffix = "vacc_combo")
mean_averted_combo_vaccination_9yr_immunity <- mean_no_vaccination_9yr_immunity %>% 
  select(season, Iteration, starts_with("total_Is")) %>% 
  left_join(mean_under5_over65_vaccination_9yr_immunity, by = c("season", "Iteration")) %>%
  mutate(
    averted_under5 = total_Is1 - total_Is1_vacc_combo_9yr,
    averted_5_14 = total_Is2 - total_Is2_vacc_combo_9yr,
    averted_15_64 = total_Is3 - total_Is3_vacc_combo_9yr,
    averted_over65 = total_Is4 - total_Is4_vacc_combo_9yr
  ) %>% 
  select(season, Iteration, starts_with("averted"), ends_with("vaccinated")) |> 
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

### annualise vaccination scenarios with different vaccine efficacy
mean_no_vaccination <- process_vaccination_data("data/mean_no_vaccination.parquet")

mean_under_5_vaccination_30_VE <- process_vaccination_data("data/mean_under_5_vaccination_30_VE.parquet", , suffix = "vacc_u5")
mean_averted_under5_vaccination_30_VE <- mean_no_vaccination %>% 
  select(season, Iteration, starts_with("total_Is")) %>% 
  left_join(mean_under_5_vaccination_30_VE, by = c("season", "Iteration")) %>%
  mutate(
    averted_under5 = total_Is1 - total_Is1_vacc_u5_30VE,
    averted_5_14 = total_Is2 - total_Is2_vacc_u5_30VE,
    averted_15_64 = total_Is3 - total_Is3_vacc_u5_30VE,
    averted_over65 = total_Is4 - total_Is4_vacc_u5_30VE
  ) %>% 
  select(season, Iteration, starts_with("averted"), ends_with("vaccinated")) |> 
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

mean_over65_vaccination_30_VE <- process_vaccination_data("data/mean_over65_vaccination_30_VE.parquet", , suffix = "vacc_over65")
mean_averted_over65_vaccination_30_VE <- mean_no_vaccination %>% 
  select(season, Iteration, starts_with("total_Is")) %>% 
  left_join(mean_over65_vaccination_30_VE, by = c("season", "Iteration")) %>%
  mutate(
    averted_under5 = total_Is1 - total_Is1_vacc_over65_30VE,
    averted_5_14 = total_Is2 - total_Is2_vacc_over65_30VE,
    averted_15_64 = total_Is3 - total_Is3_vacc_over65_30VE,
    averted_over65 = total_Is4 - total_Is4_vacc_over65_30VE
  ) %>% 
  select(season, Iteration, starts_with("averted"), ends_with("vaccinated")) |> 
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

mean_under5_over65_vaccination_30_VE <- process_vaccination_data("data/mean_under5_over65_vaccination_30_VE.parquet", , suffix = "vacc_combo")
mean_averted_combo_vaccination_30_VE <- mean_no_vaccination %>% 
  select(season, Iteration, starts_with("total_Is")) %>% 
  left_join(mean_under5_over65_vaccination_30_VE, by = c("season", "Iteration")) %>%
  mutate(
    averted_under5 = total_Is1 - total_Is1_vacc_combo_30VE,
    averted_5_14 = total_Is2 - total_Is2_vacc_combo_30VE,
    averted_15_64 = total_Is3 - total_Is3_vacc_combo_30VE,
    averted_over65 = total_Is4 - total_Is4_vacc_combo_30VE
  ) %>% 
  select(season, Iteration, starts_with("averted"), ends_with("vaccinated")) |> 
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

mean_under_5_vaccination_90_VE <- process_vaccination_data("data/mean_under_5_vaccination_90_VE.parquet", , suffix = "vacc_u5")
mean_averted_under5_vaccination_90_VE <- mean_no_vaccination %>% 
  select(season, Iteration, starts_with("total_Is")) %>% 
  left_join(mean_under_5_vaccination_90_VE, by = c("season", "Iteration")) %>%
  mutate(
    averted_under5 = total_Is1 - total_Is1_vacc_u5_90VE,
    averted_5_14 = total_Is2 - total_Is2_vacc_u5_90VE,
    averted_15_64 = total_Is3 - total_Is3_vacc_u5_90VE,
    averted_over65 = total_Is4 - total_Is4_vacc_u5_90VE
  ) %>% 
  select(season, Iteration, starts_with("averted"), ends_with("vaccinated")) |> 
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

mean_over65_vaccination_90_VE <- process_vaccination_data("data/mean_over65_vaccination_90_VE.parquet", , suffix = "vacc_over65")
mean_averted_over65_vaccination_90_VE <- mean_no_vaccination %>% 
  select(season, Iteration, starts_with("total_Is")) %>% 
  left_join(mean_over65_vaccination_90_VE, by = c("season", "Iteration")) %>%
  mutate(
    averted_under5 = total_Is1 - total_Is1_vacc_over65_90VE,
    averted_5_14 = total_Is2 - total_Is2_vacc_over65_90VE,
    averted_15_64 = total_Is3 - total_Is3_vacc_over65_90VE,
    averted_over65 = total_Is4 - total_Is4_vacc_over65_90VE
  ) %>% 
  select(season, Iteration, starts_with("averted"), ends_with("vaccinated")) |> 
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

mean_under5_over65_vaccination_90_VE <- process_vaccination_data("data/mean_under5_over65_vaccination_90_VE.parquet", , suffix = "vacc_combo")
mean_averted_combo_vaccination_90_VE <- mean_no_vaccination %>% 
  select(season, Iteration, starts_with("total_Is")) %>% 
  left_join(mean_under5_over65_vaccination_90_VE, by = c("season", "Iteration")) %>%
  mutate(
    averted_under5 = total_Is1 - total_Is1_vacc_combo_90VE,
    averted_5_14 = total_Is2 - total_Is2_vacc_combo_90VE,
    averted_15_64 = total_Is3 - total_Is3_vacc_combo_90VE,
    averted_over65 = total_Is4 - total_Is4_vacc_combo_90VE
  ) %>% 
  select(season, Iteration, starts_with("averted"), ends_with("vaccinated")) |> 
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