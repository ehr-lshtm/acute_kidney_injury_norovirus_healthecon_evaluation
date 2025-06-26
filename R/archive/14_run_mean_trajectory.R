# run mean trajectory

# Define under 5 scenario base parameters
target_coverage_under5 <- 0.9
target_coverage_over65 <- 0.75
days_to_target <- 365

source("R/norovirus_model_r.R")

run_mean_vaccination_scenarios <- function(params,
                                      vaccination_effect = 0.6,
                                      vaccination_rate = c(0, 0, 0, 0),
                                      immunity_waning = c(0, 0),
                                      scenario_label = "Default",
                                      suffix = "") {
  
  # Set basic parameters
  params[["contacts"]] <- uk_contact_rate_matrix
  params[["vacc_start"]] <- 3535
  
  # Set vaccination parameters
  params[["phi_1"]] <- vaccination_rate
  params[["phi_2"]] <- c(0, 0, 0, 0)
  params[["upsilon"]] <- immunity_waning
  
    #parameters mean values    
    base_sigma <- mean(parameter_probabilistic_samples$probabilistic$infection_parameters$sigma)
    params[["sigma"]] <- c(base_sigma, 
                           base_sigma * (1 - vaccination_effect),
                           base_sigma * (1 - vaccination_effect))
    params[["D_immun"]] <- mean(parameter_probabilistic_samples$probabilistic$seasonality_parameters$D_immun)
    params[["probT_under5"]] <- log(mean(parameter_probabilistic_samples$probabilistic$infection_parameters$probT_under5))
    params[["probT_over5"]] <- log(mean(parameter_probabilistic_samples$probabilistic$infection_parameters$probT_over5))
    params[["season_amp"]] <- mean(parameter_probabilistic_samples$probabilistic$seasonality_parameters$season_amp)
    params[["season_offset"]] <- c(mean(parameter_probabilistic_samples$probabilistic$seasonality_parameters$season_offset))
    params[["season_amp_over65"]] <- 1
    
    # Run model
    # output <- deSolve::lsoda(init_mat, times, norovirus_model_r, params)
    output <- deSolve::radau(init_mat, times, norovirus_model_r, params)
    # output <- deSolve::ode(init_mat, times, norovirus_model_r, params,
    #                        method = "bdf",
    #                        maxsteps = 100000,    # Allow more internal steps
    #                        hmin = 1e-10)   # Method flag: 22 is BDF with internally generated Jacobian
    
    output_df <- as.data.frame(output)
    output_df$Scenario <- scenario_label
    output_df <- label_columns_update(output_df)
    
    # output_df %>% 
    #   filter(time > 3535) %>% 
    #   ggplot() +
    #   geom_line(aes(x = time, y = Is1, color = "Under 5"))
    # 
    
    start_index <- which(output_df$time >= 3535)[1]
    output_df$count <- 0
    output_df$count[start_index:nrow(output_df)] <- seq_len(nrow(output_df) - start_index + 1)
    output_df$season <- ceiling(pmax(output_df$count, 1) / 365) - 1
    
    output_df %>%
      mutate(season = case_when(
        season == 0 & count <= 0 ~ "-S1",
        season == 0 & count > 0 ~ "S0",
        season >= 1 & season <= 20 ~ paste0("S", season),
        TRUE ~ as.character(season)
      )) %>%
      filter(season != "-S1") %>%
      group_by(season) %>%
      reframe(
        total_Is1 = sum(Is1),
        total_Is2 = sum(Is2),
        total_Is3 = sum(Is3),
        total_Is4 = sum(Is4),
        total_under5_vaccinated = sum(new_vaccinations_v1_S_1) + sum(new_vaccinations_v1_R_1),
        total_5_14_vaccinated = sum(new_vaccinations_v1_S_2) + sum(new_vaccinations_v1_R_2),
        total_15_64_vaccinated = sum(new_vaccinations_v1_S_3) + sum(new_vaccinations_v1_R_3),
        total_65_plus_vaccinated = sum(new_vaccinations_v1_S_4) + sum(new_vaccinations_v1_R_4)
      ) %>%
      rename_at(vars(starts_with("total_Is")), 
                ~paste0(., if(suffix != "") paste0("_", suffix) else ""))
    }

### vaccination scenarios

mean_no_vaccination <- run_mean_vaccination_scenarios(
  params = default_parameters(),
  vaccination_effect = 0.6,
  vaccination_rate = c(0, 0, 0, 0),  # Under 5 vaccination
  immunity_waning = c(0, 0),     
  scenario_label = "No Vaccination")

mean_under_5_vaccination <- run_mean_vaccination_scenarios(
  params = default_parameters(),
  vaccination_effect = 0.6,
  vaccination_rate = c(target_coverage_under5/days_to_target, 0, 0, 0),  # Under 5 vaccination
  immunity_waning = c((1/(2*365)), 0),     
  scenario_label = "Under 5 vaccination",
  suffix = "vacc_u5"
)

mean_over65_vaccination <- run_mean_vaccination_scenarios(
  params = default_parameters(),
  vaccination_effect = 0.6,
  vaccination_rate = c(0, 0, 0, target_coverage_over65/days_to_target),  # Over 65 vaccination
  immunity_waning = c((1/(2*365)), 0),     
  scenario_label = "Over 65 vaccination",
  suffix = "vacc_over65"
)

mean_under5_over65_vaccination <- run_mean_vaccination_scenarios(
  params = default_parameters(),
  vaccination_effect = 0.6,
  vaccination_rate = c(target_coverage_under5/days_to_target, 0, 0, target_coverage_over65/days_to_target),  # Over 65 vaccination
  immunity_waning = c((1/(2*365)), 0),     
  scenario_label = "Under 5 and 65+ vaccination",
  suffix = "vacc_combo"
)

### 6 mo immunity scenarios

mean_no_vaccination_6mo_immunity <- run_mean_vaccination_scenarios(
  params = default_parameters(),
  vaccination_effect = 0.6,
  vaccination_rate = c(0, 0, 0, 0),  # Under 5 vaccination
  immunity_waning = c(1/(0.5*365), 0),     # 6 month immunity
  scenario_label = "No Vaccination 6mo immunity")

mean_under_5_vaccination_6mo_immunity <- run_mean_vaccination_scenarios(
  params = default_parameters(),
  vaccination_effect = 0.6,
  vaccination_rate = c(target_coverage_under5/days_to_target, 0, 0, 0),  # Under 5 vaccination
  immunity_waning = c(1/(0.5*365), 0),     # 6 month immunity
  scenario_label = "Under 5 vaccination 6mo immunity",
  suffix = "vacc_u5_6mo"
)

mean_over65_vaccination_6mo_immunity <- run_mean_vaccination_scenarios(
  params = default_parameters(),
  vaccination_effect = 0.6,
  vaccination_rate = c(0, 0, 0, target_coverage_over65/days_to_target),  # Over 65 vaccination
  immunity_waning = c(1/(0.5*365), 0),     # 6 month immunity
  scenario_label = "Over 65 vaccination 6mo immunity",
  suffix = "vacc_over65_6mo"
)

mean_under5_over65_vaccination_6mo_immunity <- run_mean_vaccination_scenarios(
  params = default_parameters(),
  vaccination_effect = 0.6,
  vaccination_rate = c(target_coverage_under5/days_to_target, 0, 0, target_coverage_over65/days_to_target),  # Over 65 vaccination
  immunity_waning = c(1/(0.5*365), 0),     # 6 month immunity
  scenario_label = "Under 5 and 65+ vaccination 6mo immunity",
  suffix = "vacc_combo_6mo"
)

### 9 yrs immunity scenarios

mean_no_vaccination_9yr_immunity <- run_mean_vaccination_scenarios(
  params = default_parameters(),
  vaccination_effect = 0.6,
  vaccination_rate = c(0, 0, 0, 0),  # Under 5 vaccination
  immunity_waning = c(1/(mean(parameter_probabilistic_samples$probabilistic$seasonality_parameters$D_immun)*365), 0),     
  scenario_label = "No Vaccination 9yr immunity")

mean_under_5_vaccination_9yr_immunity <- run_mean_vaccination_scenarios(
  params = default_parameters(),
  vaccination_effect = 0.6,
  vaccination_rate = c(target_coverage_under5/days_to_target, 0, 0, 0),  # Under 5 vaccination
  immunity_waning = c(1/(mean(parameter_probabilistic_samples$probabilistic$seasonality_parameters$D_immun)*365), 0),     
  scenario_label = "Under 5 vaccination 9yr immunity",
  suffix = "vacc_u5_9yr"
)

mean_over65_vaccination_9yr_immunity <- run_mean_vaccination_scenarios(
  params = default_parameters(),
  vaccination_effect = 0.6,
  vaccination_rate = c(0, 0, 0, target_coverage_over65/days_to_target),  # Over 65 vaccination
  immunity_waning = c(1/(mean(parameter_probabilistic_samples$probabilistic$seasonality_parameters$D_immun)*365), 0),     
  scenario_label = "Over 65 vaccination 9yr immunity",
  suffix = "vacc_over65_9yr"
)

mean_under5_over65_vaccination_9yr_immunity <- run_mean_vaccination_scenarios(
  params = default_parameters(),
  vaccination_effect = 0.6,
  vaccination_rate = c(target_coverage_under5/days_to_target, 0, 0, target_coverage_over65/days_to_target),  # Under 5 vaccination and over 65 vaccination
  immunity_waning = c(1/(mean(parameter_probabilistic_samples$probabilistic$seasonality_parameters$D_immun)*365), 0),     
  scenario_label = "Under 5 and 65+ vaccination 9yr immunity",
  suffix = "vacc_combo_9yr"
)

### 30% vaccine efficacy scenarios

mean_under_5_vaccination_30_VE <- run_mean_vaccination_scenarios(
  params = default_parameters(),
  vaccination_effect = 0.3,
  vaccination_rate = c(target_coverage_under5/days_to_target, 0, 0, 0),  # Under 5 vaccination
  immunity_waning = c((1/(2*365)), 0),     
  scenario_label = "Under 5 vaccination",
  suffix = "vacc_u5_30VE"
)

mean_over65_vaccination_30_VE <- run_mean_vaccination_scenarios(
  params = default_parameters(),
  vaccination_effect = 0.3,
  vaccination_rate = c(0, 0, 0, target_coverage_over65/days_to_target),  # Over 65 vaccination
  immunity_waning = c((1/(2*365)), 0),     
  scenario_label = "Over 65 vaccination",
  suffix = "vacc_over65_30VE"
)

mean_under5_over65_vaccination_30_VE <- run_mean_vaccination_scenarios(
  params = default_parameters(),
  vaccination_effect = 0.3,
  vaccination_rate = c(target_coverage_under5/days_to_target, 0, 0, target_coverage_over65/days_to_target),  # Over 65 vaccination
  immunity_waning = c((1/(2*365)), 0),    
  scenario_label = "Under 5 and 65+ vaccination",
  suffix = "vacc_combo_30VE"
)

####

mean_under_5_vaccination_90_VE <- run_mean_vaccination_scenarios(
  params = default_parameters(),
  vaccination_effect = 0.9,
  vaccination_rate = c(target_coverage_under5/days_to_target, 0, 0, 0),  # Under 5 vaccination
  immunity_waning = c((1/(2*365)), 0),     
  scenario_label = "Under 5 vaccination",
  suffix = "vacc_u5_90VE"
)

mean_over65_vaccination_90_VE <- run_mean_vaccination_scenarios(
  params = default_parameters(),
  vaccination_effect = 0.9,
  vaccination_rate = c(0, 0, 0, target_coverage_over65/days_to_target),  # Over 65 vaccination
  immunity_waning = c((1/(2*365)), 0),     
  scenario_label = "Over 65 vaccination",
  suffix = "vacc_over65_90VE"
)

mean_under5_over65_vaccination_90_VE <- run_mean_vaccination_scenarios(
  params = default_parameters(),
  vaccination_effect = 0.9,
  vaccination_rate = c(target_coverage_under5/days_to_target, 0, 0, target_coverage_over65/days_to_target),  # Over 65 vaccination
  immunity_waning = c((1/(2*365)), 0),    
  scenario_label = "Under 5 and 65+ vaccination",
  suffix = "vacc_combo_90VE"
)

### annualise vacciantion scenarios

mean_total_no_vaccination <- mean_no_vaccination %>%
  mutate(
    averted_under5 = total_Is1 - total_Is1,
    averted_5_14 = total_Is2 - total_Is2,
    averted_15_64 = total_Is3 - total_Is3,
    averted_over65 = total_Is4 - total_Is4
  ) %>% 
  select(season, starts_with("averted"))

mean_averted_under5_vaccination <- mean_no_vaccination %>% 
  select(season, starts_with("total_Is")) %>% 
  left_join(mean_under_5_vaccination, by = c("season")) %>%
  mutate(
    averted_under5 = total_Is1 - total_Is1_vacc_u5,
    averted_5_14 = total_Is2 - total_Is2_vacc_u5,
    averted_15_64 = total_Is3 - total_Is3_vacc_u5,
    averted_over65 = total_Is4 - total_Is4_vacc_u5
  ) %>% 
  select(season, starts_with("averted"), ends_with("vaccinated"))

mean_averted_over65_vaccination <- mean_no_vaccination %>%
  select(season, starts_with("total_Is")) %>% 
  left_join(mean_over65_vaccination, by = c("season")) %>%
  mutate(
    averted_under5 = total_Is1 - total_Is1_vacc_over65,
    averted_5_14 = total_Is2 - total_Is2_vacc_over65,
    averted_15_64 = total_Is3 - total_Is3_vacc_over65,
    averted_over65 = total_Is4 - total_Is4_vacc_over65
  ) %>% 
  select(season, starts_with("averted"), ends_with("vaccinated"))

mean_averted_combo_vaccination <- mean_no_vaccination %>%
  select(season, starts_with("total_Is")) %>% 
  left_join(mean_under5_over65_vaccination, by = c("season")) %>%
  mutate(
    averted_under5 = total_Is1 - total_Is1_vacc_combo,
    averted_5_14 = total_Is2 - total_Is2_vacc_combo,
    averted_15_64 = total_Is3 - total_Is3_vacc_combo,
    averted_over65 = total_Is4 - total_Is4_vacc_combo
  ) %>% 
  select(season, starts_with("averted"), ends_with("vaccinated"))

### annualise vaccination scenarios with different immunity durations

mean_averted_under_5_vaccination_6mo_immunity <- mean_no_vaccination_6mo_immunity %>%
  select(season, starts_with("total_Is")) %>% 
  left_join(mean_under_5_vaccination_6mo_immunity, by = c("season")) %>%
  mutate(
    averted_under5 = total_Is1 - total_Is1_vacc_u5_6mo,
    averted_5_14 = total_Is2 - total_Is2_vacc_u5_6mo,
    averted_15_64 = total_Is3 - total_Is3_vacc_u5_6mo,
    averted_over65 = total_Is4 - total_Is4_vacc_u5_6mo
  ) %>% 
  select(season, starts_with("averted"), ends_with("vaccinated"))

mean_averted_over65_vaccination_6mo_immunity <- mean_no_vaccination_6mo_immunity %>%
  select(season, starts_with("total_Is")) %>% 
  left_join(mean_over65_vaccination_6mo_immunity, by = c("season")) %>%
  mutate(
    averted_under5 = total_Is1 - total_Is1_vacc_over65_6mo,
    averted_5_14 = total_Is2 - total_Is2_vacc_over65_6mo,
    averted_15_64 = total_Is3 - total_Is3_vacc_over65_6mo,
    averted_over65 = total_Is4 - total_Is4_vacc_over65_6mo
  ) %>% 
  select(season, starts_with("averted"), ends_with("vaccinated"))

mean_averted_combo_vaccination_6mo_immunity <- mean_no_vaccination_6mo_immunity %>%
  select(season, starts_with("total_Is")) %>% 
  left_join(mean_under5_over65_vaccination_6mo_immunity, by = c("season")) %>%
  mutate(
    averted_under5 = total_Is1 - total_Is1_vacc_combo_6mo,
    averted_5_14 = total_Is2 - total_Is2_vacc_combo_6mo,
    averted_15_64 = total_Is3 - total_Is3_vacc_combo_6mo,
    averted_over65 = total_Is4 - total_Is4_vacc_combo_6mo
  ) %>% 
  select(season, starts_with("averted"), ends_with("vaccinated"))

mean_averted_under_5_vaccination_9yr_immunity <- mean_no_vaccination_9yr_immunity %>%
  select(season, starts_with("total_Is")) %>% 
  left_join(mean_under_5_vaccination_9yr_immunity, by = c("season")) %>%
  mutate(
    averted_under5 = total_Is1 - total_Is1_vacc_u5_9yr,
    averted_5_14 = total_Is2 - total_Is2_vacc_u5_9yr,
    averted_15_64 = total_Is3 - total_Is3_vacc_u5_9yr,
    averted_over65 = total_Is4 - total_Is4_vacc_u5_9yr
  ) %>% 
  select(season, starts_with("averted"), ends_with("vaccinated"))

mean_averted_over65_vaccination_9yr_immunity <- mean_no_vaccination_9yr_immunity %>%
  select(season, starts_with("total_Is")) %>% 
  left_join(mean_over65_vaccination_9yr_immunity, by = c("season")) %>%
  mutate(
    averted_under5 = total_Is1 - total_Is1_vacc_over65_9yr,
    averted_5_14 = total_Is2 - total_Is2_vacc_over65_9yr,
    averted_15_64 = total_Is3 - total_Is3_vacc_over65_9yr,
    averted_over65 = total_Is4 - total_Is4_vacc_over65_9yr
  ) %>% 
  select(season, starts_with("averted"), ends_with("vaccinated"))

mean_averted_combo_vaccination_9yr_immunity <- mean_no_vaccination_9yr_immunity %>%
  select(season, starts_with("total_Is")) %>% 
  left_join(mean_under5_over65_vaccination_9yr_immunity, by = c("season")) %>%
  mutate(
    averted_under5 = total_Is1 - total_Is1_vacc_combo_9yr,
    averted_5_14 = total_Is2 - total_Is2_vacc_combo_9yr,
    averted_15_64 = total_Is3 - total_Is3_vacc_combo_9yr,
    averted_over65 = total_Is4 - total_Is4_vacc_combo_9yr
  ) %>% 
  select(season, starts_with("averted"), ends_with("vaccinated"))

### annualise vaccination scenarios with different vaccine efficacy

mean_averted_under_5_vaccination_30_VE <- mean_no_vaccination %>%
  select(season, starts_with("total_Is")) %>% 
  left_join(mean_under_5_vaccination_30_VE, by = c("season")) %>%
  mutate(
    averted_under5 = total_Is1 - total_Is1_vacc_u5_30VE,
    averted_5_14 = total_Is2 - total_Is2_vacc_u5_30VE,
    averted_15_64 = total_Is3 - total_Is3_vacc_u5_30VE,
    averted_over65 = total_Is4 - total_Is4_vacc_u5_30VE
  ) %>% 
  select(season, starts_with("averted"), ends_with("vaccinated"))

mean_averted_over65_vaccination_30_VE <- mean_no_vaccination %>%
  select(season, starts_with("total_Is")) %>% 
  left_join(mean_over65_vaccination_30_VE, by = c("season")) %>%
  mutate(
    averted_under5 = total_Is1 - total_Is1_vacc_over65_30VE,
    averted_5_14 = total_Is2 - total_Is2_vacc_over65_30VE,
    averted_15_64 = total_Is3 - total_Is3_vacc_over65_30VE,
    averted_over65 = total_Is4 - total_Is4_vacc_over65_30VE
  ) %>% 
  select(season, starts_with("averted"), ends_with("vaccinated"))

mean_averted_combo_vaccination_30_VE <- mean_no_vaccination %>%
  select(season, starts_with("total_Is")) %>% 
  left_join(mean_under5_over65_vaccination_30_VE, by = c("season")) %>%
  mutate(
    averted_under5 = total_Is1 - total_Is1_vacc_combo_30VE,
    averted_5_14 = total_Is2 - total_Is2_vacc_combo_30VE,
    averted_15_64 = total_Is3 - total_Is3_vacc_combo_30VE,
    averted_over65 = total_Is4 - total_Is4_vacc_combo_30VE
  ) %>% 
  select(season, starts_with("averted"), ends_with("vaccinated"))

mean_averted_under_5_vaccination_90_VE <- mean_no_vaccination %>%
  select(season, starts_with("total_Is")) %>% 
  left_join(mean_under_5_vaccination_90_VE, by = c("season")) %>%
  mutate(
    averted_under5 = total_Is1 - total_Is1_vacc_u5_90VE,
    averted_5_14 = total_Is2 - total_Is2_vacc_u5_90VE,
    averted_15_64 = total_Is3 - total_Is3_vacc_u5_90VE,
    averted_over65 = total_Is4 - total_Is4_vacc_u5_90VE
  ) %>% 
  select(season, starts_with("averted"), ends_with("vaccinated"))

mean_averted_over65_vaccination_90_VE <- mean_no_vaccination %>%
  select(season, starts_with("total_Is")) %>% 
  left_join(mean_over65_vaccination_90_VE, by = c("season")) %>%
  mutate(
    averted_under5 = total_Is1 - total_Is1_vacc_over65_90VE,
    averted_5_14 = total_Is2 - total_Is2_vacc_over65_90VE,
    averted_15_64 = total_Is3 - total_Is3_vacc_over65_90VE,
    averted_over65 = total_Is4 - total_Is4_vacc_over65_90VE
  ) %>% 
  select(season, starts_with("averted"), ends_with("vaccinated"))

mean_averted_combo_vaccination_90_VE <- mean_no_vaccination %>%
  select(season, starts_with("total_Is")) %>% 
  left_join(mean_under5_over65_vaccination_90_VE, by = c("season")) %>%
  mutate(
    averted_under5 = total_Is1 - total_Is1_vacc_combo_90VE,
    averted_5_14 = total_Is2 - total_Is2_vacc_combo_90VE,
    averted_15_64 = total_Is3 - total_Is3_vacc_combo_90VE,
    averted_over65 = total_Is4 - total_Is4_vacc_combo_90VE
  ) %>% 
  select(season, starts_with("averted"), ends_with("vaccinated"))
