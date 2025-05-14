##  function


total_health_outcomes_procees <- function(data, n_iterations = n_iter) {

  total_under5_results_list <- vector("list", n_iterations)
  total_5to14_results_list <- vector("list", n_iterations)
  total_15to64_results_list <- vector("list", n_iterations)
  total_over65_results_list <- vector("list", n_iterations)
  
  for (i in 1:n_iterations) {
    # Extract probabilistic parameters for this iteration
    gp_parameter_1 <- parameter_probabilistic_samples$probabilistic$gp_parameters[[1]][i]
    noro_hosp_parameter_1 <- parameter_probabilistic_samples$probabilistic$noro_hosp_parameters[[1]][i]
    noro_morality_parameter_1 <- parameter_probabilistic_samples$probabilistic$noro_mortality_parameters[[1]][i]
    aki_hosp_parameter_1 <- params$aki_hospitalisation_1
    aki_mortality_parameter_1 <- parameter_probabilistic_samples$probabilistic$aki_mortality_parameters[[1]][i]
    gp_parameter_2 <- parameter_probabilistic_samples$probabilistic$gp_parameters[[2]][i]
    noro_hosp_parameter_2 <- parameter_probabilistic_samples$probabilistic$noro_hosp_parameters[[2]][i]
    noro_morality_parameter_2 <- parameter_probabilistic_samples$probabilistic$noro_mortality_parameters[[2]][i]
    aki_hosp_parameter_2 <- params$aki_hospitalisation_2
    aki_mortality_parameter_2 <- parameter_probabilistic_samples$probabilistic$aki_mortality_parameters[[2]][i]
    gp_parameter_3 <- parameter_probabilistic_samples$probabilistic$gp_parameters[[3]][i]
    noro_hosp_parameter_3 <- parameter_probabilistic_samples$probabilistic$noro_hosp_parameters[[3]][i]
    noro_morality_parameter_3 <- parameter_probabilistic_samples$probabilistic$noro_mortality_parameters[[3]][i]
    aki_hosp_parameter_3 <- params$aki_hospitalisation_3
    aki_mortality_parameter_3 <- parameter_probabilistic_samples$probabilistic$aki_mortality_parameters[[3]][i]
    gp_parameter_4 <- parameter_probabilistic_samples$probabilistic$gp_parameters[[4]][i]
    noro_hosp_parameter_4 <- parameter_probabilistic_samples$probabilistic$noro_hosp_parameters[[4]][i]
    noro_morality_parameter_4 <- parameter_probabilistic_samples$probabilistic$noro_mortality_parameters[[4]][i]
    aki_mortality_parameter_4 <- parameter_probabilistic_samples$probabilistic$aki_mortality_parameters[[4]][i]
    aki_hosp_parameter_4 <- parameter_probabilistic_samples$probabilistic$aki_hosp_parameters[[4]][i]
    
    # Calculate health outcomes for each iteration
    
    total_under5_outcomes <- data %>%
      filter(Iteration == i, time >= 3535 & time < 11000) %>%
      as.data.frame() %>% 
      reframe(
        total_infections = sum(Is1),
        total_vaccinated = sum(new_vaccinations_v1_S_1) + sum(new_vaccinations_v1_R_1),
        gp_attendance = sum(Is1) * gp_parameter_1,
        noro_hospitalisation = sum(Is1) * noro_hosp_parameter_1,
        noro_mortality = sum(Is1) * noro_morality_parameter_1,
        aki_hospitalisation = sum(Is1) * aki_hosp_parameter_1,
        aki_mortality = sum(Is1) * aki_hosp_parameter_1 * aki_mortality_parameter_1
      ) %>% 
      mutate(age_group = "0-4")
    
    total_5to14_outcomes <- data %>%
      filter(Iteration == i, time >= 3535 & time < 11000) %>%
      as.data.frame() %>% 
      reframe(
        total_infections = sum(Is2),
        total_vaccinated = sum(new_vaccinations_v1_S_2) + sum(new_vaccinations_v1_R_2),
        gp_attendance = sum(Is2) * gp_parameter_2,
        noro_hospitalisation = sum(Is2) * noro_hosp_parameter_2,
        noro_mortality = sum(Is2) * noro_morality_parameter_2,
        aki_hospitalisation = sum(Is2) * aki_hosp_parameter_2,
        aki_mortality = sum(Is2) * aki_hosp_parameter_2 * aki_mortality_parameter_2
      ) %>% 
      mutate(age_group = "5-14")
    
    total_15to64_outcomes <- data %>%
      filter(Iteration == i, time >= 3535 & time < 11000) %>%
      as.data.frame() %>% 
      reframe(
        total_infections = sum(Is3),
        total_vaccinated = sum(new_vaccinations_v1_S_3) + sum(new_vaccinations_v1_R_3),
        gp_attendance = sum(Is3) * gp_parameter_3,
        noro_hospitalisation = sum(Is3) * noro_hosp_parameter_3,
        noro_mortality = sum(Is3) * noro_morality_parameter_3,
        aki_hospitalisation = sum(Is3) * aki_hosp_parameter_3,
        aki_mortality = sum(Is3) * aki_hosp_parameter_3 * aki_mortality_parameter_3
      ) %>% 
      mutate(age_group = "15-64")
    
    total_over65_outcomes <- data %>%
      filter(Iteration == i, time >= 3535 & time < 11000) %>%
      as.data.frame() %>% 
      reframe(
        total_infections = sum(Is4),
        total_vaccinated = sum(new_vaccinations_v1_S_4) + sum(new_vaccinations_v1_R_4),
        gp_attendance = sum(Is4) * gp_parameter_4,
        noro_hospitalisation = sum(Is4) * noro_hosp_parameter_4,
        noro_mortality = sum(Is4) * noro_morality_parameter_4,
        aki_hospitalisation = sum(Is4) * aki_hosp_parameter_4,
        aki_mortality = sum(Is4) * aki_hosp_parameter_4 * aki_mortality_parameter_4
      ) %>% 
      mutate(age_group = "65+")
    
    total_under5_results_list[[i]] <- c(
      total_infections = total_under5_outcomes$total_infections,
      total_vaccinated = total_under5_outcomes$total_vaccinated,
      gp_attendance = total_under5_outcomes$gp_attendance,
      noro_hospitalisation = total_under5_outcomes$noro_hospitalisation,
      noro_mortality = total_under5_outcomes$noro_mortality,
      aki_hospitalisation = total_under5_outcomes$aki_hospitalisation,
      aki_mortality = total_under5_outcomes$aki_mortality
    )
    
    total_5to14_results_list[[i]] <- c(
      total_infections = total_5to14_outcomes$total_infections,
      total_vaccinated = total_5to14_outcomes$total_vaccinated,
      gp_attendance = total_5to14_outcomes$gp_attendance,
      noro_hospitalisation = total_5to14_outcomes$noro_hospitalisation,
      noro_mortality = total_5to14_outcomes$noro_mortality,
      aki_hospitalisation = total_5to14_outcomes$aki_hospitalisation,
      aki_mortality = total_5to14_outcomes$aki_mortality
    )
    
    total_15to64_results_list[[i]] <- c(
      total_infections = total_15to64_outcomes$total_infections,
      total_vaccinated = total_15to64_outcomes$total_vaccinated,
      gp_attendance = total_15to64_outcomes$gp_attendance,
      noro_hospitalisation = total_15to64_outcomes$noro_hospitalisation,
      noro_mortality = total_15to64_outcomes$noro_mortality,
      aki_hospitalisation = total_15to64_outcomes$aki_hospitalisation,
      aki_mortality = total_15to64_outcomes$aki_mortality
    )
    
    total_over65_results_list[[i]] <- c(
      total_infections = total_over65_outcomes$total_infections,
      total_vaccinated = total_over65_outcomes$total_vaccinated,
      gp_attendance = total_over65_outcomes$gp_attendance,
      noro_hospitalisation = total_over65_outcomes$noro_hospitalisation,
      noro_mortality = total_over65_outcomes$noro_mortality,
      aki_hospitalisation = total_over65_outcomes$aki_hospitalisation,
      aki_mortality = total_over65_outcomes$aki_mortality
    )
    
  }
  
  # Process results
  total_under5_results_df <- do.call(rbind, total_under5_results_list)
  colnames(total_under5_results_df) <- c("total_infections", "total_vaccinated", "gp_attendance", "noro_hospitalisation", "noro_mortality", "aki_hospitalisation", "aki_mortality")
  
  total_5to14_results_df <- do.call(rbind, total_5to14_results_list)
  colnames(total_5to14_results_df) <- c("total_infections", "total_vaccinated", "gp_attendance", "noro_hospitalisation", "noro_mortality", "aki_hospitalisation", "aki_mortality")
  
  total_15to64_results_df <- do.call(rbind, total_15to64_results_list)
  colnames(total_15to64_results_df) <- c("total_infections", "total_vaccinated", "gp_attendance", "noro_hospitalisation", "noro_mortality", "aki_hospitalisation", "aki_mortality")
  
  total_over65_results_df <- do.call(rbind, total_over65_results_list)
  colnames(total_over65_results_df) <- c("total_infections", "total_vaccinated", "gp_attendance", "noro_hospitalisation", "noro_mortality", "aki_hospitalisation", "aki_mortality")
  
  # Calculate summary statistics
  total_under5_summary_stats <- data.frame(
    metric = c("total_infections", "total_vaccinated", "gp_attendance", "noro_hospitalisation", "noro_mortality", "aki_hospitalisation", "aki_mortality"),
    mean = colMeans(total_under5_results_df),
    median = apply(total_under5_results_df, 2, median),
    lower_ci = apply(total_under5_results_df, 2, quantile, probs = 0.025),
    upper_ci = apply(total_under5_results_df, 2, quantile, probs = 0.975)
  ) %>% 
    mutate(value = paste0(formatC(round(mean), format="f", big.mark=",", digits=0), " (",
                          formatC(round(lower_ci), format="f", big.mark=",", digits=0), "-",
                          formatC(round(upper_ci), format="f", big.mark=",", digits=0), ")")) %>% 
    select(metric, value) %>%
    pivot_wider(names_from = metric, 
                values_from = value) %>% 
    mutate(age_group = "0-4")
  
  total_under5_summary_stats_raw <- data.frame(
    metric = c("total_infections", "total_vaccinated", "gp_attendance", "noro_hospitalisation", "noro_mortality", "aki_hospitalisation", "aki_mortality"),
    mean = colMeans(total_under5_results_df),
    median = apply(total_under5_results_df, 2, median),
    lower_ci = apply(total_under5_results_df, 2, quantile, probs = 0.025),
    upper_ci = apply(total_under5_results_df, 2, quantile, probs = 0.975)
  )
  
  total_5to14_summary_stats <- data.frame(
    metric = c("total_infections", "total_vaccinated", "gp_attendance", "noro_hospitalisation", "noro_mortality", "aki_hospitalisation", "aki_mortality"),
    mean = colMeans(total_5to14_results_df),
    median = apply(total_5to14_results_df, 2, median),
    lower_ci = apply(total_5to14_results_df, 2, quantile, probs = 0.025),
    upper_ci = apply(total_5to14_results_df, 2, quantile, probs = 0.975)
  ) %>% 
    mutate(value = paste0(formatC(round(mean), format="f", big.mark=",", digits=0), " (",
                          formatC(round(lower_ci), format="f", big.mark=",", digits=0), "-",
                          formatC(round(upper_ci), format="f", big.mark=",", digits=0), ")")) %>% 
    select(metric, value) %>%
    pivot_wider(names_from = metric, 
                values_from = value) %>%
    mutate(age_group = "5-14")
  
  total_5to14_summary_stats_raw <- data.frame(
    metric = c("total_infections", "total_vaccinated", "gp_attendance", "noro_hospitalisation", "noro_mortality", "aki_hospitalisation", "aki_mortality"),
    mean = colMeans(total_5to14_results_df),
    median = apply(total_5to14_results_df, 2, median),
    lower_ci = apply(total_5to14_results_df, 2, quantile, probs = 0.025),
    upper_ci = apply(total_5to14_results_df, 2, quantile, probs = 0.975)
  )
    
  total_15to64_summary_stats <- data.frame(
    metric = c("total_infections", "total_vaccinated", "gp_attendance", "noro_hospitalisation", "noro_mortality", "aki_hospitalisation", "aki_mortality"),
    mean = colMeans(total_15to64_results_df),
    median = apply(total_15to64_results_df, 2, median),
    lower_ci = apply(total_15to64_results_df, 2, quantile, probs = 0.025),
    upper_ci = apply(total_15to64_results_df, 2, quantile, probs = 0.975)
  ) %>% 
    mutate(value = paste0(formatC(round(mean), format="f", big.mark=",", digits=0), " (",
                          formatC(round(lower_ci), format="f", big.mark=",", digits=0), "-",
                          formatC(round(upper_ci), format="f", big.mark=",", digits=0), ")")) %>% 
    select(metric, value) %>%
    pivot_wider(names_from = metric, 
                values_from = value) %>%
    mutate(age_group = "15-64")
  
  total_15to64_summary_stats_raw <- data.frame(
    metric = c("total_infections", "total_vaccinated", "gp_attendance", "noro_hospitalisation", "noro_mortality", "aki_hospitalisation", "aki_mortality"),
    mean = colMeans(total_15to64_results_df),
    median = apply(total_15to64_results_df, 2, median),
    lower_ci = apply(total_15to64_results_df, 2, quantile, probs = 0.025),
    upper_ci = apply(total_15to64_results_df, 2, quantile, probs = 0.975)
  )
  
  total_over65_summary_stats <- data.frame(
    metric = c("total_infections", "total_vaccinated", "gp_attendance", "noro_hospitalisation", "noro_mortality", "aki_hospitalisation", "aki_mortality"),
    mean = colMeans(total_over65_results_df),
    median = apply(total_over65_results_df, 2, median),
    lower_ci = apply(total_over65_results_df, 2, quantile, probs = 0.025),
    upper_ci = apply(total_over65_results_df, 2, quantile, probs = 0.975)
  ) %>% 
    mutate(value = paste0(formatC(round(mean), format="f", big.mark=",", digits=0), " (",
                          formatC(round(lower_ci), format="f", big.mark=",", digits=0), "-",
                          formatC(round(upper_ci), format="f", big.mark=",", digits=0), ")")) %>% 
    select(metric, value) %>%
    pivot_wider(names_from = metric, 
                values_from = value) %>%
    mutate(age_group = "65+")
  
  total_over65_summary_stats_raw <- data.frame(
    metric = c("total_infections", "total_vaccinated", "gp_attendance", "noro_hospitalisation", "noro_mortality", "aki_hospitalisation", "aki_mortality"),
    mean = colMeans(total_over65_results_df),
    median = apply(total_over65_results_df, 2, median),
    lower_ci = apply(total_over65_results_df, 2, quantile, probs = 0.025),
    upper_ci = apply(total_over65_results_df, 2, quantile, probs = 0.975)
  )
  
  total_summary_stats <- rbind(total_under5_summary_stats, total_5to14_summary_stats, total_15to64_summary_stats, total_over65_summary_stats)
  
  return(list(
    total_summary_stats = total_summary_stats,
    total_under5_summary_stats_raw = total_under5_summary_stats_raw,
    total_5to14_summary_stats_raw = total_5to14_summary_stats_raw,
    total_15to64_summary_stats_raw = total_15to64_summary_stats_raw,
    total_over65_summary_stats_raw = total_over65_summary_stats_raw
  ))
  
  
}

no_vaccination <- read_parquet("data/no_vaccination.parquet")
no_vaccination_total_health_outcomes <- total_health_outcomes_procees(no_vaccination, n_iterations = n_iter)
remove(no_vaccination)
gc()

under_5_vaccination <- read_parquet("data/under_5_vaccination.parquet")
under5_vaccination_total_health_outcomes <- total_health_outcomes_procees(under_5_vaccination, n_iterations = n_iter)
remove(under_5_vaccination)
gc()

over_65_vaccination <- read_parquet("data/over65_vaccination.parquet")
over65_vaccination_total_health_outcomes <- total_health_outcomes_procees(over_65_vaccination, n_iterations = n_iter)
remove(over_65_vaccination)
gc()

combo_vaccination <- read_parquet("data/under5_over65_vaccination.parquet")
combo_vaccination_total_health_outcomes <- total_health_outcomes_procees(combo_vaccination, n_iterations = n_iter)
remove(combo_vaccination)
gc()

combined_age_group_total_table <- no_vaccination_total_health_outcomes$total_summary_stats %>% 
  bind_rows(under5_vaccination_total_health_outcomes$total_summary_stats, over65_vaccination_total_health_outcomes$total_summary_stats, combo_vaccination_total_health_outcomes$total_summary_stats) %>%
  mutate(scenario = NA) %>% 
  add_row(.before = 1) %>%
  mutate(scenario = case_when(is.na(scenario) ~ "No vaccination",
                              TRUE ~ scenario)) %>%
  add_row(.before = 6) %>%
  mutate(scenario = case_when(is.na(scenario) ~ "V1 under 5",
                              TRUE ~ scenario)) %>% 
  add_row(.before = 11) %>%
  mutate(scenario = case_when(is.na(scenario) ~ "V2 over 65",
                              TRUE ~ scenario)) %>% 
  add_row(.before = 16) %>%
  mutate(scenario = case_when(is.na(scenario) ~ "V3 under 5 and over 65",
                              TRUE ~ scenario)) %>%
  mutate(scenario = case_when(
    !is.na(total_infections) ~ NA,
    TRUE ~ scenario)) %>% 
  select(scenario, age_group, total_vaccinated, everything()) %>% 
  rename(
    "Strategy" = scenario,
    "Age group" = age_group,
    "Infections total" = total_infections,
    "GP attendance" = gp_attendance,
    "Norovirus hospitalisation" = noro_hospitalisation,
    "AKI hospitalisation" = aki_hospitalisation,
    "Norovirus mortality" = noro_mortality,
    "AKI mortality" = aki_mortality,
    "Total vaccinated" = total_vaccinated
  ) %>% 
  flextable::flextable() %>%
  autofit() %>%
  theme_zebra() %>%
  fontsize(size = 11, part = "all") %>%
  bold(i = c(1, 6, 11, 16)) %>% 
  padding(padding = 3, part = "all") %>%
  hline(i = c(5, 10, 15)) %>% 
  align(align = "left", part = "all") %>%
  align(align = "right", j = 2:9, part = "body")

#### calculating incidence outcomes for no vaccination

#' No vaccination totals over 10 years

no_vaccination_outcomes_under5 <- no_vaccination_total_health_outcomes$total_under5_summary_stats_raw |> 
  mutate(value = paste0(formatC((mean/10)/demography[1]*1000, format="f", big.mark=",", digits=1), " (",
                        formatC((lower_ci/10)/demography[1]*1000, format="f", big.mark=",", digits=1), "-",
                        formatC((upper_ci/10)/demography[1]*1000, format="f", big.mark=",", digits=1), ")")) %>% 
  select(metric, value) %>%
  pivot_wider(names_from = metric, 
              values_from = value) %>%
  mutate(age_group = "0-4")

no_vaccination_outcomes_5to14 <- no_vaccination_total_health_outcomes$total_5to14_summary_stats_raw |>
  mutate(value = paste0(formatC((mean/10)/demography[2]*1000, format="f", big.mark=",", digits=1), " (",
                        formatC((lower_ci/10)/demography[2]*1000, format="f", big.mark=",", digits=1), "-",
                        formatC((upper_ci/10)/demography[2]*1000, format="f", big.mark=",", digits=1), ")")) %>% 
  select(metric, value) %>%
  pivot_wider(names_from = metric, 
              values_from = value) %>%
  mutate(age_group = "5-14")

no_vaccination_outcomes_15to64 <- no_vaccination_total_health_outcomes$total_15to64_summary_stats_raw |>
  mutate(value = paste0(formatC(mean/10/demography[3]*1000, format="f", big.mark=",", digits=1), " (",
                        formatC(lower_ci/10/demography[3]*1000, format="f", big.mark=",", digits=1), "-",
                        formatC(upper_ci/10/demography[3]*1000, format="f", big.mark=",", digits=1), ")")) %>% 
  select(metric, value) %>%
  pivot_wider(names_from = metric, 
              values_from = value) %>%
  mutate(age_group = "15-64")

no_vaccination_outcomes_over65 <- no_vaccination_total_health_outcomes$total_over65_summary_stats_raw |>
  mutate(value = paste0(formatC(mean/10/demography[4]*1000, format="f", big.mark=",", digits=1), " (",
                        formatC(lower_ci/10/demography[4]*1000, format="f", big.mark=",", digits=1), "-",
                        formatC(upper_ci/10/demography[4]*1000, format="f", big.mark=",", digits=1), ")")) %>% 
  select(metric, value) %>%
  pivot_wider(names_from = metric, 
              values_from = value) %>%
  mutate(age_group = "65+")

no_vaccination_incidence_table <- no_vaccination_outcomes_under5 %>% 
  bind_rows(no_vaccination_outcomes_5to14, no_vaccination_outcomes_15to64, no_vaccination_outcomes_over65)

####

# Create a function to calculate incidence with confidence intervals using dplyr
calculate_incidence_data <- function(outcomes, demography, scale_factor = 10, per_population = 1000) {
  # Define age groups and outcome types
  age_groups <- c("total_under5_summary_stats_raw", 
                  "total_5to14_summary_stats_raw", 
                  "total_15to64_summary_stats_raw", 
                  "total_over65_summary_stats_raw")
  
  outcome_types <- tibble(
    Outcome_Type = c("Total", "GP_Total"),
    row_index = c(1, 3)
  )
  
  # Function to extract values from the outcomes list for a specific row and column
  extract_values <- function(row_idx, col_idx) {
    age_groups %>%
      sapply(function(age_group) {
        outcomes[[age_group]][row_idx, col_idx]
      }) %>%
      sum()
  }
  
  # Calculate incidence for each outcome type and statistic
  result_table <- outcome_types %>%
    rowwise() %>%
    mutate(
      # Calculate point estimate
      point_estimate = extract_values(row_index, 3),
      lower_ci = extract_values(row_index, 4),
      upper_ci = extract_values(row_index, 5)
    ) %>%
    ungroup() %>%
    # Scale and normalize the values
    mutate(
      total_population = sum(demography),
      Incidence = (point_estimate / scale_factor) / total_population * per_population,
      Lower_CI = (lower_ci / scale_factor) / total_population * per_population,
      Upper_CI = (upper_ci / scale_factor) / total_population * per_population,
      Formatted = sprintf("%.3f (%.3f-%.3f)", Incidence, Lower_CI, Upper_CI)
    ) %>%
    # Select only the needed columns
    select(Outcome_Type, Incidence, Lower_CI, Upper_CI, Formatted)
  
  return(result_table)
}

# Calculate incidence data
incidence_table <- calculate_incidence_data(
  outcomes = no_vaccination_total_health_outcomes,
  demography = demography
)

# Print table in a nice format
incidence_table %>%
  select(Outcome_Type, Incidence, Lower_CI, Upper_CI) %>%
  kable(digits = 3,
        caption = "Incidence Rates per 1000 Population",
        col.names = c("Outcome Type", "Incidence", "Lower 95% CI", "Upper 95% CI"))
