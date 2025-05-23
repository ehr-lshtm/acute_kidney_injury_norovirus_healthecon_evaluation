# full incremental analysis

######### NO AKI Outcome

no_aki_cea_under5 <- cea_dynamic_model_probabilistic(data = averted_under5_vaccination,
                                                     n_iterations = n_iter,
                                                     parameter_probabilistic_samples = parameter_probabilistic_samples,
                                                     aki_no_outcome = TRUE)

no_aki_cea_under5 <- as.data.frame(no_aki_cea_under5$summary_stats) %>% 
  rename(mean_A = mean,
         median_A = median,
         lower_ci_A = lower_ci,
         upper_ci_A = upper_ci)

no_aki_cea_over65 <- cea_dynamic_model_probabilistic(data = averted_over65_vaccination,
                                                     n_iterations = n_iter,
                                                     parameter_probabilistic_samples = parameter_probabilistic_samples,
                                                     aki_no_outcome = TRUE)

no_aki_cea_over65 <- as.data.frame(no_aki_cea_over65$summary_stats) %>% 
  rename(mean_B = mean,
         median_B = median,
         lower_ci_B = lower_ci,
         upper_ci_B = upper_ci)

no_aki_cea_under5_over65 <- cea_dynamic_model_probabilistic(data = averted_combo_vaccination,
                                                            n_iterations = n_iter,
                                                            parameter_probabilistic_samples = parameter_probabilistic_samples,
                                                            aki_no_outcome = TRUE)

no_aki_cea_under5_over65 <- as.data.frame(no_aki_cea_under5_over65$summary_stats) %>% 
  rename(mean_C = mean,
         median_C = median,
         lower_ci_C = lower_ci,
         upper_ci_C = upper_ci)

no_aki_part_1 <- no_aki_cea_under5 %>% 
  left_join(no_aki_cea_over65, by = "metric") %>% 
  left_join(no_aki_cea_under5_over65, by = "metric") %>%
  filter(metric != "ICER") |> 
  pivot_longer(cols = -metric, names_to = "stat", values_to = "value") |> 
  pivot_wider(names_from = metric, values_from = value) |> 
  mutate(ICER = `Incremental Cost` / `QALY Gain`) |> 
  pivot_longer(cols = -stat, names_to = "metric", values_to = "value") |> 
  pivot_wider(names_from = stat, values_from = value) |> 
  mutate(
    mean_BvA = mean_B - mean_A,
    mean_CvB = mean_C - mean_B,
    mean_CvA = mean_C - mean_A,
    median_BvA = median_B - median_A,
    median_CvB = median_C - median_B,
    median_CvA = median_C - median_A,
    lower_ci_BvA = lower_ci_B - lower_ci_A,
    lower_ci_CvB = lower_ci_C - lower_ci_B,
    lower_ci_CvA = lower_ci_C - lower_ci_A,
    upper_ci_BvA = upper_ci_B - upper_ci_A,
    upper_ci_CvB = upper_ci_C - upper_ci_B,
    upper_ci_CvA = upper_ci_C - upper_ci_A
  ) %>% 
  mutate(across(where(is.numeric), ~scales::comma(round(., 0)))) %>%
  mutate(
    A = sprintf("%s (%s - %s)", mean_A, lower_ci_A, upper_ci_A),
    B = sprintf("%s (%s - %s)", mean_B, lower_ci_B, upper_ci_B),
    C = sprintf("%s (%s - %s)", mean_C, lower_ci_C, upper_ci_C),
    BvA = sprintf("%s (%s - %s)", mean_BvA, lower_ci_BvA, upper_ci_BvA),
    CvB = sprintf("%s (%s - %s)", mean_CvB, lower_ci_CvB, upper_ci_CvB),
    CvA = sprintf("%s (%s - %s)", mean_CvA, lower_ci_CvA, upper_ci_CvA)
  ) %>% 
  select(metric, A, B, C, BvA, CvB, CvA) %>% 
  pivot_longer(cols = -metric, names_to = "scenario", values_to = "value") %>% 
  pivot_wider(names_from = metric, values_from = value) %>% 
  filter(scenario == "BvA" | scenario == "CvB") %>% 
  select(scenario, ICER) %>% 
  mutate(scenario = case_when(scenario == "BvA" ~ "B",
                              scenario == "CvB" ~ "C")) %>% 
  rename(Incremental_cost_effectiveness_ratio = ICER)

no_aki_part_2 <- no_aki_cea_under5 %>% 
  left_join(no_aki_cea_over65, by = "metric") %>% 
  left_join(no_aki_cea_under5_over65, by = "metric") %>%
  filter(metric != "ICER") |> 
  pivot_longer(cols = -metric, names_to = "stat", values_to = "value") |> 
  pivot_wider(names_from = metric, values_from = value) |> 
  mutate(ICER = `Incremental Cost` / `QALY Gain`) |> 
  pivot_longer(cols = -stat, names_to = "metric", values_to = "value") |> 
  pivot_wider(names_from = stat, values_from = value) |> 
  mutate(across(where(is.numeric), ~if_else(metric == "Total cost", 
                                            ./1000000, 
                                            .))) %>% 
  mutate(across(where(is.numeric), ~if_else(metric == "Incremental Cost", 
                                            ./1000000, 
                                            .))) %>% 
  mutate(across(where(is.numeric), ~if_else(metric == "QALY Gain", 
                                            ./1000, 
                                            .))) %>% 
  mutate(across(where(is.numeric), ~if_else(metric == "INMB", 
                                            ./1000000, 
                                            .))) %>% 
  mutate(across(where(is.numeric), ~scales::comma(round(., 0)))) %>%
  mutate(
    A = sprintf("%s (%s - %s)", mean_A, lower_ci_A, upper_ci_A),
    B = sprintf("%s (%s - %s)", mean_B, lower_ci_B, upper_ci_B),
    C = sprintf("%s (%s - %s)", mean_C, lower_ci_C, upper_ci_C)
  ) %>% 
  select(metric, A, B, C) %>% 
  pivot_longer(cols = -metric, names_to = "scenario", values_to = "value") %>% 
  pivot_wider(names_from = metric, values_from = value)

no_aki_part_3 <- no_aki_part_2 %>% 
  left_join(no_aki_part_1, by = "scenario") %>%
  mutate(Incremental_cost_effectiveness_ratio = case_when(
    scenario == "A" ~ ICER,
    scenario == "B" ~ "dominated",
    TRUE ~ as.character(Incremental_cost_effectiveness_ratio)
  ))

######### with AKI Outcome


cea_under5 <- cea_dynamic_model_probabilistic(data = averted_under5_vaccination,
                                              n_iterations = n_iter,
                                              parameter_probabilistic_samples = parameter_probabilistic_samples)

cea_under5 <- as.data.frame(cea_under5$summary_stats) %>% 
  rename(mean_A = mean,
         median_A = median,
         lower_ci_A = lower_ci,
         upper_ci_A = upper_ci)

cea_over65 <- cea_dynamic_model_probabilistic(data = averted_over65_vaccination,
                                              n_iterations = n_iter,
                                              parameter_probabilistic_samples = parameter_probabilistic_samples)

cea_over65 <- as.data.frame(cea_over65$summary_stats) %>% 
  rename(mean_B = mean,
         median_B = median,
         lower_ci_B = lower_ci,
         upper_ci_B = upper_ci)

cea_under5_over65 <- cea_dynamic_model_probabilistic(data = averted_combo_vaccination,
                                                     n_iterations = n_iter,
                                                     parameter_probabilistic_samples = parameter_probabilistic_samples)

cea_under5_over65 <- as.data.frame(cea_under5_over65$summary_stats) %>% 
  rename(mean_C = mean,
         median_C = median,
         lower_ci_C = lower_ci,
         upper_ci_C = upper_ci)

with_aki_part_1 <- cea_under5 |>  
  left_join(cea_over65, by = "metric") |>  
  left_join(cea_under5_over65, by = "metric") |> 
  filter(metric != "ICER") |> 
  pivot_longer(cols = -metric, names_to = "stat", values_to = "value") |> 
  pivot_wider(names_from = metric, values_from = value) |> 
  mutate(ICER = `Incremental Cost` / `QALY Gain`) |> 
  pivot_longer(cols = -stat, names_to = "metric", values_to = "value") |> 
  pivot_wider(names_from = stat, values_from = value) |> 
  mutate(
    mean_BvA = mean_B - mean_A,
    mean_CvB = mean_C - mean_B,
    mean_CvA = mean_C - mean_A,
    median_BvA = median_B - median_A,
    median_CvB = median_C - median_B,
    median_CvA = median_C - median_A,
    lower_ci_BvA = lower_ci_B - lower_ci_A,
    lower_ci_CvB = lower_ci_C - lower_ci_B,
    lower_ci_CvA = lower_ci_C - lower_ci_A,
    upper_ci_BvA = upper_ci_B - upper_ci_A,
    upper_ci_CvB = upper_ci_C - upper_ci_B,
    upper_ci_CvA = upper_ci_C - upper_ci_A
  ) %>% 
  mutate(across(where(is.numeric), ~scales::comma(round(., 0)))) %>% 
  mutate(
    A = sprintf("%s (%s - %s)", mean_A, lower_ci_A, upper_ci_A),
    B = sprintf("%s (%s - %s)", mean_B, lower_ci_B, upper_ci_B),
    C = sprintf("%s (%s - %s)", mean_C, lower_ci_C, upper_ci_C),
    BvA = sprintf("%s (%s - %s)", mean_BvA, lower_ci_BvA, upper_ci_BvA),
    CvB = sprintf("%s (%s - %s)", mean_CvB, lower_ci_CvB, upper_ci_CvB),
    CvA = sprintf("%s (%s - %s)", mean_CvA, lower_ci_CvA, upper_ci_CvA)
  ) %>% 
  select(metric, A, B, C, BvA, CvB, CvA) %>% 
  pivot_longer(cols = -metric, names_to = "scenario", values_to = "value") %>% 
  pivot_wider(names_from = metric, values_from = value) %>% 
  filter(scenario == "BvA" | scenario == "CvB") %>% 
  select(scenario, ICER) %>% 
  mutate(scenario = case_when(scenario == "BvA" ~ "B",
                              scenario == "CvB" ~ "C")) %>% 
  rename(Incremental_cost_effectiveness_ratio = ICER)

with_aki_part_2 <- cea_under5 %>% 
  left_join(cea_over65, by = "metric") %>% 
  left_join(cea_under5_over65, by = "metric") %>%
  filter(metric != "ICER") |> 
  pivot_longer(cols = -metric, names_to = "stat", values_to = "value") |> 
  pivot_wider(names_from = metric, values_from = value) |> 
  mutate(ICER = `Incremental Cost` / `QALY Gain`) |> 
  pivot_longer(cols = -stat, names_to = "metric", values_to = "value") |> 
  pivot_wider(names_from = stat, values_from = value) |> 
  mutate(across(where(is.numeric), ~if_else(metric == "Total cost", 
                                            ./1000000, 
                                            .))) %>% 
  mutate(across(where(is.numeric), ~if_else(metric == "Incremental Cost", 
                                            ./1000000, 
                                            .))) %>% 
  mutate(across(where(is.numeric), ~if_else(metric == "QALY Gain", 
                                            ./1000, 
                                            .))) %>% 
  mutate(across(where(is.numeric), ~if_else(metric == "INMB", 
                                            ./1000000, 
                                            .))) %>% 
  mutate(across(where(is.numeric), ~scales::comma(round(., 0)))) %>%
  mutate(
    A = sprintf("%s (%s - %s)", mean_A, lower_ci_A, upper_ci_A),
    B = sprintf("%s (%s - %s)", mean_B, lower_ci_B, upper_ci_B),
    C = sprintf("%s (%s - %s)", mean_C, lower_ci_C, upper_ci_C)
  ) %>% 
  select(metric, A, B, C) %>% 
  pivot_longer(cols = -metric, names_to = "scenario", values_to = "value") %>% 
  pivot_wider(names_from = metric, values_from = value)

with_aki_part_3 <- with_aki_part_2 %>% 
  left_join(with_aki_part_1, by = "scenario") %>%
  mutate(Incremental_cost_effectiveness_ratio = case_when(
    scenario == "A" ~ ICER,
    scenario == "B" ~ "dominated",
    TRUE ~ as.character(Incremental_cost_effectiveness_ratio)
  ))

### combined table

combined_full_incremental_table <- with_aki_part_3 %>% 
  rbind(no_aki_part_3) %>%
  mutate(scenario = case_when(
    scenario == "A" ~ "V1 under 5",
    scenario == "B" ~ "V2 over 65",
    scenario == "C" ~ "V3 under and 65+",
  )) %>%
  mutate(Strategy = scenario) %>% 
  add_row(.before = 1) %>% 
  add_row(.before = 5) %>%
  mutate(scenario = case_when(
    row_number() == 1 ~ "Included" ,
    row_number() == 5 ~ "Excluded")
  ) %>% 
  select(scenario, Strategy, `Incremental Cost`, `QALY Gain`, `INMB`, ICER, Incremental_cost_effectiveness_ratio) %>%
  rename(
    "Acute kidney injury\noutcomes" = scenario,
    "Net cost (£ million )" = `Incremental Cost`,
    "Total QALYs gained (million)" = `QALY Gain`,
    "Net monetary benefit (£ million)" = `INMB`,
    "Average incremental\ncost-effectiveness\nratio (£/QALY)" = ICER,
    "Incremental\ncost-effectiveness\nratio (£/QALY)" = Incremental_cost_effectiveness_ratio
  ) %>% 
  flextable::flextable() %>%
  width(width = c(1.0, 1.3, 2.0, 1.5, 2.0, 2.0, 2.0)) %>%
  theme_zebra() %>%
  fontsize(size = 10, part = "all") %>%
  bold(i = c(1, nrow(with_aki_part_3) + 2)) %>%
  padding(padding = 3, part = "all") %>%
  hline(i = c(nrow(with_aki_part_3)+1)) %>%
  align(align = "left", part = "all") %>%
  align(align = "right", j = 2:7, part = "body") %>%
  # Center align headers
  align(align = "left", part = "header") %>%
  # Add some vertical space in header
  height_all(height = 0.6, part = "header")

combined_full_incremental_table
