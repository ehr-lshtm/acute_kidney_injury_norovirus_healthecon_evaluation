
no_vaccination
under_5_vaccination <- read_parquet("data/under_5_vaccination.parquet")

mean_no_vaccination
mean_under_5_vaccination
mean_averted_under5_vaccination |> 
  select(season, averted_under5)

mean_under_5_vaccination |> 
  summarise(total = sum(total_Is1_vacc_u5, na.rm = TRUE))

no_vaccination_uncertainty <- process_vaccination_uncertainty_data("data/no_vaccination_uncertainty.parquet")
under_5_vaccination_uncertainty <- process_vaccination_uncertainty_data("data/under_5_vaccination_uncertainty.parquet")

under_5_vaccination_uncertainty |> 
  select(percentile_50_noro_1, season) |> 
  filter(season != "-S1") |> 
  group_by(season) |>
  summarise(total = sum(percentile_50_noro_1, na.rm = TRUE)) |>
  ungroup()

averted_under5_vaccination

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

cea_one_way_analysis_function(data = psa_mean_averted_under5_vaccination, aki_no_outcome = FALSE, icer_only = TRUE )
cea_one_way_analysis_function(data = psa_mean_averted_over65_vaccination, aki_no_outcome = FALSE, icer_only = TRUE )
cea_one_way_analysis_function(data = psa_mean_averted_combo_vaccination, aki_no_outcome = FALSE, icer_only = TRUE )

cea_one_way_analysis_function(data = psa_mean_averted_under5_vaccination, aki_no_outcome = TRUE, icer_only = TRUE )
cea_one_way_analysis_function(data = psa_mean_averted_over65_vaccination, aki_no_outcome = TRUE, icer_only = TRUE )
cea_one_way_analysis_function(data = psa_mean_averted_combo_vaccination, aki_no_outcome = TRUE, icer_only = TRUE )

one_way_mean <- cea_one_way_analysis_function(data = psa_mean_averted_under5_vaccination, aki_no_outcome = FALSE, icer_only = FALSE )
one_way <- as.data.frame(one_way_mean)



under_5_vaccination_uncertainty |> 
  select(percentile_50_noro_1, season) |>
  filter(season != "-S1") |> 
  summarise(total = sum(percentile_50_noro_1, na.rm = TRUE))
  
under_5_vaccination |> 
  group_by(season) |>
  summarise(mean_season_total = mean(total_under5_vaccinated, na.rm = TRUE)) |>
  ungroup()

mean_under_5_vaccination |> 
  select(season, total_under5_vaccinated)

under_5_vaccination |> 
  group_by(season) |>
  summarise(mean_season_total = mean(total_Is1_vacc_u5, na.rm = TRUE)) |>
  ungroup()

no_vaccination_uncertainty |> 
  select(percentile_50_noro_1, season) |> 
  filter(season != "-S1") |> 
  group_by(season) |>
  summarise(total = sum(percentile_50_noro_1, na.rm = TRUE)) |>
  ungroup()

####

# distribution of incremtnal costs


cea_under5 <- cea_dynamic_model_probabilistic(data = averted_under5_vaccination,
                                              n_iterations = n_iter,
                                              parameter_probabilistic_samples = parameter_probabilistic_samples)

cea_under5_vacc <- as.data.frame(cea_under5$raw_results)

test$incr_cost
psa <- cea_under5$summary_stats
View(psa)
psa |> 
  filter(metric == "averted_cost_aki_hosp_all_ages")

cea_one_way_analysis_function(data = averted_under5_vaccination, aki_no_outcome = FALSE, icer_only = FALSE )
one_way_mean <- cea_one_way_analysis_function(data = mean_averted_under5_vaccination, aki_no_outcome = FALSE, icer_only = FALSE )
one_way <- as.data.frame(one_way_mean)
one_way |>
  slice(13)

par(mfrow = c(3, 1))
hist(cea_under5_vacc$incr_cost, breaks = 200)
hist(cea_under5_vacc$qaly_gain, breaks = 200)
hist(cea_under5_vacc$icer, breaks = 200)

mean(test$icer)
median(test$icer)
mode(test$icer)

mean(test$incr_cost) / mean(test$qaly_gain)

hist(test$qaly_gain, breaks = 100)

hist(test$program_cost, breaks = 100)
hist(test$admin_cost, breaks = 100)
hist(test$vaccine_cost, breaks = 100)

hist(test$averted_cost_aki_hosp_all_ages, breaks = 100)
hist(test$averted_cost_noro_hosp_all_ages, breaks = 100)
hist(test$averted_cost_noro_gp_all_ages, breaks = 100)

hist(parameter_probabilistic_samples$probabilistic$costs$aki_hosp, breaks = 100)
hist(parameter_probabilistic_samples$probabilistic$aki_hosp_parameters$aki_hosp_parameter_4, breaks = 100)

####

one_way_mean_icer <- cea_one_way_analysis_function(data = mean_averted_under5_vaccination, aki_no_outcome = FALSE)

df_mean <- tibble(id = 1) %>%
  mutate(
    base_sigma = mean(parameter_probabilistic_samples$probabilistic$infection_parameters$sigma),
    D_immun = mean(parameter_probabilistic_samples$probabilistic$seasonality_parameters$D_immun),
    probT_under5 = log(mean(parameter_probabilistic_samples$probabilistic$infection_parameters$probT_under5)),
    probT_over5 = log(mean(parameter_probabilistic_samples$probabilistic$infection_parameters$probT_over5)),
    season_amp = mean(parameter_probabilistic_samples$probabilistic$seasonality_parameters$season_amp),
    season_offset = c(mean(parameter_probabilistic_samples$probabilistic$seasonality_parameters$season_offset)),
    gp_parameter_1 =  mean(parameter_probabilistic_samples$probabilistic$gp_parameters$gp_parameter_1),
    noro_hosp_parameter_1 = mean(
      parameter_probabilistic_samples$probabilistic$noro_hosp_parameters$noro_hosp_parameter_1
    ),
    noro_morality_parameter_1 = mean(
      parameter_probabilistic_samples$probabilistic$noro_mortality_parameters$noro_mortality_parameter_1
    ),
    aki_hosp_parameter_1 = params[["aki_hospitalisation_1"]],
    aki_mortality_parameter_1 = mean(
      parameter_probabilistic_samples$probabilistic$aki_mortality_parameters$aki_mortality_parameter_1
    ),
    gp_parameter_2 = mean(
      parameter_probabilistic_samples$probabilistic$gp_parameters$gp_parameter_2
    ),
    noro_hosp_parameter_2 = mean(
      parameter_probabilistic_samples$probabilistic$noro_hosp_parameters$noro_hosp_parameter_2
    ),
    noro_morality_parameter_2 = mean(
      parameter_probabilistic_samples$probabilistic$noro_mortality_parameters$noro_mortality_parameter_2
    ),
    aki_hosp_parameter_2 = params[["aki_hospitalisation_2"]],
    aki_mortality_parameter_2 = mean(
      parameter_probabilistic_samples$probabilistic$aki_mortality_parameters$aki_mortality_parameter_2
    ),
    gp_parameter_3 = mean(
      parameter_probabilistic_samples$probabilistic$gp_parameters$gp_parameter_3
    ),
    noro_hosp_parameter_3 = mean(
      parameter_probabilistic_samples$probabilistic$noro_hosp_parameters$noro_hosp_parameter_3
    ),
    noro_morality_parameter_3 = mean(
      parameter_probabilistic_samples$probabilistic$noro_mortality_parameters$noro_mortality_parameter_3
    ),
    aki_hosp_parameter_3 = params[["aki_hospitalisation_3"]],
    aki_mortality_parameter_3 = mean(
      parameter_probabilistic_samples$probabilistic$aki_mortality_parameters$aki_mortality_parameter_3
    ),
    gp_parameter_4 = mean(
      parameter_probabilistic_samples$probabilistic$gp_parameters$gp_parameter_4
    ),
    noro_hosp_parameter_4 = mean(
      parameter_probabilistic_samples$probabilistic$noro_hosp_parameters$noro_hosp_parameter_4
    ),
    noro_morality_parameter_4 = mean(
      parameter_probabilistic_samples$probabilistic$noro_mortality_parameters$noro_mortality_parameter_4
    ),
    aki_hosp_parameter_4 = mean(
      parameter_probabilistic_samples$probabilistic$aki_hosp_parameters$aki_hosp_parameter_4
    ),
    aki_mortality_parameter_4 = mean(
      parameter_probabilistic_samples$probabilistic$aki_mortality_parameters$aki_mortality_parameter_4
    ),
    admin_per_dose_child = mean(parameter_probabilistic_samples$probabilistic$costs$admin_per_dose_1),
    admin_per_dose_adult = mean(parameter_probabilistic_samples$probabilistic$costs$admin_per_dose_2),
    cost_per_child_noro_hospitalisation_episode = mean(parameter_probabilistic_samples$probabilistic$costs$norovirus_hosp_1),
    cost_per_adult_noro_hospitalisation_episode =  mean(parameter_probabilistic_samples$probabilistic$costs$norovirus_hosp_2),
    cost_noro_gp_attendance = mean(parameter_probabilistic_samples$probabilistic$costs$gp_attendance),
    cost_per_aki_hospitalisation_episode = mean(parameter_probabilistic_samples$probabilistic$costs$aki_hosp),
    qaly_noro_gp_attendance = mean(parameter_probabilistic_samples$probabilistic$qalys$gp_attendance),
    qaly_noro_hospitalisation = mean(parameter_probabilistic_samples$probabilistic$qalys$norovirus_hosp),
    qaly_aki_hospitalisation = mean(parameter_probabilistic_samples$probabilistic$qalys$aki_hosp)
  ) |> 
  pivot_longer(
    cols = everything(),  # Select all columns
    names_to = "parameter",  # Column name for the parameter names
    values_to = "value_mean"  # Column name for the parameter values
  )

df_mean

df_median <- tibble(id = 1) %>%
  mutate(
    base_sigma = median(parameter_probabilistic_samples$probabilistic$infection_parameters$sigma),
    D_immun = median(parameter_probabilistic_samples$probabilistic$seasonality_parameters$D_immun),
    probT_under5 = log(median(parameter_probabilistic_samples$probabilistic$infection_parameters$probT_under5)),
    probT_over5 = log(median(parameter_probabilistic_samples$probabilistic$infection_parameters$probT_over5)),
    season_amp = median(parameter_probabilistic_samples$probabilistic$seasonality_parameters$season_amp),
    season_offset = c(median(parameter_probabilistic_samples$probabilistic$seasonality_parameters$season_offset)),
    gp_parameter_1 =  median(
      parameter_probabilistic_samples$probabilistic$gp_parameters$gp_parameter_1
    ),
    noro_hosp_parameter_1 = median(
      parameter_probabilistic_samples$probabilistic$noro_hosp_parameters$noro_hosp_parameter_1
    ),
    noro_morality_parameter_1 = median(
      parameter_probabilistic_samples$probabilistic$noro_mortality_parameters$noro_mortality_parameter_1
    ),
    aki_hosp_parameter_1 = params[["aki_hospitalisation_1"]],
    aki_mortality_parameter_1 = median(
      parameter_probabilistic_samples$probabilistic$aki_mortality_parameters$aki_mortality_parameter_1
    ),
    gp_parameter_2 = median(
      parameter_probabilistic_samples$probabilistic$gp_parameters$gp_parameter_2
    ),
    noro_hosp_parameter_2 = median(
      parameter_probabilistic_samples$probabilistic$noro_hosp_parameters$noro_hosp_parameter_2
    ),
    noro_morality_parameter_2 = median(
      parameter_probabilistic_samples$probabilistic$noro_mortality_parameters$noro_mortality_parameter_2
    ),
    aki_hosp_parameter_2 = params[["aki_hospitalisation_2"]],
    aki_mortality_parameter_2 = median(
      parameter_probabilistic_samples$probabilistic$aki_mortality_parameters$aki_mortality_parameter_2
    ),
    gp_parameter_3 = median(
      parameter_probabilistic_samples$probabilistic$gp_parameters$gp_parameter_3
    ),
    noro_hosp_parameter_3 = median(
      parameter_probabilistic_samples$probabilistic$noro_hosp_parameters$noro_hosp_parameter_3
    ),
    noro_morality_parameter_3 = median(
      parameter_probabilistic_samples$probabilistic$noro_mortality_parameters$noro_mortality_parameter_3
    ),
    aki_hosp_parameter_3 = params[["aki_hospitalisation_3"]],
    aki_mortality_parameter_3 = median(
      parameter_probabilistic_samples$probabilistic$aki_mortality_parameters$aki_mortality_parameter_3
    ),
    gp_parameter_4 = median(
      parameter_probabilistic_samples$probabilistic$gp_parameters$gp_parameter_4
    ),
    noro_hosp_parameter_4 = median(
      parameter_probabilistic_samples$probabilistic$noro_hosp_parameters$noro_hosp_parameter_4
    ),
    noro_morality_parameter_4 = median(
      parameter_probabilistic_samples$probabilistic$noro_mortality_parameters$noro_mortality_parameter_4
    ),
    aki_hosp_parameter_4 = median(
      parameter_probabilistic_samples$probabilistic$aki_hosp_parameters$aki_hosp_parameter_4
    ),
    aki_mortality_parameter_4 = median(
      parameter_probabilistic_samples$probabilistic$aki_mortality_parameters$aki_mortality_parameter_4
    ),
    admin_per_dose_child = median(parameter_probabilistic_samples$probabilistic$costs$admin_per_dose_1),
    admin_per_dose_adult = median(parameter_probabilistic_samples$probabilistic$costs$admin_per_dose_2),
    cost_per_child_noro_hospitalisation_episode = median(parameter_probabilistic_samples$probabilistic$costs$norovirus_hosp_1),
    cost_per_adult_noro_hospitalisation_episode =  median(parameter_probabilistic_samples$probabilistic$costs$norovirus_hosp_2),
    cost_noro_gp_attendance = median(parameter_probabilistic_samples$probabilistic$costs$gp_attendance),
    cost_per_aki_hospitalisation_episode = median(parameter_probabilistic_samples$probabilistic$costs$aki_hosp),
    qaly_noro_gp_attendance = median(parameter_probabilistic_samples$probabilistic$qalys$gp_attendance),
    qaly_noro_hospitalisation = median(parameter_probabilistic_samples$probabilistic$qalys$norovirus_hosp),
    qaly_aki_hospitalisation = median(parameter_probabilistic_samples$probabilistic$qalys$aki_hosp)
  ) |> 
  pivot_longer(
    cols = everything(),  # Select all columns
    names_to = "parameter",  # Column name for the parameter names
    values_to = "value_median"  # Column name for the parameter values
  )


test <- df_median |> 
  left_join(df_mean, by = "parameter")

View(test)

df_trace_mean <- tibble(id = 2) |> 
  mutate(
    sigma = mean(traceBurnThin_df$sigma),
    D_immun = mean(traceBurnThin_df$D_immun),
    probT_under5 = log(mean(traceBurnThin_df$probT_under5)),
    probT_over5 = log(mean(traceBurnThin_df$probT_over5)),
    season_amp = mean(traceBurnThin_df$season_amp)*100,
    season_offset = c(mean(traceBurnThin_df$season_offset))*100,
    gp_parameter_1 =  mean(traceBurnThin_df$gastro_gp_attend_1),
  ) |> 
  pivot_longer(
    cols = everything(),  # Select all columns
    names_to = "parameter",  # Column name for the parameter names
    values_to = "value"  # Column name for the parameter values
  )

df_trace_mean


lower_aki_hosp_cost <- cea_one_way_analysis_function (data = psa_mean_averted_over65_vaccination, aki_no_outcome = FALSE, cost_per_aki_hospitalisation_episode = 2434)
upper_aki_hosp_cost <- cea_one_way_analysis_function (data = psa_mean_averted_over65_vaccination, aki_no_outcome = FALSE, cost_per_aki_hospitalisation_episode = 10787)

tornado_data_over65 %>%
  mutate(
    Lower_Deviation = ICER_lower - ICER_base,
    Upper_Deviation = ICER_upper - ICER_base,
    Total_Impact = abs(Lower_Deviation) + abs(Upper_Deviation)
  )
