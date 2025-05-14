
mean_no_vaccination
mean_under_5_vaccination
mean_averted_under5_vaccination

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

under_5_vaccination_uncertainty |> 
  select(percentile_50_noro_1, season) |>
  filter(season != "-S1") |> 
  summarise(total = sum(percentile_50_noro_1, na.rm = TRUE))
  

no_vaccination_uncertainty |> 
  select(percentile_50_noro_1, season) |> 
  filter(season != "-S1") |> 
  group_by(season) |>
  summarise(total = sum(percentile_50_noro_1, na.rm = TRUE)) |>
  ungroup()


one_way_mean_icer <- cea_one_way_analysis_function(data = mean_averted_under5_vaccination, aki_no_outcome = FALSE)

df <- tibble(id = 1) %>%
  mutate(
    base_sigma = mean(parameter_probabilistic_samples$probabilistic$infection_parameters$sigma),
    D_immun = mean(parameter_probabilistic_samples$probabilistic$seasonality_parameters$D_immun),
    probT_under5 = log(mean(parameter_probabilistic_samples$probabilistic$infection_parameters$probT_under5)),
    probT_over5 = log(mean(parameter_probabilistic_samples$probabilistic$infection_parameters$probT_over5)),
    season_amp = mean(parameter_probabilistic_samples$probabilistic$seasonality_parameters$season_amp),
    season_offset = c(mean(parameter_probabilistic_samples$probabilistic$seasonality_parameters$season_offset)),
    gp_parameter_1 =  mean(
      parameter_probabilistic_samples$probabilistic$gp_parameters$gp_parameter_1
    ),
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
    )
  ) |> 
  pivot_longer(
    cols = everything(),  # Select all columns
    names_to = "parameter",  # Column name for the parameter names
    values_to = "value"  # Column name for the parameter values
  )

df_long

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
