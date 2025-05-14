# Define under 5 scenario base parameters
target_coverage_under5 <- 0.9
target_coverage_over65 <- 0.75
days_to_target <- 365

source("R/norovirus_model_r.R")

run_vaccination_scenarios <- function(params,
                                      vaccination_effect = 0.6,
                                      vaccination_rate = c(0, 0, 0, 0),
                                      immunity_waning = c(0, 0),
                                      n_iterations = 5,
                                      scenario_label = "Default") {
  
  # Set basic parameters
  params[["contacts"]] <- uk_contact_rate_matrix
  params[["vacc_start"]] <- 3535
  
  # Set vaccination parameters
  params[["phi_1"]] <- vaccination_rate
  params[["phi_2"]] <- c(0, 0, 0, 0)
  params[["upsilon"]] <- immunity_waning
  
  # Initialize output storage
  iteration_outputs <- list()
  
  # Create progress bar
  pb <- txtProgressBar(min = 0, max = n_iterations, style = 3)
  
  # Run iterations
  for (i in 1:n_iterations) {
    
    setTxtProgressBar(pb, i)
    
    
    # Update seasonal parameters
    params[["season_amp_over65"]] <- 1
    params[["season_amp"]] <- parameter_probabilistic_samples$probabilistic$seasonality_parameters$season_amp[i]
    params[["season_offset"]] <- parameter_probabilistic_samples$probabilistic$seasonality_parameters$season_offset[i]
    params[["D_immun"]] <- parameter_probabilistic_samples$probabilistic$seasonality_parameters$D_immun[i]
    
    # Set infection parameters with vaccination effect
    base_sigma <- parameter_probabilistic_samples$probabilistic$infection_parameters$sigma[i]
    params[["sigma"]] <- c(base_sigma, 
                           base_sigma * (1 - vaccination_effect),
                           base_sigma * (1 - vaccination_effect))
    
    # Update other infection parameters

    params[["probT_under5"]] <- log(parameter_probabilistic_samples$probabilistic$infection_parameters$probT_under5[i])
    params[["probT_over5"]] <- log(parameter_probabilistic_samples$probabilistic$infection_parameters$probT_over5[i])
    
    # Update age-specific parameters
    for (age in 1:4) {
      params[[paste0("gp_parameter_", age)]] <- parameter_probabilistic_samples$probabilistic$gp_parameters[[age]][i]
      params[[paste0("noro_hosp_parameter_", age)]] <- parameter_probabilistic_samples$probabilistic$noro_hosp_parameters[[age]][i]
      params[[paste0("noro_mortality_parameter_", age)]] <- parameter_probabilistic_samples$probabilistic$noro_mortality_parameters[[age]][i]
      params[[paste0("aki_mortality_parameter_", age)]] <- parameter_probabilistic_samples$probabilistic$aki_mortality_parameters[[age]][i]
      
      if (age <= 3) {
        params[[paste0("aki_hosp_parameter_", age)]] <- params[[paste0("aki_hospitalisation_", age)]]
      }
    }
    
    # Run model
    # output <- deSolve::lsoda(init_mat, times, norovirus_model_r, params)
    output <- deSolve::radau(init_mat, times, norovirus_model_r, params)
    # output <- deSolve::lsoda(init_mat, times, norovirus_model_r, params, 
    #                atol = 1e-6, rtol = 1e-4,
    #                hmin = 1e-8)
    # output <- deSolve::ode(init_mat, times, norovirus_model_r, params,
    #              method = "bdf",
    #              maxsteps = 100000,        # Allow more internal steps
    #              maxordn = 5,              # Limit order for increased stability
    #              hmin = 1e-10)             # Allow very small steps
    output_df <- as.data.frame(output)
    output_df$Scenario <- scenario_label
    output_df <- label_columns_update(output_df)
    output_df$Iteration <- i
    
    iteration_outputs[[i]] <- output_df
  }
  
  # Close progress bar
  close(pb)
  
  # Combine and return results
  do.call(rbind, iteration_outputs)
}


generate_uncertainty_bounds <- function(data) {
  data %>% 
    group_by(time) |> 
    reframe(
      percentile_2.5_noro_1 = quantile(Is1, probs = 0.025, na.rm = TRUE),
      percentile_2.5_noro_2 = quantile(Is2, probs = 0.025, na.rm = TRUE),
      percentile_2.5_noro_3 = quantile(Is3, probs = 0.025, na.rm = TRUE),
      percentile_2.5_noro_4 = quantile(Is4, probs = 0.025, na.rm = TRUE),
      percentile_97.5_noro_1 = quantile(Is1, probs = 0.975, na.rm = TRUE),
      percentile_97.5_noro_2 = quantile(Is2, probs = 0.975, na.rm = TRUE),
      percentile_97.5_noro_3 = quantile(Is3, probs = 0.975, na.rm = TRUE),
      percentile_97.5_noro_4 = quantile(Is4, probs = 0.975, na.rm = TRUE),
      percentile_50_noro_1 = mean(Is1, na.rm = TRUE),
      percentile_50_noro_2 = mean(Is2, na.rm = TRUE),
      percentile_50_noro_3 = mean(Is3, na.rm = TRUE),
      percentile_50_noro_4 = mean(Is4, na.rm = TRUE)
    ) |>
    ungroup() 
}

# saveRDS(parameter_probabilistic_samples, "data/parameter_probabilistic_samples_list.rds")
# parameter_probabilistic_samples <- readRDS("data/parameter_probabilistic_samples_list.rds")

no_vaccination <- run_vaccination_scenarios(
  params = default_parameters(),
  vaccination_effect = 0.5,
  vaccination_rate = c(0, 0, 0, 0),  # Under 5 vaccination
  immunity_waning = c(0, 0),     # 4.5 year immunity
  scenario_label = "No Vaccination",
  n_iterations = n_iter
)

write_parquet(no_vaccination, "data/no_vaccination.parquet")
no_vaccination_uncertainty <- generate_uncertainty_bounds(no_vaccination)
write_parquet(no_vaccination_uncertainty, "data/no_vaccination_uncertainty.parquet")
remove(no_vaccination)
gc()

under_5_vaccination <- run_vaccination_scenarios(
  params = default_parameters(),
  vaccination_effect = 0.5,
  vaccination_rate = c(target_coverage_under5/days_to_target, 0, 0, 0),  # Under 5 vaccination
  immunity_waning = c((1/(2*365)), 0),     # 4.5 year immunity
  scenario_label = "Under 5 vaccination",
  n_iterations = n_iter
)

write_parquet(under_5_vaccination, "data/under_5_vaccination.parquet")
under_5_vaccination_uncertainty <- generate_uncertainty_bounds(under_5_vaccination)
write_parquet(under_5_vaccination_uncertainty, "data/under_5_vaccination_uncertainty.parquet")
remove(under_5_vaccination)
gc()


over65_vaccination <- run_vaccination_scenarios(
  params = default_parameters(),
  vaccination_effect = 0.5,
  vaccination_rate = c(0, 0, 0, target_coverage_over65/days_to_target),  # Over 65 vaccination
  immunity_waning = c((1/(2*365)), 0),     # 4.5 year immunity
  scenario_label = "Over 65 vaccination",
  n_iterations = n_iter
)

write_parquet(over65_vaccination, "data/over65_vaccination.parquet")
over65_vaccination_uncertainty <- generate_uncertainty_bounds(over65_vaccination)
write_parquet(over65_vaccination_uncertainty, "data/over65_vaccination_uncertainty.parquet")
remove(over65_vaccination)
gc()

under5_over65_vaccination <- run_vaccination_scenarios(
  params = default_parameters(),
  vaccination_effect = 0.5,
  vaccination_rate = c(target_coverage_under5/days_to_target, 0, 0, target_coverage_over65/days_to_target),  # Over 65 vaccination
  immunity_waning = c((1/(2*365)), 0),     # 4.5 year immunity
  scenario_label = "Under 5 and 65+ vaccination",
  n_iterations = n_iter
)

write_parquet(under5_over65_vaccination, "data/under5_over65_vaccination.parquet")
under5_over65_vaccination_uncertainty <- generate_uncertainty_bounds(under5_over65_vaccination)
write_parquet(under5_over65_vaccination_uncertainty, "data/under5_over65_vaccination_uncertainty.parquet")
remove(under5_over65_vaccination)
gc()