cea_dynamic_model_probabilistic <- function(
  wtp = 20e3,
  data = averted_combo_vaccination,
  qaly_discount_rate = 0.035,
  cost_discount_rate = 0.035,
  cost_factor = 1,
  average_life_expectancy_discount = TRUE ,
  aki_no_outcome = FALSE,
  parameter_probabilistic_samples = parameter_probabilistic_samples,
  n_iterations = n_iter,
  cost_per_dose = 35,
  qaly_table = FALSE,
  cost_table = FALSE
) {
  
  results_list <- vector("list", n_iterations)
  qalys_list <- vector("list", n_iterations)
  costs_list <- vector("list", n_iterations)
  
  for (i in 1:n_iterations) {
    # Extract probabilistic parameters for this iteration
    qaly_aki_hospitalisation <- parameter_probabilistic_samples$probabilistic$qalys$aki_hosp[i]
    cost_per_child_noro_hospitalisation_episode <- parameter_probabilistic_samples$probabilistic$costs$norovirus_hosp_1[i]
    cost_per_adult_noro_hospitalisation_episode <- parameter_probabilistic_samples$probabilistic$costs$norovirus_hosp_2[i]
    qaly_noro_gp_attendance <- parameter_probabilistic_samples$probabilistic$qalys$gp_attendance[i]
    qaly_noro_hospitalisation <- parameter_probabilistic_samples$probabilistic$qalys$norovirus_hosp[i]
    cost_noro_gp_attendance <- parameter_probabilistic_samples$probabilistic$costs$gp_attendance[i]
    cost_aki_hosp <- parameter_probabilistic_samples$probabilistic$costs$aki_hosp[i]
    admin_per_dose_1 <- parameter_probabilistic_samples$probabilistic$costs$admin_per_dose_1[i]
    admin_per_dose_2 <- parameter_probabilistic_samples$probabilistic$costs$admin_per_dose_2[i]
    gp_parameter_1 <- parameter_probabilistic_samples$probabilistic$gp_parameters$gp_parameter_1[i]
    noro_hosp_parameter_1 <- parameter_probabilistic_samples$probabilistic$noro_hosp_parameters$noro_hosp_parameter_1[i]
    noro_morality_parameter_1 <- parameter_probabilistic_samples$probabilistic$noro_mortality_parameters$noro_mortality_parameter_1[i]
    aki_hosp_parameter_1 <- params$aki_hospitalisation_1
    aki_mortality_parameter_1 <- parameter_probabilistic_samples$probabilistic$aki_mortality_parameters$aki_mortality_parameter_1[i]
    gp_parameter_2 <- parameter_probabilistic_samples$probabilistic$gp_parameters$gp_parameter_2[i]
    noro_hosp_parameter_2 <- parameter_probabilistic_samples$probabilistic$noro_hosp_parameters$noro_hosp_parameter_2[i]
    noro_morality_parameter_2 <- parameter_probabilistic_samples$probabilistic$noro_mortality_parameters$noro_mortality_parameter_2[i]
    aki_hosp_parameter_2 <- params$aki_hospitalisation_2
    aki_mortality_parameter_2 <- parameter_probabilistic_samples$probabilistic$aki_mortality_parameters$aki_mortality_parameter_2[i]
    gp_parameter_3 <- parameter_probabilistic_samples$probabilistic$gp_parameters$gp_parameter_3[i]
    noro_hosp_parameter_3 <- parameter_probabilistic_samples$probabilistic$noro_hosp_parameters$noro_hosp_parameter_3[i]
    noro_morality_parameter_3 <- parameter_probabilistic_samples$probabilistic$noro_mortality_parameters$noro_mortality_parameter_3[i]
    aki_hosp_parameter_3 <- params$aki_hospitalisation_3
    aki_mortality_parameter_3 <- parameter_probabilistic_samples$probabilistic$aki_mortality_parameters$aki_mortality_parameter_3[i]
    gp_parameter_4 <- parameter_probabilistic_samples$probabilistic$gp_parameters$gp_parameter_4[i]
    noro_hosp_parameter_4 <- parameter_probabilistic_samples$probabilistic$noro_hosp_parameters$noro_hosp_parameter_4[i]
    noro_morality_parameter_4 <- parameter_probabilistic_samples$probabilistic$noro_mortality_parameters$noro_mortality_parameter_4[i]
    aki_mortality_parameter_4 <- parameter_probabilistic_samples$probabilistic$aki_mortality_parameters$aki_mortality_parameter_4[i]
    
    if (average_life_expectancy_discount) {
      average_life_expectancy = QALY_UK$disc
    } else {
      average_life_expectancy = QALY_UK$undisc
    }

    if (aki_no_outcome) {
      aki_hosp_parameter_4 = 0
    } else {
      aki_hosp_parameter_4 <- parameter_probabilistic_samples$probabilistic$aki_hosp_parameters$aki_hosp_parameter_4[i]
    }
    
    # # Annualise outcomes
    # annual_under5_infections <- annualise_scenario_data(get(paste0("data_under_5_scenarios_", vaccine_effectiveness, "_effec")), "total_infections")
    # annual_5_14_infections <- annualise_scenario_data(get(paste0("data_under_5_14_scenarios_", vaccine_effectiveness, "_effec")), "total_infections")
    # annual_15_64_infections <- annualise_scenario_data(get(paste0("data_15_64_scenarios_", vaccine_effectiveness, "_effec")), "total_infections")
    # annual_over65_infections <- annualise_scenario_data(get(paste0("data_over_65_scenarios_", vaccine_effectiveness, "_effec")), "total_infections")
    
    # annualise vaccinated number
    # annual_under5_infections <- annual_under5_infections %>% 
    #   left_join(annualise_vaccination_data %>%  select(averted_scenario, season, total_under5_vaccinated), by = c("scenario" = "averted_scenario", "season")) %>% 
    #   rename(total_vaccinated = total_under5_vaccinated)
    
    annual_under5_infections <- data %>%
      filter(Iteration == i) %>% 
      select(season, Iteration, averted_under5, total_under5_vaccinated) %>% 
      rename(total_vaccinated = total_under5_vaccinated,
             total_infections = averted_under5)
    
    annual_5_14_infections <- data %>%
      filter(Iteration == i) %>% 
      select(season, Iteration, averted_5_14, total_5_14_vaccinated ) %>% 
      rename(total_vaccinated = total_5_14_vaccinated ,
             total_infections = averted_5_14)
    
    annual_15_64_infections <- data %>%
      filter(Iteration == i) %>% 
      select(season, Iteration, averted_15_64, total_15_64_vaccinated ) %>% 
      rename(total_vaccinated = total_15_64_vaccinated ,
             total_infections = averted_15_64)
    
    annual_over65_infections <- data %>%
      filter(Iteration == i) %>% 
      select(season, Iteration, averted_over65, total_65_plus_vaccinated  ) %>% 
      rename(total_vaccinated = total_65_plus_vaccinated ,
             total_infections = averted_over65)
    
    # Discount outcomes using probabilistic function
    discounted_under5_outcomes <- discounting_data_probabilistic(
      annual_under5_infections,
      qaly_discount_rate = qaly_discount_rate,
      cost_discount_rate = cost_discount_rate,
      cost_factor = cost_factor,
      average_life_expectancy = average_life_expectancy[1],
      gp_parameter = gp_parameter_1,
      noro_hosp_parameter = noro_hosp_parameter_1,
      noro_morality_parameter = noro_morality_parameter_1,
      aki_hosp_parameter = aki_hosp_parameter_1,
      aki_mortality_parameter = aki_mortality_parameter_1,
      qaly_aki_hospitalisation = qaly_aki_hospitalisation,
      qaly_noro_gp_attendance = qaly_noro_gp_attendance,
      qaly_noro_hospitalisation = qaly_noro_hospitalisation,
      cost_noro_gp_attendance = cost_noro_gp_attendance,
      cost_noro_hosp = cost_per_child_noro_hospitalisation_episode,
      cost_aki_hosp = cost_aki_hosp,
      cost_per_dose = cost_per_dose,
      admin_per_dose = admin_per_dose_1
    )
    
    discounted_5_14_outcomes <- discounting_data_probabilistic(
      annual_5_14_infections,
      qaly_discount_rate = qaly_discount_rate,
      cost_discount_rate = cost_discount_rate,
      cost_factor = cost_factor,
      average_life_expectancy = average_life_expectancy[2],
      gp_parameter = gp_parameter_2,
      noro_hosp_parameter = noro_hosp_parameter_2,
      noro_morality_parameter = noro_morality_parameter_2,
      aki_hosp_parameter = aki_hosp_parameter_2,
      aki_mortality_parameter = aki_mortality_parameter_2,
      qaly_aki_hospitalisation = qaly_aki_hospitalisation,
      qaly_noro_gp_attendance = qaly_noro_gp_attendance,
      qaly_noro_hospitalisation = qaly_noro_hospitalisation,
      cost_noro_gp_attendance = cost_noro_gp_attendance,
      cost_noro_hosp = cost_per_child_noro_hospitalisation_episode,
      cost_aki_hosp = cost_aki_hosp,
      cost_per_dose = cost_per_dose,
      admin_per_dose = admin_per_dose_1
    )
    
    discounted_15_64_outcomes <- discounting_data_probabilistic(
      annual_15_64_infections,
      qaly_discount_rate = qaly_discount_rate,
      cost_discount_rate = cost_discount_rate,
      cost_factor = cost_factor,
      average_life_expectancy = average_life_expectancy[3],
      gp_parameter = gp_parameter_3,
      noro_hosp_parameter = noro_hosp_parameter_3,
      noro_morality_parameter = noro_morality_parameter_3,
      aki_hosp_parameter = aki_hosp_parameter_3,
      aki_mortality_parameter = aki_mortality_parameter_3,
      qaly_aki_hospitalisation = qaly_aki_hospitalisation,
      qaly_noro_gp_attendance = qaly_noro_gp_attendance,
      qaly_noro_hospitalisation = qaly_noro_hospitalisation,
      cost_noro_gp_attendance = cost_noro_gp_attendance,
      cost_noro_hosp = cost_per_adult_noro_hospitalisation_episode,
      cost_aki_hosp = cost_aki_hosp,
      cost_per_dose = cost_per_dose,
      admin_per_dose = admin_per_dose_2
    )
    
    discounted_over65_outcomes <- discounting_data_probabilistic(
      annual_over65_infections,
      qaly_discount_rate = qaly_discount_rate,
      cost_discount_rate = cost_discount_rate,
      cost_factor = cost_factor,
      average_life_expectancy = average_life_expectancy[4],
      gp_parameter = gp_parameter_4,
      noro_hosp_parameter = noro_hosp_parameter_4,
      noro_morality_parameter = noro_morality_parameter_4,
      aki_hosp_parameter = aki_hosp_parameter_4,
      aki_mortality_parameter = aki_mortality_parameter_4,
      qaly_aki_hospitalisation = qaly_aki_hospitalisation,
      qaly_noro_gp_attendance = qaly_noro_gp_attendance,
      qaly_noro_hospitalisation = qaly_noro_hospitalisation,
      cost_noro_gp_attendance = cost_noro_gp_attendance,
      cost_noro_hosp = cost_per_adult_noro_hospitalisation_episode,
      cost_aki_hosp = cost_aki_hosp,
      cost_per_dose = cost_per_dose,
      admin_per_dose = admin_per_dose_2
    )
    
    # Calculate averted costs
    averted_cost_aki_hosp_all_ages <- sum(
      discounted_under5_outcomes$discounted_cost_aki_hospitalisations,
      discounted_5_14_outcomes$discounted_cost_aki_hospitalisations,
      discounted_15_64_outcomes$discounted_cost_aki_hospitalisations,
      discounted_over65_outcomes$discounted_cost_aki_hospitalisations
    )
    
    averted_cost_noro_gp_all_ages <- sum(
      discounted_under5_outcomes$discounted_cost_gp_attendances,
      discounted_5_14_outcomes$discounted_cost_gp_attendances,
      discounted_15_64_outcomes$discounted_cost_gp_attendances,
      discounted_over65_outcomes$discounted_cost_gp_attendances
    )
    
    averted_cost_noro_hosp_all_ages <- sum(
      discounted_under5_outcomes$discounted_cost_noro_hospitalisations,
      discounted_5_14_outcomes$discounted_cost_noro_hospitalisations,
      discounted_15_64_outcomes$discounted_cost_noro_hospitalisations,
      discounted_over65_outcomes$discounted_cost_noro_hospitalisations
    )
    
    averted_cost_all_ages <- averted_cost_aki_hosp_all_ages + averted_cost_noro_gp_all_ages + averted_cost_noro_hosp_all_ages
    
    # Calcualte vaccination costs 
    
    vaccine_cost <- discounted_under5_outcomes$discounted_cost_per_dose + discounted_5_14_outcomes$discounted_cost_per_dose + discounted_15_64_outcomes$discounted_cost_per_dose + discounted_over65_outcomes$discounted_cost_per_dose
    admin_cost <- discounted_under5_outcomes$discounted_admin_per_dose + discounted_5_14_outcomes$discounted_admin_per_dose + discounted_15_64_outcomes$discounted_admin_per_dose + discounted_over65_outcomes$discounted_admin_per_dose
    program_cost <- vaccine_cost + admin_cost
    
    # Calculate QALY gains
    
    qaly_gain_under5 <- sum(
      discounted_under5_outcomes$discounted_qaly_loss_aki_hospitalisation,
      discounted_under5_outcomes$discounted_qaly_loss_noro_hospitalisation,
      discounted_under5_outcomes$discounted_qaly_loss_gp_attendances,
      discounted_under5_outcomes$discounted_qale_loss_noro_mortality,
      discounted_under5_outcomes$discounted_qale_loss_aki_mortality
    )
    
    qaly_gain_5to14 <- sum(
      discounted_5_14_outcomes$discounted_qaly_loss_aki_hospitalisation,
      discounted_5_14_outcomes$discounted_qaly_loss_noro_hospitalisation,
      discounted_5_14_outcomes$discounted_qaly_loss_gp_attendances,
      discounted_5_14_outcomes$discounted_qale_loss_noro_mortality,
      discounted_5_14_outcomes$discounted_qale_loss_aki_mortality
    )
    
    qaly_gain_15to64 <- sum(
      discounted_15_64_outcomes$discounted_qaly_loss_aki_hospitalisation,
      discounted_15_64_outcomes$discounted_qaly_loss_noro_hospitalisation,
      discounted_15_64_outcomes$discounted_qaly_loss_gp_attendances,
      discounted_15_64_outcomes$discounted_qale_loss_noro_mortality,
      discounted_15_64_outcomes$discounted_qale_loss_aki_mortality
    )
    
    qaly_gain_over65 <- sum(
      discounted_over65_outcomes$discounted_qaly_loss_aki_hospitalisation,
      discounted_over65_outcomes$discounted_qaly_loss_noro_hospitalisation,
      discounted_over65_outcomes$discounted_qaly_loss_gp_attendances,
      discounted_over65_outcomes$discounted_qale_loss_noro_mortality,
      discounted_over65_outcomes$discounted_qale_loss_aki_mortality
    )
    
    dynamic_qaly_gain_all_ages <- qaly_gain_under5 + qaly_gain_5to14 + qaly_gain_15to64 + qaly_gain_over65
    
    # CEA calculations
    dynamic_incr_cost <- program_cost - averted_cost_all_ages
    dynamic_icer <- dynamic_incr_cost / dynamic_qaly_gain_all_ages
    dynamic_inmb <- wtp * dynamic_qaly_gain_all_ages - dynamic_incr_cost
    
    results_list[[i]] <- c(
      total_cost = program_cost,
      icer = dynamic_icer,
      incr_cost = dynamic_incr_cost,
      qaly_gain = dynamic_qaly_gain_all_ages,
      inmb_k = dynamic_inmb
      
      # added to sense check costs
      # program_cost = program_cost,
      # admin_cost = admin_cost,
      # vaccine_cost = vaccine_cost,
      # averted_cost_aki_hosp_all_ages = averted_cost_aki_hosp_all_ages,
      # averted_cost_noro_gp_all_ages = averted_cost_noro_gp_all_ages,
      # averted_cost_noro_hosp_all_ages = averted_cost_noro_hosp_all_ages,
      # averted_cost_all_ages = averted_cost_all_ages,
      # qaly_gain_over65 = qaly_gain_over65,
      # qaly_gain_15to64 = qaly_gain_15to64,
      # qaly_gain_5to14 = qaly_gain_5to14,
      # qaly_gain_under5 = qaly_gain_under5
      
    )
    
    # Create the table if qaly_table is TRUE
    
    qalys_list[[i]]  <- data.frame(
      iteration = i,
      Age_Group = c("0-4", "5-14", "15-64", "65+"),
      gp_attendance = c(discounted_under5_outcomes$discounted_qaly_loss_gp_attendances,
                          discounted_5_14_outcomes$discounted_qaly_loss_gp_attendances,
                          discounted_15_64_outcomes$discounted_qaly_loss_gp_attendances,
                          discounted_over65_outcomes$discounted_qaly_loss_gp_attendances),
      noro_hosp = c(discounted_under5_outcomes$discounted_qaly_loss_noro_hospitalisation,
                      discounted_5_14_outcomes$discounted_qaly_loss_noro_hospitalisation,
                      discounted_15_64_outcomes$discounted_qaly_loss_noro_hospitalisation,
                      discounted_over65_outcomes$discounted_qaly_loss_noro_hospitalisation),
      aki_hosp = c(discounted_under5_outcomes$discounted_qaly_loss_aki_hospitalisation,
                     discounted_5_14_outcomes$discounted_qaly_loss_aki_hospitalisation,
                     discounted_15_64_outcomes$discounted_qaly_loss_aki_hospitalisation,
                     discounted_over65_outcomes$discounted_qaly_loss_aki_hospitalisation),
      noro_mortality = c(discounted_under5_outcomes$discounted_qale_loss_noro_mortality,
                           discounted_5_14_outcomes$discounted_qale_loss_noro_mortality,
                           discounted_15_64_outcomes$discounted_qale_loss_noro_mortality,
                           discounted_over65_outcomes$discounted_qale_loss_noro_mortality),
      aki_mortality = c(discounted_under5_outcomes$discounted_qale_loss_aki_mortality,
                          discounted_5_14_outcomes$discounted_qale_loss_aki_mortality,
                          discounted_15_64_outcomes$discounted_qale_loss_aki_mortality,
                          discounted_over65_outcomes$discounted_qale_loss_aki_mortality)
      )
    
    # Create the cost table if cost_table is TRUE
    
    costs_list[[i]]   <- data.frame(
      iteration = i,
      Age_Group = c("0-4", "5-14", "15-64", "65+"),
      gp_attendance_cost = c(discounted_under5_outcomes$discounted_cost_gp_attendances,
                             discounted_5_14_outcomes$discounted_cost_gp_attendances,
                             discounted_15_64_outcomes$discounted_cost_gp_attendances,
                             discounted_over65_outcomes$discounted_cost_gp_attendances),
      noro_hosp_cost = c(discounted_under5_outcomes$discounted_cost_noro_hospitalisations,
                         discounted_5_14_outcomes$discounted_cost_noro_hospitalisations,
                         discounted_15_64_outcomes$discounted_cost_noro_hospitalisations,
                         discounted_over65_outcomes$discounted_cost_noro_hospitalisations),
      aki_hosp_cost = c(discounted_under5_outcomes$discounted_cost_aki_hospitalisations,
                        discounted_5_14_outcomes$discounted_cost_aki_hospitalisations,
                        discounted_15_64_outcomes$discounted_cost_aki_hospitalisations,
                        discounted_over65_outcomes$discounted_cost_aki_hospitalisations)
    )
      
    }
  
  if (qaly_table) {
  qalys_df <- do.call(rbind, qalys_list)
  # Return the table
  return(qalys_df)
  }
  
  if (cost_table) {
    costs_df <- do.call(rbind, costs_list)
    # Return the table
    return(costs_df)
  }
  
  
  # Process results
  qaly_threshold <- 0.00001         # exclude very extreme values of QALYs because it skews the MEAN ICER
  
  results_df <- do.call(rbind, results_list) 
  colnames(results_df) <- c("total_cost", "icer", "incr_cost", "qaly_gain", "inmb_k" 
                            #  "program_cost", "admin_cost", "vaccine_cost", 
                            #  "averted_cost_aki_hosp_all_ages", 
                            #  "averted_cost_noro_gp_all_ages", 
                            #  "averted_cost_noro_hosp_all_ages",
                            # "averted_cost_all_ages", 
                            #  "qaly_gain_over65", 
                            #  "qaly_gain_15to64", 
                            #  "qaly_gain_5to14", 
                            #  "qaly_gain_under5"
                            )
  
  
  # Filter using matrix indexing - column 4 is qaly_gain
  results_df <- results_df[abs(results_df[,4]) >= qaly_threshold, ]
  
  # Calculate summary statistics
  summary_stats <- data.frame(
    metric = c("Total cost", "ICER", "Incremental Cost", "QALY Gain", "INMB"
               # "program_cost", "admin_cost", "vaccine_cost", 
               # "averted_cost_aki_hosp_all_ages", 
               # "averted_cost_noro_gp_all_ages", 
               # "averted_cost_noro_hosp_all_ages" , "averted_cost_all_ages", 
               # "qaly_gain_over65",
               # "qaly_gain_15to64",
               # "qaly_gain_5to14",
               # "qaly_gain_under5"
               ),
    mean = colMeans(results_df),
    median = apply(results_df, 2, median),
    lower_ci = apply(results_df, 2, quantile, probs = 0.025),
    upper_ci = apply(results_df, 2, quantile, probs = 0.975)
  )
  
  # Calculate probability of cost-effectiveness
  prob_cost_effective <- mean(results_df[, "inmb_k"] > 0)
  
  return(list(
    summary_stats = summary_stats,
    prob_cost_effective = prob_cost_effective,
    raw_results = results_df
  ))
}
