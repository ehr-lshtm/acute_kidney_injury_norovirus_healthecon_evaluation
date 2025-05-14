# cea function

cea_one_way_analysis_function <- function(data = data,
                                          cost_per_dose = 35,
                                          admin_per_dose_child = mean(parameter_probabilistic_samples$probabilistic$costs$admin_per_dose_1),
                                          admin_per_dose_adult = mean(parameter_probabilistic_samples$probabilistic$costs$admin_per_dose_2),
                                          cost_per_child_noro_hospitalisation_episode = mean(parameter_probabilistic_samples$probabilistic$costs$norovirus_hosp_1),
                                          cost_per_adult_noro_hospitalisation_episode =  mean(parameter_probabilistic_samples$probabilistic$costs$norovirus_hosp_2),
                                          cost_noro_gp_attendance = mean(parameter_probabilistic_samples$probabilistic$costs$gp_attendance),
                                          cost_per_aki_hospitalisation_episode = mean(parameter_probabilistic_samples$probabilistic$costs$aki_hosp),
                                          wtp = 20e3,
                                          qaly_noro_gp_attendance = mean(parameter_probabilistic_samples$probabilistic$qalys$gp_attendance),
                                          qaly_noro_hospitalisation = mean(parameter_probabilistic_samples$probabilistic$qalys$norovirus_hosp),
                                          qaly_aki_hospitalisation = mean(parameter_probabilistic_samples$probabilistic$qalys$aki_hosp),
                                          qaly_discount_rate = 0.035,
                                          cost_discount_rate = 0.035,
                                          cost_factor = 1,
                                          # cost factor multiplies the cost by a factor to increase or decrease hospitalisation/gp costs
                                          average_life_expectancy_discount = TRUE ,
                                          qaly_table = FALSE,
                                          inmb_output = FALSE,
                                          cost_table = FALSE,
                                          icer_only = TRUE,
                                          aki_no_outcome = FALSE,
                                          gp_parameter_1 =  mean(parameter_probabilistic_samples$probabilistic$gp_parameters$gp_parameter_1),
                                          noro_hosp_parameter_1 = mean(parameter_probabilistic_samples$probabilistic$noro_hosp_parameters$noro_hosp_parameter_1),
                                          noro_morality_parameter_1 = mean(parameter_probabilistic_samples$probabilistic$noro_mortality_parameters$noro_mortality_parameter_1),
                                          aki_hosp_parameter_1 = params[["aki_hospitalisation_1"]],
                                          aki_mortality_parameter_1 = mean(parameter_probabilistic_samples$probabilistic$aki_mortality_parameters$aki_mortality_parameter_1),
                                          gp_parameter_2 = mean(parameter_probabilistic_samples$probabilistic$gp_parameters$gp_parameter_2),
                                          noro_hosp_parameter_2 = mean(parameter_probabilistic_samples$probabilistic$noro_hosp_parameters$noro_hosp_parameter_2),
                                          noro_morality_parameter_2 = mean(parameter_probabilistic_samples$probabilistic$noro_mortality_parameters$noro_mortality_parameter_2),
                                          aki_hosp_parameter_2 = params[["aki_hospitalisation_2"]],
                                          aki_mortality_parameter_2 = mean(parameter_probabilistic_samples$probabilistic$aki_mortality_parameters$aki_mortality_parameter_2),
                                          gp_parameter_3 = mean(parameter_probabilistic_samples$probabilistic$gp_parameters$gp_parameter_3),
                                          noro_hosp_parameter_3 = mean(parameter_probabilistic_samples$probabilistic$noro_hosp_parameters$noro_hosp_parameter_3),
                                          noro_morality_parameter_3 = mean(parameter_probabilistic_samples$probabilistic$noro_mortality_parameters$noro_mortality_parameter_3),
                                          aki_hosp_parameter_3 = params[["aki_hospitalisation_3"]],
                                          aki_mortality_parameter_3 = mean(parameter_probabilistic_samples$probabilistic$aki_mortality_parameters$aki_mortality_parameter_3),
                                          gp_parameter_4 = mean(parameter_probabilistic_samples$probabilistic$gp_parameters$gp_parameter_4),
                                          noro_hosp_parameter_4 = mean(parameter_probabilistic_samples$probabilistic$noro_hosp_parameters$noro_hosp_parameter_4),
                                          noro_morality_parameter_4 = mean(parameter_probabilistic_samples$probabilistic$noro_mortality_parameters$noro_mortality_parameter_4),
                                          aki_hosp_parameter_4 = mean(parameter_probabilistic_samples$probabilistic$aki_hosp_parameters$aki_hosp_parameter_4),
                                          aki_mortality_parameter_4 = mean(parameter_probabilistic_samples$probabilistic$aki_mortality_parameters$aki_mortality_parameter_4)) {
  
  if (average_life_expectancy_discount) {
    average_life_expectancy = QALY_UK$disc
  } else {
    average_life_expectancy = QALY_UK$undisc
  }
  
  if (aki_no_outcome) {
    aki_hosp_parameter_4 = 0
  } else {
    aki_hosp_parameter_4 <- aki_hosp_parameter_4
  }
  
  # annualise vaccinated number
  annual_under5_infections <- data %>%
    select(season, averted_under5, total_under5_vaccinated) %>% 
    rename(total_vaccinated = total_under5_vaccinated,
           total_infections = averted_under5)
  
  annual_5_14_infections <- data %>%
    select(season,  averted_5_14, total_5_14_vaccinated ) %>% 
    rename(total_vaccinated = total_5_14_vaccinated ,
           total_infections = averted_5_14)
  
  annual_15_64_infections <- data %>%
    select(season, averted_15_64, total_15_64_vaccinated ) %>% 
    rename(total_vaccinated = total_15_64_vaccinated ,
           total_infections = averted_15_64)
  
  annual_over65_infections <- data %>%
    select(season, averted_over65, total_65_plus_vaccinated  ) %>% 
    rename(total_vaccinated = total_65_plus_vaccinated ,
           total_infections = averted_over65)
  
  # discount outcomes and cost
  
  discounted_under5_outcomes <- discounting_data_probabilistic(
    data = annual_under5_infections,
    probabilistic_discounting = FALSE,
    gp_parameter = gp_parameter_1,
    noro_hosp_parameter = noro_hosp_parameter_1,
    noro_morality_parameter = noro_morality_parameter_1,
    aki_hosp_parameter = aki_hosp_parameter_1,
    aki_mortality_parameter = aki_mortality_parameter_1,
    average_life_expectancy = average_life_expectancy[1],
    qaly_discount_rate = qaly_discount_rate,
    cost_discount_rate = cost_discount_rate,
    cost_factor = cost_factor,
    qaly_aki_hospitalisation = qaly_aki_hospitalisation,
    qaly_noro_gp_attendance = qaly_noro_gp_attendance,
    qaly_noro_hospitalisation = qaly_noro_hospitalisation,
    cost_noro_hosp = cost_per_child_noro_hospitalisation_episode,
    cost_noro_gp_attendance = cost_noro_gp_attendance ,
    cost_aki_hosp = cost_per_aki_hospitalisation_episode,
    cost_per_dose = cost_per_dose,
    admin_per_dose = admin_per_dose_child
  )
  
  discounted_5_14_outcomes <- discounting_data_probabilistic(
    data = annual_5_14_infections,
    probabilistic_discounting = FALSE,
    gp_parameter = gp_parameter_2,
    noro_hosp_parameter = noro_hosp_parameter_2,
    noro_morality_parameter = noro_morality_parameter_2,
    aki_hosp_parameter = aki_hosp_parameter_2,
    aki_mortality_parameter = aki_mortality_parameter_2,
    average_life_expectancy = average_life_expectancy[2],
    qaly_discount_rate = qaly_discount_rate,
    cost_discount_rate = cost_discount_rate,
    cost_factor = cost_factor,
    qaly_aki_hospitalisation = qaly_aki_hospitalisation,
    qaly_noro_gp_attendance = qaly_noro_gp_attendance,
    qaly_noro_hospitalisation = qaly_noro_hospitalisation,
    cost_noro_hosp = cost_per_child_noro_hospitalisation_episode,
    cost_noro_gp_attendance = cost_noro_gp_attendance ,
    cost_aki_hosp = cost_per_aki_hospitalisation_episode,
    cost_per_dose = cost_per_dose,
    admin_per_dose = admin_per_dose_child
  )
  
  discounted_15_64_outcomes <- discounting_data_probabilistic(
    data = annual_15_64_infections,
    probabilistic_discounting = FALSE,
    gp_parameter = gp_parameter_3,
    noro_hosp_parameter = noro_hosp_parameter_3,
    noro_morality_parameter = noro_morality_parameter_3,
    aki_hosp_parameter = aki_hosp_parameter_3,
    aki_mortality_parameter = aki_mortality_parameter_3,
    average_life_expectancy = average_life_expectancy[3],
    qaly_discount_rate = qaly_discount_rate,
    cost_discount_rate = cost_discount_rate,
    cost_factor = cost_factor,
    qaly_aki_hospitalisation = qaly_aki_hospitalisation,
    qaly_noro_gp_attendance = qaly_noro_gp_attendance,
    qaly_noro_hospitalisation = qaly_noro_hospitalisation,
    cost_noro_hosp = cost_per_adult_noro_hospitalisation_episode,
    cost_noro_gp_attendance = cost_noro_gp_attendance ,
    cost_aki_hosp = cost_per_aki_hospitalisation_episode,
    cost_per_dose = cost_per_dose,
    admin_per_dose = admin_per_dose_adult
  )
  
  discounted_over65_outcomes <- discounting_data_probabilistic(
    data = annual_over65_infections,
    probabilistic_discounting = FALSE,
    gp_parameter = gp_parameter_4,
    noro_hosp_parameter = noro_hosp_parameter_4,
    noro_morality_parameter = noro_morality_parameter_4,
    aki_hosp_parameter = aki_hosp_parameter_4,
    aki_mortality_parameter = aki_mortality_parameter_4,
    average_life_expectancy = average_life_expectancy[4],
    qaly_discount_rate = qaly_discount_rate,
    cost_discount_rate = cost_discount_rate,
    cost_factor = cost_factor,
    qaly_aki_hospitalisation = qaly_aki_hospitalisation,
    qaly_noro_gp_attendance = qaly_noro_gp_attendance,
    qaly_noro_hospitalisation = qaly_noro_hospitalisation,
    cost_noro_hosp = cost_per_adult_noro_hospitalisation_episode,
    cost_noro_gp_attendance = cost_noro_gp_attendance ,
    cost_aki_hosp = cost_per_aki_hospitalisation_episode,
    cost_per_dose = cost_per_dose,
    admin_per_dose = admin_per_dose_adult
  )
  
  # averted costs
  averted_cost_aki_hosp_all_ages  = discounted_under5_outcomes$discounted_cost_aki_hospitalisations  + discounted_5_14_outcomes$discounted_cost_aki_hospitalisations  + discounted_15_64_outcomes$discounted_cost_aki_hospitalisations  + discounted_over65_outcomes$discounted_cost_aki_hospitalisations 
  averted_cost_noro_gp_all_ages = discounted_under5_outcomes$discounted_cost_gp_attendances  + discounted_5_14_outcomes$discounted_cost_gp_attendances  + discounted_15_64_outcomes$discounted_cost_gp_attendances  + discounted_over65_outcomes$discounted_cost_gp_attendances 
  averted_cost_noro_hosp_all_ages = discounted_under5_outcomes$discounted_cost_noro_hospitalisations  + discounted_5_14_outcomes$discounted_cost_noro_hospitalisations  + discounted_15_64_outcomes$discounted_cost_noro_hospitalisations  + discounted_over65_outcomes$discounted_cost_noro_hospitalisations 
  
  averted_cost_all_ages = averted_cost_aki_hosp_all_ages + averted_cost_noro_gp_all_ages + averted_cost_noro_hosp_all_ages
  
  # Calcualte vaccination costs 
  
  vaccine_cost <- discounted_under5_outcomes$discounted_cost_per_dose + discounted_5_14_outcomes$discounted_cost_per_dose + discounted_15_64_outcomes$discounted_cost_per_dose + discounted_over65_outcomes$discounted_cost_per_dose
  admin_cost <- discounted_under5_outcomes$discounted_admin_per_dose + discounted_5_14_outcomes$discounted_admin_per_dose + discounted_15_64_outcomes$discounted_admin_per_dose + discounted_over65_outcomes$discounted_admin_per_dose
  program_cost <- vaccine_cost + admin_cost
  
  # dynamic qaly gain
  
  ## qaly gain under 5
  
  aki_hosp_qaly_gain_under5 = discounted_under5_outcomes$discounted_qaly_loss_aki_hospitalisation 
  noro_hosp_qaly_gain_under5 = discounted_under5_outcomes$discounted_qaly_loss_noro_hospitalisation 
  noro_gp_qaly_gain_under5 = discounted_under5_outcomes$discounted_qaly_loss_gp_attendances 
  noro_mortality_qale_gain_under5 = discounted_under5_outcomes$discounted_qale_loss_noro_mortality 
  aki_mortality_qale_gain_under5 = discounted_under5_outcomes$discounted_qale_loss_aki_mortality 
  
  qaly_gain_under5 = aki_hosp_qaly_gain_under5 + noro_hosp_qaly_gain_under5 + noro_gp_qaly_gain_under5 + noro_mortality_qale_gain_under5 + aki_mortality_qale_gain_under5
  
  ## qaly gain 5 to 14
  
  aki_hosp_qaly_gain_5to14 = discounted_5_14_outcomes$discounted_qaly_loss_aki_hospitalisation 
  noro_hosp_qaly_gain_5to14 = discounted_5_14_outcomes$discounted_qaly_loss_noro_hospitalisation 
  noro_gp_qaly_gain_5to14 = discounted_5_14_outcomes$discounted_qaly_loss_gp_attendances 
  noro_mortality_qale_gain_5to14 = discounted_5_14_outcomes$discounted_qale_loss_noro_mortality 
  aki_mortality_qale_gain_5to14 = discounted_5_14_outcomes$discounted_qale_loss_aki_mortality 
  
  qaly_gain_5to14 = aki_hosp_qaly_gain_5to14 + noro_hosp_qaly_gain_5to14 + noro_gp_qaly_gain_5to14 + noro_mortality_qale_gain_5to14 + aki_mortality_qale_gain_5to14
  
  ## qaly gain 15 to 64
  
  aki_hosp_qaly_gain_15to64 = discounted_15_64_outcomes$discounted_qaly_loss_aki_hospitalisation 
  noro_hosp_qaly_gain_15to64 = discounted_15_64_outcomes$discounted_qaly_loss_noro_hospitalisation 
  noro_gp_qaly_gain_15to64 = discounted_15_64_outcomes$discounted_qaly_loss_gp_attendances 
  noro_mortality_qale_gain_15to64 = discounted_15_64_outcomes$discounted_qale_loss_noro_mortality 
  aki_mortality_qale_gain_15to64 = discounted_15_64_outcomes$discounted_qale_loss_aki_mortality 
  
  qaly_gain_15to64 = aki_hosp_qaly_gain_15to64 + noro_hosp_qaly_gain_15to64 + noro_gp_qaly_gain_15to64 + noro_mortality_qale_gain_15to64 + aki_mortality_qale_gain_15to64
  
  ## qaly gain 65+
  
  aki_hosp_qaly_gain_over65 = discounted_over65_outcomes$discounted_qaly_loss_aki_hospitalisation 
  noro_hosp_qaly_gain_over65 = discounted_over65_outcomes$discounted_qaly_loss_noro_hospitalisation 
  noro_gp_qaly_gain_over65 = discounted_over65_outcomes$discounted_qaly_loss_gp_attendances 
  noro_mortality_qale_gain_over65 = discounted_over65_outcomes$discounted_qale_loss_noro_mortality 
  aki_mortality_qale_gain_over65 = discounted_over65_outcomes$discounted_qale_loss_aki_mortality 
  
  qaly_gain_over65 = aki_hosp_qaly_gain_over65 + noro_hosp_qaly_gain_over65 + noro_gp_qaly_gain_over65 + noro_mortality_qale_gain_over65 + aki_mortality_qale_gain_over65
  
  ## qaly gain all ages
  
  dynamic_qaly_gain_all_ages = qaly_gain_under5 + qaly_gain_5to14 + qaly_gain_15to64 + qaly_gain_over65
  
  #cea
  dynamic_incr_cost = program_cost - averted_cost_all_ages
  
  dynamic_icer = dynamic_incr_cost  / dynamic_qaly_gain_all_ages
  
  dynamic_inmb = wtp * dynamic_qaly_gain_all_ages - dynamic_incr_cost
  
  
  # Create the table if qaly_table is TRUE
  if (qaly_table) {
    qaly_table_data <- data.frame(
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
    
    # Return the table
    return(qaly_table_data)
  }
  
  # Create the cost table if cost_table is TRUE
  if (cost_table) {
    cost_table_data <- data.frame(
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
    
    # Round values and rename columns
    return(cost_table_data)
  }
  
  if (inmb_output) {
    return(inmb_k = round(dynamic_inmb,digits=0))
  }
  
  if (icer_only) {
    return(round(dynamic_icer,digits=0))
  }
  
  # RETURN RESULTS
  return(
    round(c(
      icer = dynamic_icer,
      incr_cost = dynamic_incr_cost,
      qaly_gain = dynamic_qaly_gain_all_ages,
      inmb_k=dynamic_inmb
    ),digits=0))
}

