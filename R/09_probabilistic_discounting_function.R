
##### Probabilistic discounting function #####

discounting_data_probabilistic <- function(data,
                                           probabilistic_discounting = TRUE,
                                           gp_parameter,
                                           noro_hosp_parameter,
                                           noro_morality_parameter,
                                           aki_hosp_parameter,
                                           aki_mortality_parameter,
                                           average_life_expectancy,
                                           qaly_discount_rate,
                                           cost_discount_rate,
                                           cost_factor,
                                           qaly_noro_gp_attendance,
                                           qaly_noro_hospitalisation,
                                           qaly_aki_hospitalisation,
                                           cost_noro_gp_attendance,
                                           cost_noro_hosp,
                                           cost_aki_hosp,
                                           cost_per_dose,
                                           admin_per_dose
) {
  
  qaly_loss_per_noro_gp_attendance_episode = qaly_noro_gp_attendance
  qaly_loss_per_noro_hospitalisiation_episode = qaly_noro_hospitalisation
  qaly_loss_per_aki_hospitalisation_episode = qaly_aki_hospitalisation

  data %>% 
    mutate(discount_years = as.numeric(gsub("S", "", season))) %>% 
    arrange(discount_years) %>%
    mutate(
      qaly_discounted_rate = (1 + qaly_discount_rate) ^ -discount_years,
      cost_discounted_rate = (1 + cost_discount_rate) ^ -discount_years,
      
      # gp attendance
      gp_attendances = total_infections * gp_parameter,
      qaly_loss_gp_attendances = gp_attendances * qaly_loss_per_noro_gp_attendance_episode,
      cost_gp_attendances = gp_attendances * cost_noro_gp_attendance * cost_factor,
      discounted_qaly_loss_gp_attendances = qaly_loss_gp_attendances * qaly_discounted_rate,
      discounted_cost_gp_attendances = cost_gp_attendances * cost_discounted_rate,
      
      # noro hospitalisations
      noro_hospitalisations = total_infections * noro_hosp_parameter,
      qaly_loss_noro_hospitalisation = noro_hospitalisations * qaly_loss_per_noro_hospitalisiation_episode,
      cost_noro_hospitalisations = noro_hospitalisations * cost_noro_hosp * cost_factor,
      discounted_qaly_loss_noro_hospitalisation = qaly_loss_noro_hospitalisation * qaly_discounted_rate,
      discounted_cost_noro_hospitalisations = cost_noro_hospitalisations * cost_discounted_rate,
      
      # noro mortality
      noro_mortality = total_infections * noro_morality_parameter,
      qale_loss_noro_mortality = noro_mortality * average_life_expectancy,
      discounted_qale_loss_noro_mortality = qale_loss_noro_mortality * qaly_discounted_rate,
      
      # aki hospitalisations
      aki_hospitalisations = total_infections * aki_hosp_parameter,
      qaly_loss_aki_hospitalisation = aki_hospitalisations * qaly_loss_per_aki_hospitalisation_episode,
      cost_aki_hospitalisations = aki_hospitalisations * cost_aki_hosp * cost_factor,
      discounted_qaly_loss_aki_hospitalisation = qaly_loss_aki_hospitalisation * qaly_discounted_rate,
      discounted_cost_aki_hospitalisations = cost_aki_hospitalisations * cost_discounted_rate,
      
      # aki mortality
      aki_mortality = aki_hospitalisations * aki_mortality_parameter,
      qale_loss_aki_mortality = aki_mortality * average_life_expectancy,
      discounted_qale_loss_aki_mortality = qale_loss_aki_mortality * qaly_discounted_rate,
      
      # cost per dose
      vaccination_cost = total_vaccinated * cost_per_dose,
      discounted_cost_per_dose = vaccination_cost * cost_discounted_rate,
      
      # admin per dose
      admin_cost = total_vaccinated * admin_per_dose,
      discounted_admin_per_dose = admin_cost * cost_discounted_rate
      
    ) %>%
    select(-season,
           # -Iteration,
           -discount_years,
           -qaly_discounted_rate,
           -cost_discounted_rate) %>%
    # group_by(scenario) %>%
    summarise(across(everything(), sum, na.rm = TRUE)) %>%
    {if(probabilistic_discounting) select(., -Iteration) else .}
    # ungroup() %>%
    # mutate(
    #   scenario = case_when(
    #     scenario == "averted_data_no_vac" ~ "No vaccination",
    #     scenario == "averted_data_under5_over65" ~ "Vaccinating under 5 and 65+",
    #     scenario == "averted_data_under5" ~ "Vaccinating under 5",
    #     scenario == "averted_data_over65" ~ "Vaccinating 65+"
    #   )
    # ) %>%
    # mutate(scenario = factor(
    #   scenario,
    #   levels = c(
    #     "No vaccination",
    #     "Vaccinating 65+",
    #     "Vaccinating under 5",
    #     "Vaccinating under 5 and 65+"
    #   )
    # )) %>%
    # arrange(scenario)
}
