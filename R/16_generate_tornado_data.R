## generate tornado data


generate_tornado_data <- function(data, aki_no_outcome) {

base_icer <- cea_one_way_analysis_function(data = data, aki_no_outcome = aki_no_outcome)

lower_aki_qaly_icer <- cea_one_way_analysis_function (data = data, aki_no_outcome = aki_no_outcome, qaly_aki_hospitalisation = (((1-0.520)/365)*12))
upper_aki_qaly_icer <- cea_one_way_analysis_function (data = data, aki_no_outcome = aki_no_outcome, qaly_aki_hospitalisation = (((1-0.99)/365)*12))
lower_noro_hosp_qaly_icer <- cea_one_way_analysis_function (data = data, aki_no_outcome = aki_no_outcome, qaly_noro_hospitalisation = (((1-0.445)/365)*5.7))
upper_noro_hosp_qaly_icer <- cea_one_way_analysis_function (data = data, aki_no_outcome = aki_no_outcome, qaly_noro_hospitalisation = (((1-0.638)/365)*5.7))
lower_noro_gp_qaly_icer <- cea_one_way_analysis_function (data = data, aki_no_outcome = aki_no_outcome, qaly_noro_gp_attendance = (((1-0.553)/365)*3))
upper_noro_gp_qaly_icer <- cea_one_way_analysis_function (data = data, aki_no_outcome = aki_no_outcome, qaly_noro_gp_attendance = (((1-0.824)/365)*3))

lower_gp_cost <- cea_one_way_analysis_function (data = data, aki_no_outcome = aki_no_outcome, cost_noro_gp_attendance = 36.75)
upper_gp_cost <- cea_one_way_analysis_function (data = data, aki_no_outcome = aki_no_outcome, cost_noro_gp_attendance = 61.25)
lower_child_hosp_cost <- cea_one_way_analysis_function (data = data, aki_no_outcome = aki_no_outcome, cost_per_child_noro_hospitalisation_episode = 543)
upper_child_hosp_cost <- cea_one_way_analysis_function (data = data, aki_no_outcome = aki_no_outcome, cost_per_child_noro_hospitalisation_episode = 3600)
lower_adult_hosp_cost <- cea_one_way_analysis_function (data = data, aki_no_outcome = aki_no_outcome, cost_per_adult_noro_hospitalisation_episode = 403)
upper_adult_hosp_cost <- cea_one_way_analysis_function (data = data, aki_no_outcome = aki_no_outcome, cost_per_adult_noro_hospitalisation_episode = 8855)
lower_aki_hosp_cost <- cea_one_way_analysis_function (data = data, aki_no_outcome = aki_no_outcome, cost_per_aki_hospitalisation_episode = 2434)
upper_aki_hosp_cost <- cea_one_way_analysis_function (data = data, aki_no_outcome = aki_no_outcome, cost_per_aki_hospitalisation_episode = 10787)
lower_cost_per_dose <- cea_one_way_analysis_function (data = data, aki_no_outcome = aki_no_outcome, cost_per_dose = 0)
upper_cost_per_dose <- cea_one_way_analysis_function (data = data, aki_no_outcome = aki_no_outcome, cost_per_dose = 100)
lower_admin_per_dose_child <- cea_one_way_analysis_function (data = data, aki_no_outcome = aki_no_outcome, admin_per_dose_child = 9.71)
upper_admin_per_dose_child <- cea_one_way_analysis_function (data = data, aki_no_outcome = aki_no_outcome, admin_per_dose_child = 25.97)
lower_admin_per_dose_adult <- cea_one_way_analysis_function (data = data, aki_no_outcome = aki_no_outcome, admin_per_dose_adult = 7.59)
upper_admin_per_dose_adult <- cea_one_way_analysis_function (data = data, aki_no_outcome = aki_no_outcome, admin_per_dose_adult = 25.97)

lower_gp_pameter_1 <- cea_one_way_analysis_function (data = data, aki_no_outcome = aki_no_outcome, gp_parameter_1 = 0.043)
upper_gp_pameter_1 <- cea_one_way_analysis_function (data = data, aki_no_outcome = aki_no_outcome, gp_parameter_1 = 0.051)
lower_gp_pameter_2 <- cea_one_way_analysis_function (data = data, aki_no_outcome = aki_no_outcome, gp_parameter_2 = 0.032)
upper_gp_pameter_2 <- cea_one_way_analysis_function (data = data, aki_no_outcome = aki_no_outcome, gp_parameter_2 = 0.039)
lower_gp_pameter_3 <- cea_one_way_analysis_function (data = data, aki_no_outcome = aki_no_outcome, gp_parameter_3 = 0.0007)
upper_gp_pameter_3 <- cea_one_way_analysis_function (data = data, aki_no_outcome = aki_no_outcome, gp_parameter_3 = 0.0018)
lower_gp_pameter_4 <- cea_one_way_analysis_function (data = data, aki_no_outcome = aki_no_outcome, gp_parameter_4 = 0.0011)
upper_gp_pameter_4 <- cea_one_way_analysis_function (data = data, aki_no_outcome = aki_no_outcome, gp_parameter_4 = 0.004)

lower_noro_hosp_parameter_1 <- cea_one_way_analysis_function (data = data, aki_no_outcome = aki_no_outcome, noro_hosp_parameter_1 = 0.00241)
upper_noro_hosp_parameter_1 <- cea_one_way_analysis_function (data = data, aki_no_outcome = aki_no_outcome, noro_hosp_parameter_1 = 0.0044)
lower_noro_hosp_parameter_2 <- cea_one_way_analysis_function (data = data, aki_no_outcome = aki_no_outcome, noro_hosp_parameter_2 = 0.0003)
upper_noro_hosp_parameter_2 <- cea_one_way_analysis_function (data = data, aki_no_outcome = aki_no_outcome, noro_hosp_parameter_2 = 0.0005)
lower_noro_hosp_parameter_3 <- cea_one_way_analysis_function (data = data, aki_no_outcome = aki_no_outcome, noro_hosp_parameter_3 = 0.0002)
upper_noro_hosp_parameter_3 <- cea_one_way_analysis_function (data = data, aki_no_outcome = aki_no_outcome, noro_hosp_parameter_3 = 0.00033)
lower_noro_hosp_parameter_4 <- cea_one_way_analysis_function (data = data, aki_no_outcome = aki_no_outcome, noro_hosp_parameter_4 = 0.033)
upper_noro_hosp_parameter_4 <- cea_one_way_analysis_function (data = data, aki_no_outcome = aki_no_outcome, noro_hosp_parameter_4 = 0.114)
lower_aki_parameter_4 <- cea_one_way_analysis_function (data = data, aki_no_outcome = aki_no_outcome, aki_hosp_parameter_4 = 0.179)
upper_aki_parameter_4 <- cea_one_way_analysis_function (data = data, aki_no_outcome = aki_no_outcome, aki_hosp_parameter_4 = 0.363)
lower_noro_mortality_1 <- cea_one_way_analysis_function (data = data, aki_no_outcome = aki_no_outcome, noro_morality_parameter_1 = 0.0000057)
upper_noro_mortality_1 <- cea_one_way_analysis_function (data = data, aki_no_outcome = aki_no_outcome, noro_morality_parameter_1 = 0.0000067)
lower_noro_mortality_2 <- cea_one_way_analysis_function (data = data, aki_no_outcome = aki_no_outcome, noro_morality_parameter_2 = 0.0000043)
upper_noro_mortality_2 <- cea_one_way_analysis_function (data = data, aki_no_outcome = aki_no_outcome, noro_morality_parameter_2 = 0.000005)
lower_noro_mortality_3 <- cea_one_way_analysis_function (data = data, aki_no_outcome = aki_no_outcome, noro_morality_parameter_3 = 0.0000043)
upper_noro_mortality_3 <- cea_one_way_analysis_function (data = data, aki_no_outcome = aki_no_outcome, noro_morality_parameter_3 = 0.000005)
lower_noro_mortality_4 <- cea_one_way_analysis_function (data = data, aki_no_outcome = aki_no_outcome, noro_morality_parameter_4 = 0.0004)
upper_noro_mortality_4 <- cea_one_way_analysis_function (data = data, aki_no_outcome = aki_no_outcome, noro_morality_parameter_4 = 0.00047)
lower_aki_mortality_4 <- cea_one_way_analysis_function (data = data, aki_no_outcome = aki_no_outcome, aki_mortality_parameter_4 = 0.116)
upper_aki_mortality_4 <- cea_one_way_analysis_function (data = data, aki_no_outcome = aki_no_outcome, aki_mortality_parameter_4 = 0.845)
lower_discounting <- cea_one_way_analysis_function (data = data, aki_no_outcome = aki_no_outcome, qaly_discount_rate = 0, cost_discount_rate = 0)
upper_discounting <- cea_one_way_analysis_function (data = data, aki_no_outcome = aki_no_outcome, qaly_discount_rate = 0.015, cost_discount_rate = 0.015)

data_name <- deparse(substitute(data))

immunity_lower_data <- switch(data_name,
                              "mean_averted_under5_vaccination" = mean_averted_under_5_vaccination_6mo_immunity,
                              "mean_averted_over65_vaccination" = mean_averted_over65_vaccination_6mo_immunity,
                              "mean_averted_combo_vaccination" = mean_averted_combo_vaccination_6mo_immunity
)

immunity_upper_data <- switch(data_name,
                              "mean_averted_under5_vaccination" = mean_averted_under_5_vaccination_9yr_immunity,
                              "mean_averted_over65_vaccination" = mean_averted_over65_vaccination_9yr_immunity,
                              "mean_averted_combo_vaccination" = mean_averted_combo_vaccination_9yr_immunity
)

ve_lower_data <- switch(data_name,
                        "mean_averted_under5_vaccination" = mean_averted_under_5_vaccination_30_VE,
                        "mean_averted_over65_vaccination" = mean_averted_over65_vaccination_30_VE,
                        "mean_averted_combo_vaccination" = mean_averted_combo_vaccination_30_VE
)

ve_upper_data <- switch(data_name,
                        "mean_averted_under5_vaccination" = mean_averted_under_5_vaccination_90_VE,
                        "mean_averted_over65_vaccination" = mean_averted_over65_vaccination_90_VE,
                        "mean_averted_combo_vaccination" = mean_averted_combo_vaccination_90_VE
)

lower_immunity <- cea_one_way_analysis_function(data = immunity_lower_data, aki_no_outcome = aki_no_outcome)
upper_immunity <- cea_one_way_analysis_function(data = immunity_upper_data, aki_no_outcome = aki_no_outcome)

lower_vaccine_efficacy <- cea_one_way_analysis_function(data = ve_lower_data, aki_no_outcome = aki_no_outcome)
upper_vaccine_efficacy <- cea_one_way_analysis_function(data = ve_upper_data, aki_no_outcome = aki_no_outcome)

# create data frame

# Create the data frame with all parameters
tornado_data <- data.frame(
  Parameter = c(
    "AKI QALY (0.520-0.99)",
    "Norovirus Hosp QALY (0.445-0.638)",
    "Norovirus GP QALY (0.553-0.824)",
    "GP Visit Cost (£36.75-£61.25)",
    "Child Norovirus Hosp Cost (£543-£3600)",
    "Adult Noroviurs Hosp Cost (£403-£8855)",
    "AKI Hosp Cost (£2434-£10787)",
    "Vaccine Cost (£0-£100)",
    "Child Admin Cost (£9.71-£25.97)",
    "Adult Admin Cost (£7.59-£25.97)",
    "GP Attend 0-4 (0.043-0.051)",
    "GP Attend 5-14 (0.032-0.039)",
    "GP Attend 15-64 (0.0007-0.0018)",
    "GP Attend 65+ (0.0011-0.004)",
    "Norovirus Hosp 0-4 (0.00241-0.0044)",
    "Norovirus Hosp 5-14 (0.0003-0.0005)",
    "Norovirus Hosp 15-64 (0.0002-0.00033)",
    "Norovirus Hosp 65+ (0.033-0.114)",
    "AKI Hosp 65+ (0.179-0.363)",
    "Noro Mortality 0-4 (0.0000057-0.0000067)",
    "Noro Mortality 5-14 (0.0000043-0.00005)",
    "Noro Mortality 15-64 (0.0000043-0.00005)",
    "Noro Mortality 65+ (0.0004-0.00047)",
    "AKI Mortality 65+ (0.116-0.845)",
    "Vaccine Efficacy (30-90%)",
    "Immunity Duration (6 mo-8 yrs)",
    "Discounting (0-1.5%)"
  ),
  ICER_lower = c(
    lower_aki_qaly_icer,
    lower_noro_hosp_qaly_icer,
    lower_noro_gp_qaly_icer,
    lower_gp_cost,
    lower_child_hosp_cost,
    lower_adult_hosp_cost,
    lower_aki_hosp_cost,
    lower_cost_per_dose,
    lower_admin_per_dose_child,
    lower_admin_per_dose_adult,
    lower_gp_pameter_1,
    lower_gp_pameter_2,
    lower_gp_pameter_3,
    lower_gp_pameter_4,
    lower_noro_hosp_parameter_1,
    lower_noro_hosp_parameter_2,
    lower_noro_hosp_parameter_3,
    lower_noro_hosp_parameter_4,
    lower_aki_parameter_4,
    lower_noro_mortality_1,
    lower_noro_mortality_2,
    lower_noro_mortality_3,
    lower_noro_mortality_4,
    lower_aki_mortality_4,
    lower_vaccine_efficacy,
    lower_immunity,
    lower_discounting
  ),
  ICER_upper = c(
    upper_aki_qaly_icer,
    upper_noro_hosp_qaly_icer,
    upper_noro_gp_qaly_icer,
    upper_gp_cost,
    upper_child_hosp_cost,
    upper_adult_hosp_cost,
    upper_aki_hosp_cost,
    upper_cost_per_dose,
    upper_admin_per_dose_child,
    upper_admin_per_dose_adult,
    upper_gp_pameter_1,
    upper_gp_pameter_2,
    upper_gp_pameter_3,
    upper_gp_pameter_4,
    upper_noro_hosp_parameter_1,
    upper_noro_hosp_parameter_2,
    upper_noro_hosp_parameter_3,
    upper_noro_hosp_parameter_4,
    upper_aki_parameter_4,
    upper_noro_mortality_1,
    upper_noro_mortality_2,
    upper_noro_mortality_3,
    upper_noro_mortality_4,
    upper_aki_mortality_4,
    upper_vaccine_efficacy,
    upper_immunity,
    upper_discounting
  ),
  ICER_base = base_icer  # Assuming base_icer is defined
)

return(tornado_data)
}

tornado_data_under5 <- generate_tornado_data(data = mean_averted_under5_vaccination, aki_no_outcome = FALSE)
tornado_data_over65 <- generate_tornado_data(data = mean_averted_over65_vaccination, aki_no_outcome = FALSE)
tornado_data_under5_over65 <- generate_tornado_data(data = mean_averted_combo_vaccination, aki_no_outcome = FALSE)

no_aki_tornado_data_under5 <- generate_tornado_data(data = mean_averted_under5_vaccination, aki_no_outcome = TRUE)
no_aki_tornado_data_over65 <- generate_tornado_data(data = mean_averted_over65_vaccination, aki_no_outcome = TRUE)
no_aki_tornado_data_under5_over65 <- generate_tornado_data(data = mean_averted_combo_vaccination, aki_no_outcome = TRUE)
