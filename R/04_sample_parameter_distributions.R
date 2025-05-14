parameter_probabilistic_sampling <- function(n_iter = 100,
                                             # seasonality parameters
                                             # season_amp_over65_mean = 1.46, season_amp_over65_sd = 0.2,
                                             season_amp_mean = season_amp_mean, season_amp_sd = season_amp_sd,
                                             season_offset_mean = season_offset_mean, season_offset_sd = season_offset_sd,
                                             D_immun_mean = D_immun_mean, D_immun_sd = D_immun_sd,
                                             # infection parameters
                                             sigma_alpha = sigma_alpha, sigma_beta = sigma_beta,
                                             probT_under5_alpha = probT_under5_alpha, probT_under5_beta = probT_under5_beta,
                                             probT_over5_alpha = probT_over5_alpha, probT_over5_beta = probT_over5_beta,
                                             # GP parameters beta values
                                             gp_parameter_1_alpha = gp_parameter_1_alpha, gp_parameter_1_beta = gp_parameter_1_beta,
                                             gp_parameter_2_alpha = gp_parameter_2_alpha, gp_parameter_2_beta = gp_parameter_2_beta,
                                             gp_parameter_3_alpha = gp_parameter_3_alpha, gp_parameter_3_beta = gp_parameter_3_beta,
                                             gp_parameter_4_alpha = gp_parameter_4_alpha, gp_parameter_4_beta = gp_parameter_4_beta,
                                             # Norovirus hospitalization beta parameters
                                             noro_hosp_parameter_1_alpha = noro_hosp_parameter_1_alpha, noro_hosp_parameter_1_beta = noro_hosp_parameter_1_beta,
                                             noro_hosp_parameter_2_alpha = noro_hosp_parameter_2_alpha, noro_hosp_parameter_2_beta = noro_hosp_parameter_2_beta,
                                             noro_hosp_parameter_3_alpha = noro_hosp_parameter_3_alpha, noro_hosp_parameter_3_beta = noro_hosp_parameter_3_beta,
                                             noro_hosp_parameter_4_alpha = noro_hosp_parameter_4_alpha, noro_hosp_parameter_4_beta = noro_hosp_parameter_4_beta,
                                             # AKI hospitalization beta parameters
                                             aki_hosp_parameter_1_alpha = 1, aki_hosp_parameter_1_beta = 999,
                                             aki_hosp_parameter_2_alpha = 1, aki_hosp_parameter_2_beta = 999,
                                             aki_hosp_parameter_3_alpha = 1, aki_hosp_parameter_3_beta = 999,
                                             aki_hosp_parameter_4_alpha = aki_hosp_parameter_4_alpha, aki_hosp_parameter_4_beta = aki_hosp_parameter_4_beta,
                                             # Norovirus mortality beta parameters
                                             noro_mortality_parameter_1_alpha = noro_mortality_parameter_1_alpha, noro_mortality_parameter_1_beta = noro_mortality_parameter_1_beta,
                                             noro_mortality_parameter_2_alpha = noro_mortality_parameter_2_alpha, noro_mortality_parameter_2_beta = noro_mortality_parameter_2_beta,
                                             noro_mortality_parameter_3_alpha = noro_mortality_parameter_2_alpha, noro_mortality_parameter_3_beta = noro_mortality_parameter_2_beta,
                                             noro_mortality_parameter_4_alpha = noro_mortality_parameter_4_alpha, noro_mortality_parameter_4_beta = noro_mortality_parameter_4_beta,
                                             # AKI mortality beta parameters
                                             aki_mortality_parameter_1_alpha = aki_mortality_parameter_1_alpha, aki_mortality_parameter_1_beta = aki_mortality_parameter_1_beta,
                                             aki_mortality_parameter_2_alpha = aki_mortality_parameter_2_alpha, aki_mortality_parameter_2_beta = aki_mortality_parameter_2_beta,
                                             aki_mortality_parameter_3_alpha = aki_mortality_parameter_3_alpha, aki_mortality_parameter_3_beta = aki_mortality_parameter_3_beta,
                                             aki_mortality_parameter_4_alpha = aki_mortality_parameter_4_alpha, aki_mortality_parameter_4_beta = aki_mortality_parameter_4_beta,
                                             # QALYs beta parameters
                                             qaly_gp_alpha = qaly_gp_alpha, qaly_gp_beta = qaly_gp_beta,
                                             qaly_norovirus_alpha = qaly_norovirus_alpha, qaly_norovirus_beta = qaly_norovirus_beta,
                                             qaly_aki_alpha = qaly_aki_alpha, qaly_aki_beta = qaly_aki_beta,
                                             # Costs lognormal parameters
                                             cost_gp_mean = 49, cost_gp_sd = 7.35,
                                             cost_norovirus_1_mean = 1045, cost_norovirus_1_sd = 779,
                                             cost_norovirus_2_mean = 1749, cost_norovirus_2_sd = 2157,
                                             cost_aki_mean = 3730, cost_aki_sd = 2132,
                                             # cost_per_dose_mean = 35, cost_per_dose_sd = 20,
                                             admin_cost_per_dose_1_mean = 18.20, admin_cost_per_dose_1_sd = 4.15,
                                             admin_cost_per_dose_2_mean = 14.05, admin_cost_per_dose_2_sd = 3.39
) {
  
  # Helper functions
  lognorm_location <- function(mean, sd) { log(mean^2 / sqrt(sd^2 + mean^2)) }
  lognorm_shape <- function(mean, sd) { sqrt(log(1 + (sd^2 / mean^2))) }

  # Create LHS matrix
  LHS_samples <- lhs::randomLHS(n = n_iter, k = 35)
  
  # Define parameters
  params <- list(
    seasonality_parameters = list(
      # season_amp_over65 = c("mean" = season_amp_over65_mean, "sd" = season_amp_over65_sd),
      season_amp = c("mean" = season_amp_mean, "sd" = season_amp_sd),
      season_offset = c("mean" = season_offset_mean, "sd" = season_offset_sd),
      D_immun = c("mean" = D_immun_mean, "sd" = D_immun_sd)
    ),
    infection_parameters = list(
      sigma = c("alpha" = sigma_alpha, "beta" = sigma_beta),
      probT_under5 = c("alpha" = probT_under5_alpha, "beta" = probT_under5_beta),
      probT_over5 = c("alpha" = probT_over5_alpha, "beta" = probT_over5_beta)
    ),
    gp_parameters = list(
      gp_parameter_1 = c("alpha" = gp_parameter_1_alpha, "beta" = gp_parameter_1_beta),
      gp_parameter_2 = c("alpha" = gp_parameter_2_alpha, "beta" = gp_parameter_2_beta),
      gp_parameter_3 = c("alpha" = gp_parameter_3_alpha, "beta" = gp_parameter_3_beta),
      gp_parameter_4 = c("alpha" = gp_parameter_4_alpha, "beta" = gp_parameter_4_beta)
    ),
    noro_hosp_parameters = list(
      noro_hosp_parameter_1 = c("alpha" = noro_hosp_parameter_1_alpha, "beta" = noro_hosp_parameter_1_beta),
      noro_hosp_parameter_2 = c("alpha" = noro_hosp_parameter_2_alpha, "beta" = noro_hosp_parameter_2_beta),
      noro_hosp_parameter_3 = c("alpha" = noro_hosp_parameter_3_alpha, "beta" = noro_hosp_parameter_3_beta),
      noro_hosp_parameter_4 = c("alpha" = noro_hosp_parameter_4_alpha, "beta" = noro_hosp_parameter_4_beta)
    ),
    aki_hosp_parameters = list(
      aki_hosp_parameter_1 = c("alpha" = aki_hosp_parameter_1_alpha, "beta" = aki_hosp_parameter_1_beta),
      aki_hosp_parameter_2 = c("alpha" = aki_hosp_parameter_2_alpha, "beta" = aki_hosp_parameter_2_beta),
      aki_hosp_parameter_3 = c("alpha" = aki_hosp_parameter_3_alpha, "beta" = aki_hosp_parameter_3_beta),
      aki_hosp_parameter_4 = c("alpha" = aki_hosp_parameter_4_alpha, "beta" = aki_hosp_parameter_4_beta)
    ),
    noro_mortality_parameters = list(
      noro_mortality_parameter_1 = c("alpha" = noro_mortality_parameter_1_alpha, "beta" = noro_mortality_parameter_1_beta),
      noro_mortality_parameter_2 = c("alpha" = noro_mortality_parameter_2_alpha, "beta" = noro_mortality_parameter_2_beta),
      noro_mortality_parameter_3 = c("alpha" = noro_mortality_parameter_3_alpha, "beta" = noro_mortality_parameter_3_beta),
      noro_mortality_parameter_4 = c("alpha" = noro_mortality_parameter_4_alpha, "beta" = noro_mortality_parameter_4_beta)
    ),
    aki_mortality_parameters = list(
      aki_mortality_parameter_1 = c("alpha" = aki_mortality_parameter_1_alpha, "beta" = aki_mortality_parameter_1_beta),
      aki_mortality_parameter_2 = c("alpha" = aki_mortality_parameter_2_alpha, "beta" = aki_mortality_parameter_2_beta),
      aki_mortality_parameter_3 = c("alpha" = aki_mortality_parameter_3_alpha, "beta" = aki_mortality_parameter_3_beta),
      aki_mortality_parameter_4 = c("alpha" = aki_mortality_parameter_4_alpha, "beta" = aki_mortality_parameter_4_beta)
    ),
    qalys = list(
      gp_attendance = c("alpha" = qaly_gp_alpha, "beta" = qaly_gp_beta),
      norovirus_hosp = c("alpha" = qaly_norovirus_alpha, "beta" = qaly_norovirus_beta),
      aki_hosp = c("alpha" = qaly_aki_alpha, "beta" = qaly_aki_beta)
    ),
    costs = list(
      gp_attendance = c("mean" = cost_gp_mean, "sd" = cost_gp_sd),
      norovirus_hosp_1 = c("mean" = cost_norovirus_1_mean, "sd" = cost_norovirus_1_sd),
      norovirus_hosp_2 = c("mean" = cost_norovirus_2_mean, "sd" = cost_norovirus_2_sd),
      aki_hosp = c("mean" = cost_aki_mean, "sd" = cost_aki_sd),
      admin_per_dose_1 = c("mean" = admin_cost_per_dose_1_mean, "sd" = admin_cost_per_dose_1_sd),
      admin_per_dose_2 = c("mean" = admin_cost_per_dose_2_mean, "sd" = admin_cost_per_dose_2_sd)
    )
  )
  
  # Calculate probabilistic seasonality parameters using normal distribution
  seasonality_parameters_prob <- lapply(seq_along(params$seasonality_parameters), function(i) {
    qnorm(LHS_samples[, i],
          mean = params$seasonality_parameters[[i]]["mean"],
          sd = params$seasonality_parameters[[i]]["sd"])
  })
  names(seasonality_parameters_prob) <- names(params$seasonality_parameters)
  
  # Calculate probabilistic infection parameters using normal distribution
  infection_parameters_prob <- lapply(seq_along(params$infection_parameters), function(i) {
  qbeta(LHS_samples[, i + 2],
        shape1 = params$infection_parameters[[i]]["alpha"],
        shape2 = params$infection_parameters[[i]]["beta"])
  })
  names(infection_parameters_prob) <- names(params$infection_parameters)
  
  # Calculate probabilistic GP parameters
  gp_parameters_prob <- lapply(seq_along(params$gp_parameters), function(i) {
    qbeta(LHS_samples[, i + 6],
          shape1 = params$gp_parameters[[i]]["alpha"],
          shape2 = params$gp_parameters[[i]]["beta"])
  })
  names(gp_parameters_prob) <- names(params$gp_parameters)
  
  # Calculate probabilistic norovirus hospitalization parameters
  noro_hosp_parameters_prob <- lapply(seq_along(params$noro_hosp_parameters), function(i) {
    qbeta(LHS_samples[, i + 10],
          shape1 = params$noro_hosp_parameters[[i]]["alpha"],
          shape2 = params$noro_hosp_parameters[[i]]["beta"])
  })
  names(noro_hosp_parameters_prob) <- names(params$noro_hosp_parameters)
  
  # Calculate probabilistic AKI hospitalization parameters
  aki_hosp_parameters_prob <- lapply(seq_along(params$aki_hosp_parameters), function(i) {
    qbeta(LHS_samples[, i + 14],
          shape1 = params$aki_hosp_parameters[[i]]["alpha"],
          shape2 = params$aki_hosp_parameters[[i]]["beta"])
  })
  names(aki_hosp_parameters_prob) <- names(params$aki_hosp_parameters)
  
  # Calculate probabilistic norovirus mortality parameters
  noro_mortality_parameters_prob <- lapply(seq_along(params$noro_mortality_parameters), function(i) {
    qbeta(LHS_samples[, i + 18],
          shape1 = params$noro_mortality_parameters[[i]]["alpha"],
          shape2 = params$noro_mortality_parameters[[i]]["beta"])
  })
  names(noro_mortality_parameters_prob) <- names(params$noro_mortality_parameters)
  
  # Calculate probabilistic AKI mortality parameters
  aki_mortality_parameters_prob <- lapply(seq_along(params$aki_mortality_parameters), function(i) {
    qbeta(LHS_samples[, i + 22],
          shape1 = params$aki_mortality_parameters[[i]]["alpha"],
          shape2 = params$aki_mortality_parameters[[i]]["beta"])
  })
  names(aki_mortality_parameters_prob) <- names(params$aki_mortality_parameters)
  
  # Calculate probabilistic QALYs using alpha/beta directly
  qalys_prob <- lapply(seq_along(params$qalys), function(i) {
    qbeta(LHS_samples[, i + 26],
          shape1 = params$qalys[[i]]["alpha"],
          shape2 = params$qalys[[i]]["beta"])
  })
  names(qalys_prob) <- names(params$qalys)
  
  # Calculate probabilistic costs
  costs_prob <- lapply(seq_along(params$costs), function(i) {
    qlnorm(LHS_samples[, i + 29],
           meanlog = lognorm_location(params$costs[[i]]["mean"], params$costs[[i]]["sd"]),
           sdlog = lognorm_shape(params$costs[[i]]["mean"], params$costs[[i]]["sd"]))
  })
  names(costs_prob) <- names(params$costs)
  
  # Return results
  list(
    deterministic = params,
    probabilistic = list(
      seasonality_parameters = seasonality_parameters_prob,
      infection_parameters = infection_parameters_prob,
      gp_parameters = gp_parameters_prob,
      noro_hosp_parameters = noro_hosp_parameters_prob,
      aki_hosp_parameters = aki_hosp_parameters_prob,
      noro_mortality_parameters = noro_mortality_parameters_prob,
      aki_mortality_parameters = aki_mortality_parameters_prob,
      qalys = qalys_prob,
      costs = costs_prob
    )
  )
}


parameter_probabilistic_samples <- parameter_probabilistic_sampling(n_iter = n_iter,
                                                                    season_amp_mean = season_amp_mean, season_amp_sd = season_amp_sd,
                                                                    season_offset_mean = season_offset_mean, season_offset_sd = season_offset_sd,
                                                                    D_immun_mean = D_immun_mean, D_immun_sd = D_immun_sd,
                                                                    sigma_alpha = sigma_alpha, sigma_beta = sigma_beta,
                                                                    probT_under5_alpha = probT_under5_alpha, probT_under5_beta = probT_under5_beta,
                                                                    probT_over5_alpha = probT_over5_alpha, probT_over5_beta = probT_over5_beta,
                                                                    # GP parameters beta values
                                                                    gp_parameter_1_alpha = gp_parameter_1_alpha, gp_parameter_1_beta = gp_parameter_1_beta,
                                                                    gp_parameter_2_alpha = gp_parameter_2_alpha, gp_parameter_2_beta = gp_parameter_2_beta,
                                                                    gp_parameter_3_alpha = gp_parameter_3_alpha, gp_parameter_3_beta = gp_parameter_3_beta,
                                                                    gp_parameter_4_alpha = gp_parameter_4_alpha, gp_parameter_4_beta = gp_parameter_4_beta,
                                                                    # Norovirus hospitalization beta parameters
                                                                    noro_hosp_parameter_1_alpha = noro_hosp_parameter_1_alpha, noro_hosp_parameter_1_beta = noro_hosp_parameter_1_beta,
                                                                    noro_hosp_parameter_2_alpha = noro_hosp_parameter_2_alpha, noro_hosp_parameter_2_beta = noro_hosp_parameter_2_beta,
                                                                    noro_hosp_parameter_3_alpha = noro_hosp_parameter_3_alpha, noro_hosp_parameter_3_beta = noro_hosp_parameter_3_beta,
                                                                    noro_hosp_parameter_4_alpha = noro_hosp_parameter_4_alpha, noro_hosp_parameter_4_beta = noro_hosp_parameter_4_beta,
                                                                    # AKI hospitalization beta parameters
                                                                    aki_hosp_parameter_4_alpha = aki_hosp_parameter_4_alpha, aki_hosp_parameter_4_beta = aki_hosp_parameter_4_beta,
                                                                    # Norovirus mortality beta parameters
                                                                    noro_mortality_parameter_1_alpha = noro_mortality_parameter_1_alpha, noro_mortality_parameter_1_beta = noro_mortality_parameter_1_beta,
                                                                    noro_mortality_parameter_2_alpha = noro_mortality_parameter_2_alpha, noro_mortality_parameter_2_beta = noro_mortality_parameter_2_beta,
                                                                    noro_mortality_parameter_3_alpha = noro_mortality_parameter_2_alpha, noro_mortality_parameter_3_beta = noro_mortality_parameter_2_beta,
                                                                    noro_mortality_parameter_4_alpha = noro_mortality_parameter_4_alpha, noro_mortality_parameter_4_beta = noro_mortality_parameter_4_beta,
                                                                    # AKI mortality beta parameters
                                                                    aki_mortality_parameter_1_alpha = aki_mortality_parameter_1_alpha, aki_mortality_parameter_1_beta = aki_mortality_parameter_1_beta,
                                                                    aki_mortality_parameter_2_alpha = aki_mortality_parameter_2_alpha, aki_mortality_parameter_2_beta = aki_mortality_parameter_2_beta,
                                                                    aki_mortality_parameter_3_alpha = aki_mortality_parameter_3_alpha, aki_mortality_parameter_3_beta = aki_mortality_parameter_3_beta,
                                                                    aki_mortality_parameter_4_alpha = aki_mortality_parameter_4_alpha, aki_mortality_parameter_4_beta = aki_mortality_parameter_4_beta,
                                                                    # QALYs beta parameters
                                                                    qaly_gp_alpha = qaly_gp_alpha, qaly_gp_beta = qaly_gp_beta,
                                                                    qaly_norovirus_alpha = qaly_norovirus_alpha, qaly_norovirus_beta = qaly_norovirus_beta,
                                                                    qaly_aki_alpha = qaly_aki_alpha, qaly_aki_beta = qaly_aki_beta)

# 
# par(mfrow = c(2, 3))
# for (param in names(parameter_probabilistic_samples$probabilistic$seasonality_parameters)) {
#   plot(density(parameter_probabilistic_samples$probabilistic$seasonality_parameters[[param]]),
#        main = paste("Seasoanlity -", param),
#        xlab = "value",
#        ylab = "Density")
# }
# 
# par(mfrow = c(3, 3))
# 
# for (param in names(parameter_probabilistic_samples$probabilistic$infection_parameters)) {
#   plot(density(parameter_probabilistic_samples$probabilistic$infection_parameters[[param]]),
#        main = paste("Infection -", param),
#        xlab = "value",
#        ylab = "Density")
# }
# 
# par(mfrow = c(2, 4))
# 
# for (param in 1:4) {
#   plot(density(parameter_probabilistic_samples$probabilistic$gp_parameters[[param]]),
#        main = paste("GP parameter -", param),
#        ylab = "Density")
# }
# 
# par(mfrow = c(2, 4))
# 
# for (param in 1:4) {
#   plot(density(parameter_probabilistic_samples$probabilistic$noro_hosp_parameters[[param]]),
#        main = paste("Noro hosp parameter -", param),
#        ylab = "Density")
# }
# 
# for (param in 4:4) {
#   plot(density(parameter_probabilistic_samples$probabilistic$aki_hosp_parameters[[param]]),
#        main = paste("AKI hosp parameter -", param),
#        ylab = "Density")
# }
# 
# 
# par(mfrow = c(2, 4))
# 
# for (param in 1:4) {
#   plot(density(parameter_probabilistic_samples$probabilistic$noro_mortality_parameters[[param]]),
#        main = paste("Noro mortality parameter -", param),
#        ylab = "Density")
# }
# 
# for (param in 1:4) {
#   plot(density(parameter_probabilistic_samples$probabilistic$aki_mortality_parameters[[param]]),
#        main = paste("AKI mortality parameter -", param),
#        ylab = "Density")
# }
# 
# par(mfrow = c(2, 3))
# for (qaly_type in names(parameter_probabilistic_samples$probabilistic$qalys)) {
#   plot(density(parameter_probabilistic_samples$probabilistic$qalys[[qaly_type]]),
#        main = paste("QALY -", qaly_type),
#        xlab = "QALY",
#        ylab = "Density")
# }
# 
# par(mfrow = c(3, 3))
# 
# for (cost_type in names(parameter_probabilistic_samples$probabilistic$costs)) {
#   plot(density(parameter_probabilistic_samples$probabilistic$costs[[cost_type]]),
#        main = paste("Cost -", cost_type),
#        xlab = "Cost",
#        ylab = "Density")
# }
# 
