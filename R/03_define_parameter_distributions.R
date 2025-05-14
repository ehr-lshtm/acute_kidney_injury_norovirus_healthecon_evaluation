#### load trace

traceBurnThin_df <- fread("docs/trace/traceBurnThin_df.txt")

########

find_beta_parameters_dist <- function(mean_val, ci_lower, ci_upper) {
  # Calculate approximate standard deviation from CI
  sd_approx <- (ci_upper - ci_lower) / (2 * 1.96)
  var_approx <- sd_approx^2
  s <- (mean_val * (1-mean_val) / var_approx) - 1
  
  # Then solve for alpha and beta
  alpha <- s * mean_val
  beta <- s * (1 - mean_val)
  
  return(list(alpha=alpha, beta=beta))
}


# Example usage for sample data:
posterior_samples <-traceBurnThin_df$aki_hospitalisation_4  # simulate some data
mean_val <- mean(posterior_samples)
ci <- quantile(posterior_samples, c(0.025, 0.975))

# TO GET ANNUAL MEAN DIVIDE BY TWO (BECAUSE THEY ARE SINUSOIDAL MEANS ACROSS THE YEAR)
mean_val <- mean_val/2
ci <- ci/2

parameters_dist <- find_beta_parameters_dist(mean_val, ci[1], ci[2])
aki_hosp_parameter_4_alpha <- as.numeric(parameters_dist$alpha[1])
aki_hosp_parameter_4_beta <- as.numeric(parameters_dist$beta[1])

print(paste("Alpha:", round(parameters_dist$alpha, 1)))
print(paste("Beta:", round(parameters_dist$beta, 1)))

# Verify the fit:
x <- seq(0, 1, length=1000)
hist(posterior_samples/2, freq=FALSE, main="Fitted Beta Distribution")
lines(x, dbeta(x, parameters_dist$alpha, parameters_dist$beta), col="red", lwd=2)
legend("topleft", legend=c("Data", "Fitted Beta"), 
       col=c("grey", "red"), lwd=c(1,2))


######### Norovirus gp attendance 0-4

# Example usage for sample data:
posterior_samples <- traceBurnThin_df$gastro_gp_attend_1 / iid_proportion_noro# simulate some data
mean_val <- mean(posterior_samples)
ci <- quantile(posterior_samples, c(0.025, 0.975))

parameters_dist <- find_beta_parameters_dist(mean_val, ci[1], ci[2])
gp_parameter_1_alpha <- as.numeric(parameters_dist$alpha[1])
gp_parameter_1_beta <- as.numeric(parameters_dist$beta[1])

print(paste("Alpha:", round(parameters_dist$alpha, 1)))
print(paste("Beta:", round(parameters_dist$beta, 1)))

# Verify the fit:
x <- seq(0, 1, length=1000)
hist(posterior_samples, freq=FALSE, main="Fitted Beta Distribution")
lines(x, dbeta(x, parameters_dist$alpha, parameters_dist$beta), col="red", lwd=2)
legend("topleft", legend=c("Data", "Fitted Beta"), 
       col=c("grey", "red"), lwd=c(1,2))

######### Norovirus gp attendance 5-14

# Example usage for sample data:
mean_val <- mean(posterior_samples)
ci <- quantile(posterior_samples, c(0.025, 0.975))

parameters_dist <- find_beta_parameters_dist(mean_val, ci[1], ci[2])
gp_parameter_2_alpha <- as.numeric(parameters_dist$alpha[1])
gp_parameter_2_beta <- as.numeric(parameters_dist$beta[1])

print(paste("Alpha:", round(parameters_dist$alpha, 1)))
print(paste("Beta:", round(parameters_dist$beta, 1)))

######### Norovirus gp attendance 15-64

# Example usage for sample data:
mean_val <- 0.0011
ci <- c(0.0007, 0.0018)

parameters_dist <- find_beta_parameters_dist(mean_val, ci[1], ci[2])
gp_parameter_3_alpha <- parameters_dist$alpha[1]
gp_parameter_3_beta <- parameters_dist$beta[1]

print(paste("Alpha:", round(parameters_dist$alpha, 1)))
print(paste("Beta:", round(parameters_dist$beta, 1)))

######### Norovirus gp attendance 65+

# Example usage for sample data:
mean_val <- 0.0021
ci <- c(0.0011, 0.004)

parameters_dist <- find_beta_parameters_dist(mean_val, ci[1], ci[2])
gp_parameter_4_alpha <- parameters_dist$alpha[1]
gp_parameter_4_beta <- parameters_dist$beta[1]

print(paste("Alpha:", round(parameters_dist$alpha, 1)))
print(paste("Beta:", round(parameters_dist$beta, 1)))

######### Norovirus hospitalisation 0-4 

# Example usage for sample data:
mean_val <- 3.3/1000
ci <- c(2.41/1000, 4.44/1000)

parameters_dist <- find_beta_parameters_dist(mean_val, ci[1], ci[2])
noro_hosp_parameter_1_alpha <- parameters_dist$alpha[1]
noro_hosp_parameter_1_beta <- parameters_dist$beta[1]

print(paste("Alpha:", round(parameters_dist$alpha, 1)))
print(paste("Beta:", round(parameters_dist$beta, 1)))

######### Norovirus hospitalisation 5-14

# Example usage for sample data:
mean_val <- 0.4/1000
ci <- c(0.3/1000, 0.5/1000)

parameters_dist <- find_beta_parameters_dist(mean_val, ci[1], ci[2])
noro_hosp_parameter_2_alpha <- parameters_dist$alpha[1]
noro_hosp_parameter_2_beta <- parameters_dist$beta[1]

print(paste("Alpha:", round(parameters_dist$alpha, 1)))
print(paste("Beta:", round(parameters_dist$beta, 1)))

######### Norovirus hospitalisation 15-64

# Example usage for sample data:
mean_val <- 0.26/1000
ci <- c(0.2/1000, 0.33/1000)

parameters_dist <- find_beta_parameters_dist(mean_val, ci[1], ci[2])
noro_hosp_parameter_3_alpha <- parameters_dist$alpha[1]
noro_hosp_parameter_3_beta <- parameters_dist$beta[1]

print(paste("Alpha:", round(parameters_dist$alpha, 1)))
print(paste("Beta:", round(parameters_dist$beta, 1)))

######### Norovirus hospitalisation 65+

# Example usage for sample data:
posterior_samples <-traceBurnThin_df$gastro_hospitalisation_4  # simulate some data
mean_val <- mean(posterior_samples)
ci <- quantile(posterior_samples, c(0.025, 0.975))

# TO GET ANNUAL MEAN DIVIDE BY TWO (BECAUSE THEY ARE SINUSOIDAL MEANS ACROSS THE YEAR)
mean_val <- mean_val/2
ci <- ci/2

parameters_dist <- find_beta_parameters_dist(mean_val, ci[1], ci[2])
noro_hosp_parameter_4_alpha <- as.numeric(parameters_dist$alpha[1])
noro_hosp_parameter_4_beta <- as.numeric(parameters_dist$beta[1])

print(paste("Alpha:", round(parameters_dist$alpha, 1)))
print(paste("Beta:", round(parameters_dist$beta, 1)))

# Verify the fit:
x <- seq(0, 1, length=1000)
hist(posterior_samples/2, freq=FALSE, main="Fitted Beta Distribution")
lines(x, dbeta(x, parameters_dist$alpha, parameters_dist$beta), col="red", lwd=2)
legend("topleft", legend=c("Data", "Fitted Beta"), 
       col=c("grey", "red"), lwd=c(1,2))

######### AKI mortality 0-4 

# Example usage for sample data:
mean_val <- 3.3/100
ci <- c(2.0/100, 11.7/100)

parameters_dist <- find_beta_parameters_dist(mean_val, ci[1], ci[2])
aki_mortality_parameter_1_alpha <- parameters_dist$alpha[1]
aki_mortality_parameter_1_beta <- parameters_dist$beta[1]

print(paste("Alpha:", round(parameters_dist$alpha, 1)))
print(paste("Beta:", round(parameters_dist$beta, 1)))

######### AKI mortality 5-14

# Example usage for sample data:
mean_val <- 3.3/100
ci <- c(2.0/100, 11.7/100)

parameters_dist <- find_beta_parameters_dist(mean_val, ci[1], ci[2])
aki_mortality_parameter_2_alpha <- parameters_dist$alpha[1]
aki_mortality_parameter_2_beta <- parameters_dist$beta[1]

print(paste("Alpha:", round(parameters_dist$alpha, 1)))
print(paste("Beta:", round(parameters_dist$beta, 1)))

######### AKI mortality 15-64

# Example usage for sample data:
mean_val <- ((2.7+11)/2)/100
ci <- c(1.4/100, 25.4/100)

parameters_dist <- find_beta_parameters_dist(mean_val, ci[1], ci[2])
aki_mortality_parameter_3_alpha <- parameters_dist$alpha[1]
aki_mortality_parameter_3_beta <- parameters_dist$beta[1]

print(paste("Alpha:", round(parameters_dist$alpha, 1)))
print(paste("Beta:", round(parameters_dist$beta, 1)))

######### AKI mortality 65+

# Example usage for sample data:
mean_val <- ((17.5+26.9)/2)/100
ci <- c(11.6/100, 45.0/100)

parameters_dist <- find_beta_parameters_dist(mean_val, ci[1], ci[2])
aki_mortality_parameter_4_alpha <- parameters_dist$alpha[1]
aki_mortality_parameter_4_beta <- parameters_dist$beta[1]

print(paste("Alpha:", round(parameters_dist$alpha, 1)))
print(paste("Beta:", round(parameters_dist$beta, 1)))

######### Noro mortality 0-4

# Example usage for sample data:
mean_val <- 0.00000625
ci <- c(0.0000057, 0.0000067)

parameters_dist <- find_beta_parameters_dist(mean_val, ci[1], ci[2])
noro_mortality_parameter_1_alpha <- parameters_dist$alpha[1]
noro_mortality_parameter_1_beta <- parameters_dist$beta[1]

print(paste("Alpha:", round(parameters_dist$alpha, 1)))
print(paste("Beta:", round(parameters_dist$beta, 1)))

######### Noro mortality 5-64

# Example usage for sample data:
mean_val <- 0.00000466
ci <- c(0.0000043, 0.000005)

parameters_dist <- find_beta_parameters_dist(mean_val, ci[1], ci[2])
noro_mortality_parameter_2_alpha <- parameters_dist$alpha[1]
noro_mortality_parameter_2_beta <- parameters_dist$beta[1]

print(paste("Alpha:", round(parameters_dist$alpha, 1)))
print(paste("Beta:", round(parameters_dist$beta, 1)))

######### Noro mortality 65+

# Example usage for sample data:
mean_val <- 0.000435
ci <- c(0.0004, 0.00047)

parameters_dist <- find_beta_parameters_dist(mean_val, ci[1], ci[2])
noro_mortality_parameter_4_alpha <- parameters_dist$alpha[1]
noro_mortality_parameter_4_beta <- parameters_dist$beta[1]

print(paste("Alpha:", round(parameters_dist$alpha, 1)))
print(paste("Beta:", round(parameters_dist$beta, 1)))

######### QALY distributions GP attendance

# Example usage for sample data:
mean_val <- 1-0.688
ci <- c(1-0.824, 1-0.553)
mean_val = (mean_val/365)*3 # QALY loss per episode with duration of 3 days
ci = (ci/365)*3

parameters_dist <- find_beta_parameters_dist(mean_val, ci[1], ci[2])
qaly_gp_alpha <- parameters_dist$alpha[1]
qaly_gp_beta <- parameters_dist$beta[1]

print(paste("Alpha:", round(parameters_dist$alpha, 1)))
print(paste("Beta:", round(parameters_dist$beta, 1)))

######### QALY distributions noro hospitalisation

# Example usage for sample data:
mean_val <- 1-0.542
ci <- c(1-0.638, 1-0.445)
mean_val = (mean_val/365)*5.7 # QALY loss per episode with duration of 5.7 days
ci = (ci/365)*5.7

parameters_dist <- find_beta_parameters_dist(mean_val, ci[1], ci[2])
qaly_norovirus_alpha <- parameters_dist$alpha[1]
qaly_norovirus_beta <- parameters_dist$beta[1]

print(paste("Alpha:", round(parameters_dist$alpha, 1)))
print(paste("Beta:", round(parameters_dist$beta, 1)))

######### QALY distributions aki hospitalisation

# Example usage for sample data:
mean_val <- 1-0.676
ci <- c(1-1, 1-0.520)
mean_val = (mean_val/365)*12 # QALY loss per episode with duration of 12 days
ci = (ci/365)*12

parameters_dist <- find_beta_parameters_dist(mean_val, ci[1], ci[2])
qaly_aki_alpha <- parameters_dist$alpha[1]
qaly_aki_beta <- parameters_dist$beta[1]

print(paste("Alpha:", round(parameters_dist$alpha, 1)))
print(paste("Beta:", round(parameters_dist$beta, 1)))

# sigma

posterior_samples <-traceBurnThin_df$sigma  # simulate some data
mean_val <- mean(posterior_samples)
ci <- quantile(posterior_samples, c(0.025, 0.975))

parameters_dist <- find_beta_parameters_dist(mean_val, ci[1], ci[2])
sigma_alpha <- as.numeric(parameters_dist$alpha[1])
sigma_beta <- as.numeric(parameters_dist$beta[1])

print(paste("Alpha:", round(parameters_dist$alpha, 1)))
print(paste("Beta:", round(parameters_dist$beta, 1)))

# probT_under5

posterior_samples <-traceBurnThin_df$probT_under5  # simulate some data
mean_val <- mean(posterior_samples)
ci <- quantile(posterior_samples, c(0.025, 0.975))

parameters_dist <- find_beta_parameters_dist(mean_val, ci[1], ci[2])
probT_under5_alpha <- as.numeric(parameters_dist$alpha[1])
probT_under5_beta <- as.numeric(parameters_dist$beta[1])

print(paste("Alpha:", round(parameters_dist$alpha, 1)))
print(paste("Beta:", round(parameters_dist$beta, 1)))

# probT_over5

posterior_samples <-traceBurnThin_df$probT_over5  # simulate some data
mean_val <- mean(posterior_samples)
ci <- quantile(posterior_samples, c(0.025, 0.975))

parameters_dist <- find_beta_parameters_dist(mean_val, ci[1], ci[2])
probT_over5_alpha <- as.numeric(parameters_dist$alpha[1])
probT_over5_beta <- as.numeric(parameters_dist$beta[1])

print(paste("Alpha:", round(parameters_dist$alpha, 1)))
print(paste("Beta:", round(parameters_dist$beta, 1)))

# D_immun

posterior_samples <-traceBurnThin_df$D_immun  # simulate some data
mean_val <- mean(posterior_samples)
sd_val <- stats::sd(posterior_samples)

D_immun_mean <- mean_val
D_immun_sd <- sd_val

# season offset

posterior_samples <- traceBurnThin_df$season_offset*100  # simulate some data
mean_val <- mean(posterior_samples)
sd_val <- stats::sd(posterior_samples)

season_offset_mean <- mean_val
season_offset_sd <- sd_val

# season amp

posterior_samples <- traceBurnThin_df$season_amp*100  # simulate some data
mean_val <- mean(posterior_samples)
sd_val <- stats::sd(posterior_samples)

season_amp_mean <- mean_val
season_amp_sd <- sd_val
