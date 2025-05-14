# initial conditions and parameters

# init <- c(
#   3857263, 8103718, 42460865, 12374961, # S
#   100, 0, 0, 0, # E
#   0, 0, 0, 0, # Is
#   0, 0, 0, 0, # Ia
#   0, 0, 0, 0, # R
#   0, 0, 0, 0, # new infections
#   0, 0, 0, 0 # reinfections
# )
# 
# row <- no_vaccination[11000,]
# row <- row[,2:29]
# init <- as.vector(row)

init <- c(2030501, 5199938, 31153534, 10344375, #S              (4 values)
          1317.97, 1003.017, 3968.002, 1110.415, #E             (4 values)
          2185.541, 1666.941, 6586.28, 1843.082, #Is           (4 values)
          18856.33, 14568.83, 51809.13, 14005.68, #Ia          (4 values)
          928250.2, 2394201, 10012580, 2862128, #R             (4 values)
          8499504, 5317121, 13508132, 3047449, #re infections   (4 values)
          13074454, 11802986, 45143483, 11957206, #new infections (4 values)
          0,0,0,0, #new_vaccinations_v1                         (4 values)
          0,0,0,0) #new_vaccinations_v2                         (4 values)

# Reshape initial conditions
init_core <- matrix(
  init[1:28],  # First 28 values (S,E,Is,Ia,R,re_infections,new_infections for each age group)
  nrow = 4, ncol = 7
)

# Create vaccination tracking matrix
vax_tracking <- matrix(
  init[29:36],  # Last 8 values (new_vaccinations_v1 and v2 for each age group)
  nrow = 4, ncol = 2
)

# Create empty matrices for vax1 and vax2 states (same structure as init_core)
init_vax1 <- matrix(0, nrow = 4, ncol = 7)
init_vax2 <- matrix(0, nrow = 4, ncol = 7)

# Combine all matrices
init_mat <- cbind(init_core, init_vax1, init_vax2, vax_tracking)
init_mat <- as.numeric(init_mat)

# define parameters
# initial conditions
# 


# 
# 
# init_vax1 <- matrix(0, nrow = 4, ncol = 9)
# init_vax2 <- init_vax1
# init_mat <- matrix(
#   init,
#   nrow = 4, ncol = 9
# )
# 
# init_mat <- cbind(init_mat, init_vax1, init_vax2)
# init_mat <- as.numeric(init_mat)

age_groups <- c(0, 5, 15, 65)
polymod <- socialmixr::polymod
UK_structure <- socialmixr::contact_matrix(
  polymod,
  countries = "United Kingdom",
  age.limits = c(age_groups),
  symmetric = TRUE
)

# Symmetrical contact matrix
uk_contact_rate_matrix <- as.matrix(UK_structure$matrix)
demography <- UK_structure$demography$population

uk_contact_rate_matrix <- t(t(uk_contact_rate_matrix) / demography)

# add contact matrix to pop
params <- default_parameters()
params[["contacts"]] <- uk_contact_rate_matrix
params[["season_offset"]] <- rep(0.5, length(params[["season_offset"]]))
#params[["vacc_start"]] <- 3535
params[["vacc_start"]] <- 1460

# updated parameters based on fitting data
params[["season_amp_over65"]] <- 1
# params[["sigma"]] <- c(0.82, 0.82/2, 0.82/2)
# params[["D_immun"]] <- 9.01
# params[["probT_under5"]] <- log(0.19)
# params[["probT_over5"]] <- log(0.04)
# params[["season_amp"]] <- 3.9
# params[["season_offset"]] <- c(8.4)


params[["aki_hospitalisation_1"]] <- 0
params[["aki_hospitalisation_2"]] <- 0
params[["aki_hospitalisation_3"]] <- 0
params[["aki_hospitalisation_4"]] <- 0.258

params[["noro_gp_attend_1_iid"]] <- 14.4/1000
params[["noro_gp_attend_2_iid"]] <- 1.5/1000
params[["noro_gp_attend_3_iid"]] <- 1.1/1000
params[["noro_gp_attend_4_iid"]] <- 2.1/1000
params[["gastro_hospitalisation_4"]] <- 0.067
params[["noro_hospitalisation_1_p95"]] <- 3.3/1000
params[["noro_hospitalisation_2_p95"]] <- 0.40/1000
params[["noro_hospitalisation_3_p95"]] <- 0.26/1000
params[["noro_hospitalisation_4_p95"]] <- 1.69/1000

iid_proportion_noro <- 1/(128022/1096190)

params[["noro_gp_attend_1_iid"]]/iid_proportion_noro
params[["noro_gp_attend_2_iid"]]/iid_proportion_noro
params[["noro_gp_attend_3_iid"]]/iid_proportion_noro
params[["noro_gp_attend_4_iid"]]/iid_proportion_noro

params[["gastro_gp_attend_1"]] <- 0.396
params[["gastro_gp_attend_2"]] <- 0.302

params[["noro_gp_attend_1_bolt"]] <- params[["gastro_gp_attend_1"]]/iid_proportion_noro
params[["noro_gp_attend_2_bolt"]] <- params[["gastro_gp_attend_2"]]/iid_proportion_noro

params[["noro_attributable_death_1"]] <- 0.00000625
params[["noro_attributable_death_2"]] <- 0.00000466
params[["noro_attributable_death_3"]] <- 0.00000466
params[["noro_attributable_death_4"]] <- 0.000435

params[["aki_attributable_death_1"]] <- 3.3/100
params[["aki_attributable_death_2"]] <- 3.3/100
params[["aki_attributable_death_3"]] <- 6.9/100
params[["aki_attributable_death_4"]] <- 22.2/100

# time sequence

times <- seq(0, 7185)

# model

# source("R/norovirus_model_r.R")
