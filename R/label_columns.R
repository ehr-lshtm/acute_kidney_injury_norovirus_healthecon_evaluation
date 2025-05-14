label_columns_update <- function(df) {
  # Define the new column names
  new_colnames <- c("time",
                   # Non-vaccinated compartments (7 per age group)
                   "S1", "S2", "S3", "S4",             # Susceptible
                   "E1", "E2", "E3", "E4",             # Exposed
                   "Is1", "Is2", "Is3", "Is4",         # Infected symptomatic
                   "Ia1", "Ia2", "Ia3", "Ia4",         # Infected asymptomatic
                   "R1", "R2", "R3", "R4",             # Recovered
                   "re_1", "re_2", "re_3", "re_4",     # Re-infections
                   "new_1", "new_2", "new_3", "new_4", # New infections
                   
                   # Vaccine 1 compartments (7 per age group)
                   "S1_v1", "S2_v1", "S3_v1", "S4_v1",         # Vaccinated 1
                   "E1_v1", "E2_v1", "E3_v1", "E4_v1",         # Exposed v1
                   "Is1_v1", "Is2_v1", "Is3_v1", "Is4_v1",     # Infected symptomatic v1
                   "Ia1_v1", "Ia2_v1", "Ia3_v1", "Ia4_v1",     # Infected asymptomatic v1
                   "R1_v1", "R2_v1", "R3_v1", "R4_v1",         # Recovered v1
                   "re_1_v1", "re_2_v1", "re_3_v1", "re_4_v1", # Re-infections v1
                   "new_1_v1", "new_2_v1", "new_3_v1", "new_4_v1", # New infections v1
                   
                   # Vaccine 2 compartments (7 per age group)
                   "S1_v2", "S2_v2", "S3_v2", "S4_v2",         # Vaccinated 2
                   "E1_v2", "E2_v2", "E3_v2", "E4_v2",         # Exposed v2
                   "Is1_v2", "Is2_v2", "Is3_v2", "Is4_v2",     # Infected symptomatic v2
                   "Ia1_v2", "Ia2_v2", "Ia3_v2", "Ia4_v2",     # Infected asymptomatic v2
                   "R1_v2", "R2_v2", "R3_v2", "R4_v2",         # Recovered v2
                   "re_1_v2", "re_2_v2", "re_3_v2", "re_4_v2", # Re-infections v2
                   "new_1_v2", "new_2_v2", "new_3_v2", "new_4_v2", # New infections v2
                   
                   # Vaccination tracking (2 per age group)
                   # "new_vaccinations_v1_SR_1", "new_vaccinations_v1_SR_2", "new_vaccinations_v1_SR_3", "new_vaccinations_v1_SR_4",     # New vaccinations v1
                   # "new_vaccinations_v1_R_1", "new_vaccinations_v1_R_2", "new_vaccinations_v1_R_3", "new_vaccinations_v1_R_4",     # New vaccinations v2
                   "new_vaccinations_v1_S_1", "new_vaccinations_v1_S_2", "new_vaccinations_v1_S_3", "new_vaccinations_v1_S_4",     # New vaccinations v1
                   "new_vaccinations_v1_R_1", "new_vaccinations_v1_R_2", "new_vaccinations_v1_R_3", "new_vaccinations_v1_R_4",     # New vaccinations v2
                   
                   
                   
                   "Scenario")
  
  # Check if the dataframe has the correct number of columns
  expected_cols <- length(new_colnames)
  if (ncol(df) != expected_cols) {
    stop(sprintf("The dataframe must have exactly %d columns (found %d).", 
                expected_cols, ncol(df)))
  }
  
  # Assign the new column names to the dataframe
  colnames(df) <- new_colnames
  
  # Return the modified dataframe
  return(df)
}
# 
# label_columns_update <- function(df) {
#   # Define the new column names
#   new_colnames <- c("time",
#                     # Non-vaccinated compartments (7 per age group)
#                     "S1", "S2", "S3", "S4",             # Susceptible
#                     "E1", "E2", "E3", "E4",             # Exposed
#                     "Is1", "Is2", "Is3", "Is4",         # Infected symptomatic
#                     "Ia1", "Ia2", "Ia3", "Ia4",         # Infected asymptomatic
#                     "R1", "R2", "R3", "R4",             # Recovered
#                     "re_1", "re_2", "re_3", "re_4",     # Re-infections
#                     "new_1", "new_2", "new_3", "new_4", # New infections
#                     
#                     # Vaccine 1 compartments (7 per age group)
#                     "S1_v1", "S2_v1", "S3_v1", "S4_v1",         # Vaccinated 1
#                     "E1_v1", "E2_v1", "E3_v1", "E4_v1",         # Exposed v1
#                     "Is1_v1", "Is2_v1", "Is3_v1", "Is4_v1",     # Infected symptomatic v1
#                     "Ia1_v1", "Ia2_v1", "Ia3_v1", "Ia4_v1",     # Infected asymptomatic v1
#                     "R1_v1", "R2_v1", "R3_v1", "R4_v1",         # Recovered v1
#                     "re_1_v1", "re_2_v1", "re_3_v1", "re_4_v1", # Re-infections v1
#                     "new_1_v1", "new_2_v1", "new_3_v1", "new_4_v1", # New infections v1
#                     
#                     # Vaccine 2 compartments (7 per age group)
#                     "S1_v2", "S2_v2", "S3_v2", "S4_v2",         # Vaccinated 2
#                     "E1_v2", "E2_v2", "E3_v2", "E4_v2",         # Exposed v2
#                     "Is1_v2", "Is2_v2", "Is3_v2", "Is4_v2",     # Infected symptomatic v2
#                     "Ia1_v2", "Ia2_v2", "Ia3_v2", "Ia4_v2",     # Infected asymptomatic v2
#                     "R1_v2", "R2_v2", "R3_v2", "R4_v2",         # Recovered v2
#                     "re_1_v2", "re_2_v2", "re_3_v2", "re_4_v2", # Re-infections v2
#                     "new_1_v2", "new_2_v2", "new_3_v2", "new_4_v2", # New infections v2
#                     
#                     # Vaccination tracking (2 per age group)
#                     "vax1_1_S", "vax1_2_S", "vax1_3_S", "vax1_4_S",     # New vaccinations v1
#                     "vax1_1_R", "vax1_2_R", "vax1_3_R", "vax1_4_R",     # New vaccinations v1
#                   
#                     "Scenario")
#   
#   # Check if the dataframe has the correct number of columns
#   expected_cols <- length(new_colnames)
#   if (ncol(df) != expected_cols) {
#     stop(sprintf("The dataframe must have exactly %d columns (found %d).", 
#                  expected_cols, ncol(df)))
#   }
#   
#   # Assign the new column names to the dataframe
#   colnames(df) <- new_colnames
#   
#   # Return the modified dataframe
#   return(df)
# }
