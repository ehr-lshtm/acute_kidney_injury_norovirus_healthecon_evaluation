# reference costing


aki_reference_costs <- fread("docs/costs/aki_reference_costs.csv")

norovirus_reference_costs <- fread("docs/costs/norovirus_reference_costs.csv")

inflate_to_2022_2023 <- 1.073 # inflation based on provisional 2022/2023 NHS cost inflation index 

aki_mean_cost <- aki_reference_costs |>
  summarise(total_cost = sum(total_cost)*inflate_to_2022_2023,
            total_activity = sum(activity),
            mean_cost_per_activity = total_cost/total_activity)

adult_norovirus_mean_cost <- norovirus_reference_costs |>
  filter(type == "adult") %>% 
  summarise(total_cost = sum(total_cost, na.rm = TRUE)*inflate_to_2022_2023,
            total_activity = sum(activity, na.rm = TRUE),
            mean_cost_per_activity = total_cost/total_activity)

child_norovirus_mean_cost <- norovirus_reference_costs |>
  filter(type == "paediatric") %>% 
  summarise(total_cost = sum(total_cost, na.rm = TRUE)*inflate_to_2022_2023,
            total_activity = sum(activity, na.rm = TRUE),
            mean_cost_per_activity = total_cost/total_activity)

cost_per_aki_hospitalisation_episode = aki_mean_cost$mean_cost_per_activity # gbp
cost_per_adult_noro_hospitalisation_episode = adult_norovirus_mean_cost$mean_cost_per_activity # gbp 
cost_per_child_noro_hospitalisation_episode = child_norovirus_mean_cost$mean_cost_per_activity # gbp 
cost_per_noro_gp_attendance_episode = 49 # gbp 

# vaccine administation costs inflated

# Function to inflate costs over multiple years
inflate_costs <- function(base_cost, inflation_rates) {
  
  inflated_cost <- base_cost
  
  for (rate in inflation_rates) {
    inflated_cost <- inflated_cost * (1 + rate)
  }
  
  return(inflated_cost)
}

inflation_rates <- c(0.0124, 0.0160, 0.0214, 0.0249, 0.0257, 0.0703)

admin_cost_per_dose_children <- inflate_costs(18.20, inflation_rates)
admin_cost_per_dose_adult <- inflate_costs(14.05, inflation_rates)

admin_cost_per_dose_children_sd <- inflate_costs(4.64, inflation_rates)
admin_cost_per_dose_adult_sd <- inflate_costs(4.64, inflation_rates)