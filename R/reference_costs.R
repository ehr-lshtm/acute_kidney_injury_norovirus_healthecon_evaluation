# reference costing


aki_reference_costs <- fread("docs/costs/aki_reference_costs.csv")

norovirus_reference_costs <- fread("docs/costs/norovirus_reference_costs.csv")

aki_mean_cost <- aki_reference_costs |>
  summarise(total_cost = sum(total_cost),
            total_activity = sum(activity),
            mean_cost_per_activity = total_cost/total_activity)

adult_norovirus_mean_cost <- norovirus_reference_costs |>
  filter(type == "adult") %>% 
  summarise(total_cost = sum(total_cost, na.rm = TRUE),
            total_activity = sum(activity, na.rm = TRUE),
            mean_cost_per_activity = total_cost/total_activity)

child_norovirus_mean_cost <- norovirus_reference_costs |>
  filter(type == "paediatric") %>% 
  summarise(total_cost = sum(total_cost, na.rm = TRUE),
            total_activity = sum(activity, na.rm = TRUE),
            mean_cost_per_activity = total_cost/total_activity)

cost_per_aki_hospitalisation_episode = aki_mean_cost$mean_cost_per_activity # gbp
cost_per_adult_noro_hospitalisation_episode = adult_norovirus_mean_cost$mean_cost_per_activity # gbp 
cost_per_child_noro_hospitalisation_episode = child_norovirus_mean_cost$mean_cost_per_activity # gbp 
cost_per_noro_gp_attendance_episode = 49 # gbp 