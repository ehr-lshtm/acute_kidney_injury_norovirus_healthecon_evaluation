# population structure

no_vaccination <- run_vaccination_scenarios(
  params = default_parameters(),
  vaccination_effect = 0.5,
  vaccination_rate = c(0, 0, 0, 0),  # Under 5 vaccination
  immunity_waning = c(0, 0),     # 4.5 year immunity
  scenario_label = "No Vaccination",
  n_iterations = 1
)

no_vaccination |> 
  mutate(
    population_1 = S1 + E1 + Is1 + Ia1 + R1,
    population_2 = S2 + E2 + Is2 + Ia2 + R2,
    population_3 = S3 + E3 + Is3 + Ia3 + R3,
    population_4 = S4 + E4 + Is4 + Ia4 + R4
  ) %>%
  group_by(time) %>%
  summarise(
    `Age group 1` = sum(population_1),
    `Age group 2` = sum(population_2),
    `Age group 3` = sum(population_3),
    `Age group 4` = sum(population_4)
  ) %>%
  filter(time == min(time) | time == max(time)) %>%
  mutate(time_point = ifelse(time == min(time), "Start", "End")) %>%
  pivot_longer(cols = starts_with("Age"), names_to = "age_group", values_to = "population") %>%
  select(time_point, age_group, population) |> 
  mutate(time_point = factor(time_point, levels = c("Initial", "Start", "End"))) %>%
  ggplot(aes(x = age_group, y = population, fill = age_group)) +
  geom_col() +
  facet_wrap(~ time_point) +
  labs(title = "Population distribution at start and end of 10 year projection (no vaccination)",
       x = "Age group", y = "Population size", fill = "Age group") +
  theme_bw()

