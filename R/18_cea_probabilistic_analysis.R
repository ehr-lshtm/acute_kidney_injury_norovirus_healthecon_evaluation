# probabilistic sensitivity analysis

source("R/reference_costs.R")
# source("R/annualise_data.R")
# source("R/probabilistic_discounting_function.R")
# source("R/probabilistic_cea_function.R")

# Generate probabilistic samples

# qaly_cost_samples <- qaly_cost_probabilistic_sampling(n_iter = n_iter)

# saveRDS(qaly_cost_samples, "results/qaly_cost_samples_list.rds")
# qaly_cost_samples <- readRDS("results/qaly_cost_samples_list.rds")


# Run probabilistic CEA with AKI
under5_over65_prob_results <- cea_dynamic_model_probabilistic(
  data = averted_combo_vaccination,
  n_iterations = n_iter,
  parameter_probabilistic_samples = parameter_probabilistic_samples
)

under5_prob_results <- cea_dynamic_model_probabilistic(
  data = averted_under5_vaccination,
  n_iterations = n_iter,
  parameter_probabilistic_samples = parameter_probabilistic_samples
)

over65_prob_results <- cea_dynamic_model_probabilistic(
  data = averted_over65_vaccination,
  n_iterations = n_iter,
  parameter_probabilistic_samples = parameter_probabilistic_samples
)

# Run probabilistic CEA NOT AKI

no_aki_under5_over65_prob_results <- cea_dynamic_model_probabilistic(
  data = averted_combo_vaccination,
  n_iterations = n_iter,
  parameter_probabilistic_samples = parameter_probabilistic_samples,
  aki_no_outcome = TRUE
)

no_aki_under5_prob_results <- cea_dynamic_model_probabilistic(
  data = averted_under5_vaccination,
  n_iterations = n_iter,
  parameter_probabilistic_samples = parameter_probabilistic_samples,
  aki_no_outcome = TRUE
)

no_aki_over65_prob_results <- cea_dynamic_model_probabilistic(
  data = averted_over65_vaccination,
  n_iterations = n_iter,
  parameter_probabilistic_samples = parameter_probabilistic_samples,
  aki_no_outcome = TRUE
)

# saveRDS(under5_over65_prob_results, "results/under5_over65_prob_results.rds")
# saveRDS(under5_prob_results, "results/under5_prob_results.rds")
# saveRDS(over65_prob_results, "results/over65_prob_results.rds")
# 
# saveRDS(no_aki_under5_over65_prob_results, "results/no_aki_under5_over65_prob_results.rds")
# saveRDS(no_aki_under5_prob_results, "results/no_aki_under5_prob_results.rds")
# saveRDS(no_aki_over65_prob_results, "results/no_aki_over65_prob_results.rds")

# load results

# under5_over65_prob_results <- readRDS("results/under5_over65_prob_results.rds")
# under5_prob_results <- readRDS("results/under5_prob_results.rds")
# over65_prob_results <- readRDS("results/over65_prob_results.rds")
# 
# no_aki_under5_over65_prob_results <- readRDS("results/no_aki_under5_over65_prob_results.rds")
# no_aki_under5_prob_results <- readRDS("results/no_aki_under5_prob_results.rds")
# no_aki_over65_prob_results <- readRDS("results/no_aki_over65_prob_results.rds")


# View results
under5_over65_prob_results_table <- data.frame(under5_over65_prob_results$raw_results) %>% mutate(strategy = "Under 5 & Over 65")
under5_prob_results_table <- data.frame(under5_prob_results$raw_results) %>% mutate(strategy = "Under 5")
over65_prob_results_table <- data.frame(over65_prob_results$raw_results) %>% mutate(strategy = "Over 65")

no_aki_under5_over65_prob_results_table <- data.frame(no_aki_under5_over65_prob_results$raw_results) %>% mutate(strategy = "Under 5 & Over 65")
no_aki_under5_prob_results_table <- data.frame(no_aki_under5_prob_results$raw_results) %>% mutate(strategy = "Under 5")
no_aki_over65_prob_results_table <- data.frame(no_aki_over65_prob_results$raw_results) %>% mutate(strategy = "Over 65")

 # Define the maximum acceptable ICER

scenario_colors <- c(
  "V3 under 5 and 65+" = "#56B4E9",  
  "V1 under 5" =  "#FFA500",          
  "V2 65+" = "#22A884FF"          
)

max_icer <- 20000

cea_mean <- under5_over65_prob_results_table %>% 
  rbind(under5_prob_results_table, over65_prob_results_table) %>%
  group_by(strategy) %>%
  summarise(qaly_gain = mean(qaly_gain)/1000000,
            incr_cost = mean(incr_cost)/1000000)


cea_plane <-  under5_over65_prob_results_table %>% 
  rbind(under5_prob_results_table, over65_prob_results_table) %>% 
  mutate(strategy = case_when(
    strategy == "Under 5 & Over 65" ~ "V3 under 5 and 65+",
    strategy == "Under 5" ~ "V1 under 5",
    strategy == "Over 65" ~ "V2 65+"
  )) %>% 
  mutate(strategy = factor(strategy, 
                           levels = c("V1 under 5", 
                                      "V2 65+", 
                                      "V3 under 5 and 65+"))) %>% 
  ggplot(aes(x = qaly_gain/1000000, y = incr_cost/1000000)) +
  geom_point(aes(color = factor(strategy)), size = 3, alpha = 0.5) +
  geom_point(data = cea_mean, aes(x = qaly_gain, y = incr_cost), size = 5, alpha = 1.0, shape = 4, stroke = 1.5) +
  geom_hline(yintercept = 0, linetype = "solid", color = "gray50") +
  geom_vline(xintercept = 0, linetype = "solid", color = "gray50") +
  geom_abline(slope = max_icer, intercept = 0, color = "black", linetype = "dashed") +
  labs(x = "Incremental QALYs",
       y = "Incremental Cost (£)",
       color = "Strategy") +
  theme_minimal() +
  scale_color_manual( 
    values = scenario_colors,
    labels = c(
      "V3 under 5 and 65+" = "V3 under 5 and 65+",
      "V1 under 5" = "V1 under 5",
      "V2 65+" = "V2 65+"
    )
  ) +
  theme(legend.position = "bottom",
        panel.grid.major = element_line(color = "gray90"),
        panel.grid.minor = element_line(color = "gray95")) +
  scale_x_continuous(labels = scales::comma, 
                     expand = expansion(mult = 0.1)) +
  scale_y_continuous(labels = scales::comma, 
                     expand = expansion(mult = 0.1))

cea_plane <-  under5_over65_prob_results_table %>% 
  rbind(under5_prob_results_table, over65_prob_results_table) %>% 
  mutate(strategy = case_when(
    strategy == "Under 5 & Over 65" ~ "V3 under 5 and 65+",
    strategy == "Under 5" ~ "V1 under 5",
    strategy == "Over 65" ~ "V2 65+"
  )) %>% 
  mutate(strategy = factor(strategy, 
                           levels = c("V1 under 5", 
                                      "V2 65+", 
                                      "V3 under 5 and 65+"))) %>% 
  ggplot(aes(x = qaly_gain/1000000, y = incr_cost/1000000)) +
  geom_point(aes(color = factor(strategy), shape = factor(strategy)), size = 3, alpha = 0.5) +
  geom_point(data = cea_mean, aes(x = qaly_gain, y = incr_cost), size = 5, alpha = 1.0, shape = 4, stroke = 1.5) +
  geom_hline(yintercept = 0, linetype = "solid", color = "gray50") +
  geom_vline(xintercept = 0, linetype = "solid", color = "gray50") +
  geom_abline(slope = max_icer, intercept = 0, color = "black", linetype = "dashed") +
  labs(x = "Incremental QALYs",
       y = "Incremental Cost (£)",
       color = "Strategy",
       shape = "Strategy") +  # Added shape label
  theme_minimal() +
  scale_color_manual( 
    values = scenario_colors,
    labels = c(
      "V3 under 5 and 65+" = "V3 under 5 and 65+",
      "V1 under 5" = "V1 under 5",
      "V2 65+" = "V2 65+"
    )
  ) +
  scale_shape_manual(  # Added shape scale
    values = c(15, 16, 17),  # Circle, triangle, square
    labels = c(
      "V1 under 5" = "V1 under 5",
      "V2 65+" = "V2 65+",
      "V3 under 5 and 65+" = "V3 under 5 and 65+"
    )
  ) +
  theme(legend.position = "bottom",
        panel.grid.major = element_line(color = "gray90"),
        panel.grid.minor = element_line(color = "gray95")) +
  scale_x_continuous(labels = scales::comma, 
                     expand = expansion(mult = 0.1)) +
  scale_y_continuous(labels = scales::comma, 
                     expand = expansion(mult = 0.1))

cea_plane

## no aki
cea_mean_no_aki <- no_aki_under5_prob_results_table %>% 
  rbind(no_aki_over65_prob_results_table, no_aki_under5_over65_prob_results_table)  %>%
  group_by(strategy) %>%
  summarise(qaly_gain = mean(qaly_gain)/10000,
            incr_cost = mean(incr_cost)/10000)


cea_plane_no_aki <- no_aki_under5_prob_results_table %>% 
  rbind(no_aki_over65_prob_results_table, no_aki_under5_over65_prob_results_table) %>% 
  mutate(strategy = case_when(
    strategy == "Under 5 & Over 65" ~ "V3 under 5 and 65+",
    strategy == "Under 5" ~ "V1 under 5",
    strategy == "Over 65" ~ "V2 65+"
  )) %>% 
  mutate(strategy = factor(strategy, 
                           levels = c("V1 under 5", 
                                      "V2 65+", 
                                      "V3 under 5 and 65+"))) %>% 
  ggplot(aes(x = qaly_gain/10000, y = incr_cost/10000)) +
  geom_point(aes(color = factor(strategy), shape = factor(strategy)), size = 3, alpha = 0.5) +
  geom_point(data = cea_mean_no_aki, aes(x = qaly_gain, y = incr_cost), size = 5, alpha = 1.0, shape = 4, stroke = 1.5) +
  geom_hline(yintercept = 0, linetype = "solid", color = "gray50") +
  geom_vline(xintercept = 0, linetype = "solid", color = "gray50") +
  geom_abline(slope = max_icer, intercept = 0, color = "black", linetype = "dashed") +
  labs(x = "Incremental QALYs",
       y = "Incremental Cost (£)",
       color = "Strategy",
       shape = "Strategy") +  # Added shape label
  theme_minimal() +
  scale_color_manual( 
    values = scenario_colors,
    labels = c(
      "V3 under 5 and 65+" = "V3 under 5 and 65+",
      "V1 under 5" = "V1 under 5",
      "V2 65+" = "V2 65+"
    )
  ) +
  scale_shape_manual(  # Added shape scale
    values = c(15, 16, 17),  # Square, circle, triangle - same as first plot
    labels = c(
      "V1 under 5" = "V1 under 5",
      "V2 65+" = "V2 65+",
      "V3 under 5 and 65+" = "V3 under 5 and 65+"
    )
  ) +
  theme(legend.position = "bottom",
        panel.grid.major = element_line(color = "gray90"),
        panel.grid.minor = element_line(color = "gray95")) +
  scale_x_continuous(labels = scales::comma, 
                     expand = expansion(mult = 0.1),
                     limits = c(0,3)) +
  scale_y_continuous(labels = scales::comma, 
                     expand = expansion(mult = 0.1))

cea_plane_no_aki

### No aki, AKI comparison
plot_data_both <- under5_over65_prob_results_table %>% 
  rbind(over65_prob_results_table, under5_prob_results_table, no_aki_under5_over65_prob_results_table, no_aki_under5_prob_results_table, no_aki_over65_prob_results_table) %>% 
  mutate(incr_cost = incr_cost/100000, qaly_gain = qaly_gain/100000) %>% 
  mutate(strategy = case_when(
    strategy == "Under 5 & Over 65" ~ "Vaccinating under 5 and 65+",
    strategy == "Under 5" ~ "Vaccinating under 5",
    strategy == "Over 65" ~ "Vaccinating 65+"
  )) %>% 
  mutate(strategy = factor(strategy, 
                           levels = c("Vaccinating under 5", 
                                      "Vaccinating 65+", 
                                      "Vaccinating under 5 and 65+")))

cea_plane_both <- plot_data_both %>% 
  ggplot(aes(x = qaly_gain, y = incr_cost)) +
  geom_point(aes(color = factor(strategy)), size = 3, alpha = 0.7) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray50") +
  geom_vline(xintercept = 0, linetype = "dashed", color = "gray50") +
  geom_abline(slope = max_icer, intercept = 0, color = "red", linetype = "dashed") +
  labs(x = "Incremental QALYs",
       y = "Incremental Cost (£)",
       color = "Strategy") +
  theme_minimal() +
  scale_color_manual( 
    values = scenario_colors,
    labels = c(
      "Vaccinating under 5 and 65+" = "Vaccinating under 5 and 65+",
      "Vaccinating under 5" = "Vaccinating under 5",
      "Vaccinating 65+" = "Vaccinating 65+"
    )
  ) +
  theme(legend.position = "bottom",
        panel.grid.major = element_line(color = "gray90"),
        panel.grid.minor = element_line(color = "gray95")) +
  scale_x_continuous(labels = scales::comma, 
                     expand = expansion(mult = 0.1)) +
  scale_y_continuous(labels = scales::comma, 
                     expand = expansion(mult = 0.1))

cea_plane_both

