# cost effective acceptibility curve

ceac_under5_over65_prob_results_inmb_calc <- under5_over65_prob_results$raw_results %>% as.data.frame() %>%  select(incr_cost, qaly_gain)
ceac_under5_prob_results_inmb_calc <- under5_prob_results$raw_results %>% as.data.frame() %>%  select(incr_cost, qaly_gain)
ceac_over65_prob_results_inmb_calc <- over65_prob_results$raw_results %>% as.data.frame() %>%  select(incr_cost, qaly_gain)
ceac_no_aki_under5_over65_prob_results_inmb_calc <- no_aki_under5_over65_prob_results$raw_results %>% as.data.frame() %>%  select(incr_cost, qaly_gain)
ceac_no_aki_under5_prob_results_inmb_calc <- no_aki_under5_prob_results$raw_results %>% as.data.frame() %>%  select(incr_cost, qaly_gain)
ceac_no_aki_over65_prob_results_inmb_calc <- no_aki_over65_prob_results$raw_results %>% as.data.frame() %>%  select(incr_cost, qaly_gain)

# Function to calculate INMB for three specific intervention datasets
calculate_inmb_age_interventions <- function(
    data_both = ceac_under5_over65_prob_results_inmb_calc,
    data_under5 = ceac_under5_prob_results_inmb_calc,
    data_over65 = ceac_over65_prob_results_inmb_calc,
    inc_cost_col = "incr_cost",
    inc_qaly_col = "qaly_gain") {
  
  # Error checking for data frames and columns
  for(df in list(data_both, data_under5, data_over65)) {
    if (!inc_cost_col %in% colnames(df)) {
      stop("Incremental cost column not found in one of the datasets")
    }
    if (!inc_qaly_col %in% colnames(df)) {
      stop("Incremental QALY column not found in one of the datasets")
    }
  }
  
  # Create WTP sequence
  wtp_range <- seq(0, 40000, by = 500)
  n_wtp <- length(wtp_range)
  
  # Get number of iterations
  n_iterations <- nrow(data_both)
  
  # Extract incremental values
  inc_costs <- list(
    both = as.numeric(data_both[, inc_cost_col]),
    under5 = as.numeric(data_under5[, inc_cost_col]),
    over65 = as.numeric(data_over65[, inc_cost_col])
  )
  
  inc_qalys <- list(
    both = as.numeric(data_both[, inc_qaly_col]),
    under5 = as.numeric(data_under5[, inc_qaly_col]),
    over65 = as.numeric(data_over65[, inc_qaly_col])
  )
  
  # Initialize matrices to store INMB for each intervention
  inmb_matrices <- list(
    both = matrix(0, nrow = n_iterations, ncol = n_wtp),
    under5 = matrix(0, nrow = n_iterations, ncol = n_wtp),
    over65 = matrix(0, nrow = n_iterations, ncol = n_wtp)
  )
  
  # Calculate INMB for each intervention and WTP value
  for(i in 1:n_wtp) {
    wtp <- wtp_range[i]
    for(int in c("both", "under5", "over65")) {
      inmb_matrices[[int]][, i] <- (wtp * inc_qalys[[int]]) - inc_costs[[int]]
    }
  }
  
  # Add column names to INMB matrices
  for(int in c("both", "under5", "over65")) {
    colnames(inmb_matrices[[int]]) <- paste0("WTP_", wtp_range)
  }
  
  # Calculate probability of cost-effectiveness
  prob_ce <- matrix(0, nrow = n_wtp, ncol = 4)  # Including baseline
  colnames(prob_ce) <- c("baseline", "both", "under5", "over65")
  
  # For each WTP threshold
  for(i in 1:n_wtp) {
    # Get INMB values for this WTP
    inmb_values <- cbind(
      rep(0, n_iterations),  # Baseline always has INMB of 0
      inmb_matrices$both[,i],
      inmb_matrices$under5[,i],
      inmb_matrices$over65[,i]
    )
    
    # Find which intervention has highest INMB for each iteration
    best_intervention <- apply(inmb_values, 1, which.max)
    
    # Calculate probabilities
    prob_ce[i,] <- table(factor(best_intervention, levels=1:4)) / n_iterations
  }
  
  # Create results data frames with full information
  results_both <- data.frame(
    data_both,
    as.data.frame(inmb_matrices$both)
  )
  
  results_under5 <- data.frame(
    data_under5,
    as.data.frame(inmb_matrices$under5)
  )
  
  results_over65 <- data.frame(
    data_over65,
    as.data.frame(inmb_matrices$over65)
  )
  
  # Return comprehensive results
  return(list(
    inmb_matrices = inmb_matrices,
    probability_ce = data.frame(
      wtp = wtp_range,
      prob_ce
    ),
    full_results = list(
      both = results_both,
      under5 = results_under5,
      over65 = results_over65
    ),
    wtp_values = wtp_range
  ))
}

# Calculate INMB for the three interventions

results <- calculate_inmb_age_interventions()

# Function to plot CEAC

prob_data <- results$probability_ce
  
  # Create more descriptive labels
  scenario_colors <- c(
    "V3 under 5 and 65+" = "#56B4E9",  
    "V1 under 5" =  "#FFA500",          
    "V2 65+" = "#22A884FF",
    "No Vaccination" = "#440154FF"
  )
  
prob_data <- prob_data %>%
    pivot_longer(cols = c("baseline", "both", "under5", "over65"),
                 names_to = "Intervention",
                 values_to = "Probability") %>% 
    mutate(Intervention = case_when(
      Intervention == "baseline" ~ "No Vaccination",
      Intervention == "both" ~ "V3 under 5 and 65+",
      Intervention == "under5" ~ "V1 under 5",
      Intervention == "over65" ~ "V2 65+"
    )) %>%
    mutate(Intervention = factor(Intervention, levels = c("No Vaccination", "V1 under 5", "V2 65+", "V3 under 5 and 65+"))) 
  
ceac_plot <- prob_data %>% 
    ggplot(aes(x = wtp, y = Probability, color = Intervention)) +
    geom_line(size = 1) +
    theme_minimal() +
    labs(x = "Willingness to Pay (£/QALY)",
         y = "Probability Cost-Effective",
         color = "Strategy") +
    scale_y_continuous(limits = c(0, 1)) +
    scale_color_manual(values = scenario_colors) +
    theme(
      legend.position = "top",
      plot.title = element_text(hjust = 0.5),
      panel.grid.minor = element_blank()
    ) +
    scale_x_continuous(labels = scales::comma, 
                       expand = expansion(mult = 0.1))


#################################################################
#################################################################

# Function to calculate INMB for three specific intervention datasets
calculate_inmb_age_interventions_no_aki <- function(
    data_both = ceac_no_aki_under5_over65_prob_results_inmb_calc,
    data_under5 = ceac_no_aki_under5_prob_results_inmb_calc,
    data_over65 = ceac_no_aki_over65_prob_results_inmb_calc,
    inc_cost_col = "incr_cost",
    inc_qaly_col = "qaly_gain") {
  
  # Error checking for data frames and columns
  for(df in list(data_both, data_under5, data_over65)) {
    if (!inc_cost_col %in% colnames(df)) {
      stop("Incremental cost column not found in one of the datasets")
    }
    if (!inc_qaly_col %in% colnames(df)) {
      stop("Incremental QALY column not found in one of the datasets")
    }
  }
  
  # Create WTP sequence
  wtp_range <- seq(0, 400000, by = 500)
  n_wtp <- length(wtp_range)
  
  # Get number of iterations
  n_iterations <- nrow(data_both)
  
  # Extract incremental values
  inc_costs <- list(
    both = as.numeric(data_both[, inc_cost_col]),
    under5 = as.numeric(data_under5[, inc_cost_col]),
    over65 = as.numeric(data_over65[, inc_cost_col])
  )
  
  inc_qalys <- list(
    both = as.numeric(data_both[, inc_qaly_col]),
    under5 = as.numeric(data_under5[, inc_qaly_col]),
    over65 = as.numeric(data_over65[, inc_qaly_col])
  )
  
  # Initialize matrices to store INMB for each intervention
  inmb_matrices <- list(
    both = matrix(0, nrow = n_iterations, ncol = n_wtp),
    under5 = matrix(0, nrow = n_iterations, ncol = n_wtp),
    over65 = matrix(0, nrow = n_iterations, ncol = n_wtp)
  )
  
  # Calculate INMB for each intervention and WTP value
  for(i in 1:n_wtp) {
    wtp <- wtp_range[i]
    for(int in c("both", "under5", "over65")) {
      inmb_matrices[[int]][, i] <- (wtp * inc_qalys[[int]]) - inc_costs[[int]]
    }
  }
  
  # Add column names to INMB matrices
  for(int in c("both", "under5", "over65")) {
    colnames(inmb_matrices[[int]]) <- paste0("WTP_", wtp_range)
  }
  
  # Calculate probability of cost-effectiveness
  prob_ce <- matrix(0, nrow = n_wtp, ncol = 4)  # Including baseline
  colnames(prob_ce) <- c("baseline", "both", "under5", "over65")
  
  # For each WTP threshold
  for(i in 1:n_wtp) {
    # Get INMB values for this WTP
    inmb_values <- cbind(
      rep(0, n_iterations),  # Baseline always has INMB of 0
      inmb_matrices$both[,i],
      inmb_matrices$under5[,i],
      inmb_matrices$over65[,i]
    )
    
    # Find which intervention has highest INMB for each iteration
    best_intervention <- apply(inmb_values, 1, which.max)
    
    # Calculate probabilities
    prob_ce[i,] <- table(factor(best_intervention, levels=1:4)) / n_iterations
  }
  
  # Create results data frames with full information
  results_both <- data.frame(
    data_both,
    as.data.frame(inmb_matrices$both)
  )
  
  results_under5 <- data.frame(
    data_under5,
    as.data.frame(inmb_matrices$under5)
  )
  
  results_over65 <- data.frame(
    data_over65,
    as.data.frame(inmb_matrices$over65)
  )
  
  # Return comprehensive results
  return(list(
    inmb_matrices = inmb_matrices,
    probability_ce = data.frame(
      wtp = wtp_range,
      prob_ce
    ),
    full_results = list(
      both = results_both,
      under5 = results_under5,
      over65 = results_over65
    ),
    wtp_values = wtp_range
  ))
}

results_no_aki <- calculate_inmb_age_interventions_no_aki()

prob_data_no_aki <- results_no_aki$probability_ce

prob_data_no_aki <- prob_data_no_aki %>%
  pivot_longer(cols = c("baseline", "both", "under5", "over65"),
               names_to = "Intervention",
               values_to = "Probability") %>% 
  mutate(Intervention = case_when(
    Intervention == "baseline" ~ "No Vaccination",
    Intervention == "both" ~ "V3 under 5 and 65+",
    Intervention == "under5" ~ "V1 under 5",
    Intervention == "over65" ~ "V2 65+"
  )) %>%
  mutate(Intervention = factor(Intervention, levels = c("No Vaccination", "V1 under 5", "V2 65+", "V3 under 5 and 65+"))) 

ceac_plot_no_aki <- prob_data_no_aki %>% 
  ggplot(aes(x = wtp, y = Probability, color = Intervention)) +
  geom_line(size = 1) +
  theme_minimal() +
  labs(x = "Willingness to Pay (£/QALY)",
       y = "Probability Cost-Effective",
       color = "Strategy") +
  scale_y_continuous(limits = c(0, 1)) +
  scale_color_manual(values = scenario_colors) +
  theme(
    legend.position = "top",
    plot.title = element_text(hjust = 0.5),
    panel.grid.minor = element_blank()
  ) +
  scale_x_continuous(labels = scales::comma, 
                     expand = expansion(mult = 0.1))
### Frontier ceac

# Find the optimal strategy at each WTP threshold
# For each row, identify which strategy has the highest probability

# frontier_data <- results$probability_ce
# 
# frontier_data <- frontier_data %>%
#   rowwise() %>%
#   mutate(optimal = case_when(
#     baseline >= max(both, under5, over65) ~ "baseline",
#     both >= max(baseline, under5, over65) ~ "both",
#     under5 >= max(baseline, both, over65) ~ "under5",
#     TRUE ~ "over65"
#   ))

# ceac_frontier_plot <- prob_data %>% 
#   ggplot(aes(x = wtp, y = Probability, color = Intervention)) +
#   geom_line() +
#   geom_line(data = frontier_data, 
#             aes(y = pmax(baseline, both, under5, over65)),
#             color = "black",
#             linetype = "dashed",
#             size = 1.2) +
#   theme_minimal() +
#   labs(x = "Willingness to Pay (£/QALY)",
#        y = "Probability Cost-Effective",
#        color = "Strategy") +
#   scale_y_continuous(limits = c(0, 1)) +
#   scale_color_manual(values = scenario_colors) +
#   theme(
#     legend.position = "top",
#     plot.title = element_text(hjust = 0.5),
#     panel.grid.minor = element_blank()
#   ) +
#   scale_x_continuous(labels = scales::comma, 
#                      expand = expansion(mult = 0.1))

# Create the frontier data
frontier_data <- prob_data %>%
  group_by(wtp) %>%
  slice_max(order_by = Probability, n = 1) %>%
  ungroup() %>%
  arrange(wtp)  # Make sure data is ordered by wtp

# Plot using both color for intervention and group = 1 to force connection
ceac_frontier_plot <- ggplot(frontier_data, aes(x = wtp, y = Probability, color = Intervention, group = 1)) +
  geom_line(size = 1) +
  theme_minimal() +
  labs(x = "Willingness to Pay (£/QALY)",
       y = "Probability Cost-Effective",
       color = "Strategy") +
  scale_y_continuous(limits = c(0, 1)) +
  scale_color_manual(values = scenario_colors) +
  theme(
    legend.position = "top",
    plot.title = element_text(hjust = 0.5),
    panel.grid.minor = element_blank()
  ) +
  scale_x_continuous(labels = scales::comma, 
                     expand = expansion(mult = 0.1))


###

# frontier_data_no_aki <- results_no_aki$probability_ce
# 
# frontier_data_no_aki <- frontier_data_no_aki %>%
#   rowwise() %>%
#   mutate(optimal = case_when(
#     baseline >= max(both, under5, over65) ~ "baseline",
#     both >= max(baseline, under5, over65) ~ "both",
#     under5 >= max(baseline, both, over65) ~ "under5",
#     TRUE ~ "over65"
#   ))
# 
# ceac_frontier_plot_no_aki <- prob_data_no_aki %>% 
#   ggplot(aes(x = wtp, y = Probability, color = Intervention)) +
#   geom_line() +
#   geom_line(data = frontier_data_no_aki, 
#             aes(y = pmax(baseline, both, under5, over65)),
#             color = "black",
#             linetype = "dashed",
#             size = 1.2) +
#   theme_minimal() +
#   labs(x = "Willingness to Pay (£/QALY)",
#        y = "Probability Cost-Effective",
#        color = "Strategy") +
#   scale_y_continuous(limits = c(0, 1)) +
#   scale_color_manual(values = scenario_colors) +
#   theme(
#     legend.position = "top",
#     plot.title = element_text(hjust = 0.5),
#     panel.grid.minor = element_blank()
#   ) +
#   scale_x_continuous(labels = scales::comma, 
#                      expand = expansion(mult = 0.1))

# Create the frontier data
frontier_data_no_aki <- prob_data_no_aki %>%
  group_by(wtp) %>%
  slice_max(order_by = Probability, n = 1) %>%
  ungroup() %>%
  arrange(wtp)  # Make sure data is ordered by wtp

# Plot using both color for intervention and group = 1 to force connection
ceac_frontier_plot_no_aki <- ggplot(frontier_data_no_aki, aes(x = wtp, y = Probability, color = Intervention, group = 1)) +
  geom_line(size = 1) +
  theme_minimal() +
  labs(x = "Willingness to Pay (£/QALY)",
       y = "Probability Cost-Effective",
       color = "Strategy") +
  scale_y_continuous(limits = c(0, 1)) +
  scale_color_manual(values = scenario_colors) +
  theme(
    legend.position = "top",
    plot.title = element_text(hjust = 0.5),
    panel.grid.minor = element_blank()
  ) +
  scale_x_continuous(labels = scales::comma, 
                     expand = expansion(mult = 0.1))
