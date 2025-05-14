calculate_evpi <- function(
    wtp_max = 80000,
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
  wtp_range <- seq(0, wtp_max, by = 1000)
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
  
  # Initialize vectors to store EVPI
  evpi_values <- numeric(n_wtp)
  
  # Calculate INMB for each intervention and WTP value
  for(i in 1:n_wtp) {
    wtp <- wtp_range[i]
    for(int in c("both", "under5", "over65")) {
      inmb_matrices[[int]][, i] <- (wtp * inc_qalys[[int]]) - inc_costs[[int]]
    }
    
    # For EVPI calculation at this WTP
    current_inmb <- cbind(
      rep(0, n_iterations),  # Baseline always has INMB of 0
      inmb_matrices$both[,i],
      inmb_matrices$under5[,i],
      inmb_matrices$over65[,i]
    )
    
    # Calculate EVPI for this WTP
    max_mean_inmb <- max(colMeans(current_inmb))
    perfect_info_value <- mean(apply(current_inmb, 1, max))
    evpi_values[i] <- perfect_info_value - max_mean_inmb
  }
  
  # Add column names to INMB matrices
  for(int in c("both", "under5", "over65")) {
    colnames(inmb_matrices[[int]]) <- paste0("WTP_", wtp_range)
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
    evpi = data.frame(
      wtp = wtp_range,
      evpi = evpi_values
    ),
    full_results = list(
      both = results_both,
      under5 = results_under5,
      over65 = results_over65
    ),
    wtp_values = wtp_range
  ))
}

aki_evpi_data <- calculate_evpi()
evpi_data <- aki_evpi_data$evpi

# Create the plot
evpi_plot <- ggplot(evpi_data, aes(x = wtp, y = evpi/1000000)) +
  geom_line(color = "#2c3e50", size = 1) +
  theme_minimal() +
  labs(
    title = NULL,
    x = "Willingness to Pay (£/QALY)",
    y = "EVPI (million £)") +
  scale_x_continuous(labels = scales::comma,
                     breaks = seq(0, 80000, by = 20000),
                     expand = expansion(mult = 0.1)) +
  scale_y_continuous(labels = scales::comma, 
                     breaks = seq(0, 400, by = 100),  # Changed to show up to 400 million
                     limits = c(0, 400),              # Set explicit limits
                     expand = expansion(mult = 0.1)) +
  theme(
    panel.grid.minor = element_blank(),
    plot.title = element_text(face = "bold"),
    axis.text = element_text(size = 10),
    axis.title = element_text(size = 11)
  )

###

no_aki_evpi_data <- calculate_evpi(wtp_max = 400000,
                                   data_both = ceac_no_aki_under5_over65_prob_results_inmb_calc,
                                   data_under5 = ceac_no_aki_under5_prob_results_inmb_calc,
                                   data_over65 = ceac_no_aki_over65_prob_results_inmb_calc)

no_aki_evpi_data <- no_aki_evpi_data$evpi

evpi_plot_no_aki <- ggplot(no_aki_evpi_data, aes(x = wtp, y = evpi/1000000)) +
  geom_line(color = "#2c3e50", size = 1) +
  theme_minimal() +
  labs(
    title = NULL,
    x = "Willingness to Pay (£/QALY)",
    y = "EVPI (million £)") +
  scale_x_continuous(labels = scales::comma,
                     breaks = seq(0, 400000, by = 100000),
                     expand = expansion(mult = 0.1)) +
  scale_y_continuous(labels = scales::comma, 
                     breaks = seq(0, 400, by = 100),  # Changed to show up to 500 million
                     limits = c(0, 400),              # Set explicit limits
                     expand = expansion(mult = 0.1)) +
  theme(
    panel.grid.minor = element_blank(),
    plot.title = element_text(face = "bold"),
    axis.text = element_text(size = 10),
    axis.title = element_text(size = 11)
  )
