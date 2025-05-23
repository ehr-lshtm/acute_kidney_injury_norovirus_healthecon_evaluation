create_tornado_plot <- function(tornado_data, plot_title, y_min = -5000, y_max = 12500) {
  # Calculate deviations and total impact
  plot_data <- tornado_data %>%
    mutate(
      Lower_Deviation = ICER_lower - ICER_base,
      Upper_Deviation = ICER_upper - ICER_base,
      Total_Impact = abs(Lower_Deviation) + abs(Upper_Deviation)
    ) %>%
    # Get top 10 parameters by total impact
    top_n(10, Total_Impact) %>%
    select(Parameter, Lower_Deviation, Upper_Deviation, Total_Impact) %>%
    gather(key = "Deviation_Type", 
           value = "Deviation_Value", 
           Lower_Deviation, Upper_Deviation)
  
  # Create the plot
  tornado_plot <- ggplot(plot_data, 
                         aes(x = reorder(Parameter, Total_Impact),
                             y = Deviation_Value,
                             fill = Deviation_Type)) +
    geom_bar(stat = "identity", width = 0.7) +
    coord_flip() +
    scale_fill_manual(
      values = c("Lower_Deviation" = "#FF9999", "Upper_Deviation" = "#66B2FF"),
      labels = c("Lower Value", "Upper Value")
    ) +
    labs(
      title = plot_title,
      x = "Parameter",
      y = "Change in ICER (£/QALY)",
      fill = "Parameter Value"
    ) +
    theme_minimal() +
    theme(
      legend.position = "right",
      plot.title = element_text(size = 14, face = "bold"),
      axis.text.y = element_text(size = 10),
      axis.text.x = element_text(size = 10),
      axis.title = element_text(size = 12)
    ) +
    geom_hline(yintercept = 0, linetype = "dashed", color = "gray50") +
    scale_y_continuous(
      labels = scales::comma,
      expand = expansion(mult = 0.1),
      limits = c(y_min, y_max)
    )
  
  return(tornado_plot)
}

tornado_plot_under5 <- create_tornado_plot(tornado_data_under5, "Vaccinating under 5", y_min = -5000, y_max = 12000)
tornado_plot_over65 <- create_tornado_plot(tornado_data_over65, "Vaccinating over 65", y_min = -5000, y_max = 12000)
tornado_plot_under5_over65 <- create_tornado_plot(tornado_data_under5_over65, "Vaccinating under 5 and 65+", y_min = -5000, y_max = 12000)

torando_plot_no_aki_under5 <- create_tornado_plot(no_aki_tornado_data_under5, "Vaccinating under 5 without AKI", y_min = -200000, y_max = 600000)
torando_plot_no_aki_over65 <- create_tornado_plot(no_aki_tornado_data_over65, "Vaccinating over 65 without AKI", y_min = -200000, y_max = 600000)
torando_plot_no_aki_under5_over65 <- create_tornado_plot(no_aki_tornado_data_under5_over65, "Vaccinating under 5 and 65+ without AKI", y_min = -200000, y_max = 600000)
