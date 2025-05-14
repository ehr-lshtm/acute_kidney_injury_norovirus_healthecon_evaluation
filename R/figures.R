#' ---
#' title: Vaccination scenarios and cost effectiveness analysis
#' date: "`r format(Sys.time(), '%d %B %Y %H:%M')`"
#' author: Hikaru Bolt
#' output:
#'   html_document:
#'     df_print: paged
#'     highlight: kate
#'     theme: spacelab
#'     toc: yes
#'     toc_float: yes
#'     fig_height: 8
#'     fig_width: 12
#'     mathjax: null
#' ---
#'
#'
# To run this
# rmarkdown::render("R/figures.R", output_dir = ".", intermediates_dir = ".", output_file = paste0("figures", paste0(gsub("-", "", tolower(Sys.Date())), "_", format(Sys.time(), "%H%M"))))

knitr::opts_chunk$set(echo = FALSE,
                      warning = FALSE,
                      message = FALSE)

#+ figures

n_iter = 100
set.seed(1234)

save_plot <- function(plot, filename) {
  ggsave(filename, plot, dpi = 300)
}

save_flextable_as_png <- function(ft, filename, scale = 1, webshot = "webshot2") {
  # Create directory if it doesn't exist
  dir.create(dirname(filename), showWarnings = FALSE, recursive = TRUE)
  
  # Error handling
  if (!inherits(ft, "flextable")) {
    stop("Input must be a flextable object")
  }
  
  # Try to save the flextable
  tryCatch({
    # Check if required packages are installed
    if (!requireNamespace(webshot, quietly = TRUE)) {
      stop(sprintf("Package '%s' is required. Please install it first.", webshot))
    }
    
    save_as_image(
      x = ft,
      path = filename,
      webshot = webshot,
      zoom = scale
    )
    message(sprintf("Flextable successfully saved to %s", filename))
  }, error = function(e) {
    stop(sprintf("Failed to save flextable: %s", e$message))
  })
}

#' # Parameter distributions

source("R/01_setup.R")
source("R/02_initial_parameter_fixed.R")
source("R/03_define_parameter_distributions.R")
source("R/04_sample_parameter_distributions.R")

# saveRDS(parameter_probabilistic_samples, "results/parameter_probabilistic_samples.rds")
# parameter_probabilistic_samples <- readRDS("results/parameter_probabilistic_samples.rds")

par(mfrow = c(3, 3))
for (param in names(parameter_probabilistic_samples$probabilistic$seasonality_parameters)) {
  plot(density(parameter_probabilistic_samples$probabilistic$seasonality_parameters[[param]]),
       main = paste("Seasoanlity -", param),
       xlab = "value",
       ylab = "Density")
}

for (param in names(parameter_probabilistic_samples$probabilistic$infection_parameters)) {
  plot(density(parameter_probabilistic_samples$probabilistic$infection_parameters[[param]]),
       main = paste("Infection -", param),
       xlab = "value",
       ylab = "Density")
}

par(mfrow = c(3, 4))

for (param in 1:4) {
  plot(density(parameter_probabilistic_samples$probabilistic$gp_parameters[[param]]),
       main = paste("GP parameter -", param),
       ylab = "Density")
}

for (param in 1:4) {
  plot(density(parameter_probabilistic_samples$probabilistic$noro_hosp_parameters[[param]]),
       main = paste("Noro hosp parameter -", param),
       ylab = "Density")
}

for (param in 4:4) {
  plot(density(parameter_probabilistic_samples$probabilistic$aki_hosp_parameters[[param]]),
       main = paste("AKI hosp parameter -", param),
       ylab = "Density")
}


par(mfrow = c(2, 4))

for (param in 1:4) {
  plot(density(parameter_probabilistic_samples$probabilistic$noro_mortality_parameters[[param]]),
       main = paste("Noro mortality parameter -", param),
       ylab = "Density")
}

for (param in 1:4) {
  plot(density(parameter_probabilistic_samples$probabilistic$aki_mortality_parameters[[param]]),
       main = paste("AKI mortality parameter -", param),
       ylab = "Density")
}

par(mfrow = c(2, 3))
for (qaly_type in names(parameter_probabilistic_samples$probabilistic$qalys)) {
  plot(density(parameter_probabilistic_samples$probabilistic$qalys[[qaly_type]]),
       main = paste("QALY -", qaly_type),
       xlab = "QALY",
       ylab = "Density")
}

par(mfrow = c(3, 3))

for (cost_type in names(parameter_probabilistic_samples$probabilistic$costs)) {
  plot(density(parameter_probabilistic_samples$probabilistic$costs[[cost_type]]),
       main = paste("Cost -", cost_type),
       xlab = "Cost",
       ylab = "Density")
}


#' # Impact of vaccine efficacy on the number of symptomatic individuals

source("R/05_run_probabilistic_model.R")
source("R/06_trajectories_with_uncertainty.R")

fig_time_series <- annotate_figure(
  ggarrange(
    # First row
    under5_vaccination_under5_plot + xlab(NULL) + ylab(NULL)+ ggtitle("V1 under 5 cases"),
    over65_vaccination_under5_plot + xlab(NULL) + ylab(NULL)+ ggtitle("V2 under 5 cases"),
    under5_over65_vaccination_under5_plot + xlab(NULL) + ylab(NULL)+ ggtitle("V3 under 5 cases"),
    # Second row
    under5_vaccination_5to64_plot + xlab(NULL) + ylab(NULL)+ ggtitle("V1 5 to 64 cases"),
    over65_vaccination_5to64_plot + xlab(NULL) + ylab(NULL)+ ggtitle("V2 5 to 64 cases"),
    under5_over65_vaccination_5to64_plot + xlab(NULL) + ylab(NULL)+ ggtitle("V3 5 to 64 cases"),
    # Third row
    under5_vaccination_over65_plot + xlab(NULL) + ylab(NULL)+ ggtitle("V1 over 65 cases"),
    over65_vaccination_over65_plot + xlab(NULL) + ylab(NULL)+ ggtitle("V2 over 65 cases"),
    under5_over65_vaccination_over65_plot + xlab(NULL) + ylab(NULL)+ ggtitle("V3 over 65 cases"),
    ncol = 3,
    nrow = 3,
    common.legend = TRUE,
    legend = "none",
    # labels = c("Under 5 vaccination", "Over 65 vaccination", "Combination vaccination"),
    font.label = list(size = 12)
  ),
  bottom = text_grob("Time (days) per season (S)"),
  left = text_grob("Number of symptomatic cases", rot = 90)
)

#' A. Symptomatic cases under 5 B. Symptomatic cases between 5 and 64 c. Symptomatic cases over 65

#' ## Percentage of symptomatic cases averted by vaccination scenarios

source("R/07_bar_chart_averted.R")

fig_bar_chart <- annotate_figure(
  ggarrange(
    averted_cases_under5_bar_chart + xlab(NULL) + ylab(NULL) + ggtitle(""), 
    averted_cases_15_64_bar_chart + xlab(NULL) + ylab(NULL) + ggtitle(""), 
    averted_cases_over65_bar_chart + xlab(NULL) + ylab(NULL) + ggtitle(""), 
    ncol = 3, 
    nrow = 1, 
    common.legend = TRUE, 
    legend = "none",
    labels = c("A) Under 5 cases", "B) 5-64 cases", "C) 65+ cases"),
    font.label = list(size = 12)
    # widths = c(1, 1),
    # heights = c(1)
  ),
  bottom = text_grob("Vaccination strategy"),
  left = text_grob("Percentage symptomatic cases averted", rot = 90)
)

# ggarrange(fig_time_series + theme(aspect.ratio = 0.5), fig_bar_chart + theme(aspect.ratio = 0.6), ncol = 1, nrow = 2)
# ggarrange(fig_time_series, fig_bar_chart, ncol = 1, nrow = 2)

fig_time_series

save_plot(fig_time_series, "results/fig_time_series.png")

fig_bar_chart

save_plot(fig_bar_chart, "results/fig_bar_chart.png")

#' A. Averted symptomatic cases under 5 B. Averted symptomatic cases between 5 and 64 C. Averted symptomatic cases over 65 

#' ## Total infections by vaccination strategy

source("R/08_total_health_outcomes_function.R")

combined_age_group_total_table

no_vaccination_incidence_table

incidence_table

save_flextable_as_png(combined_age_group_total_table, "results/combined_age_group_total_table.png")

# ## Total infections averted by vaccination strategy and age group

# combined_age_group_averted_table
# 
# save_flextable_as_png(combined_age_group_averted_table, "results/combined_age_group_total_table.png")


#' ## QALYS gained (discounted)
source("R/09_probabilistic_discounting_function.R")
source("R/10_probabilistic_cea_function.R")
source("R/11_QALYs_mort_discount_UK.R")
source("R/12_annualise_data.R")

# source("R/06_cea_function_one_way_analysis.R")

qalys_tables_run_scenarios <- function(under5_data, over65_data, combo_data, qaly_discount_rate = 0.035, cost_discount_rate = 0.035) {
  
  results <- list()
  data_list <- list(
    "averted_under5_vaccination" = under5_data,
    "averted_over65_vaccination" = over65_data, 
    "averted_combo_vaccination" = combo_data
  )
  
  for (scenario in names(data_list)) {
    qaly_table <- cea_dynamic_model_probabilistic(
      data = data_list[[scenario]],
      n_iterations = n_iter,
      parameter_probabilistic_samples = parameter_probabilistic_samples,
      qaly_discount_rate = qaly_discount_rate,
      cost_discount_rate = cost_discount_rate,
      qaly_table = TRUE
    )
    
    results[[scenario]] <- qaly_table %>%
      group_by(Age_Group) %>%
      summarise(
        gp_attendance = paste0(format(round(mean(gp_attendance)), big.mark = ","), " (", 
                               format(round(quantile(gp_attendance, 0.025)), big.mark = ","), "-",
                               format(round(quantile(gp_attendance, 0.975)), big.mark = ","), ")"),
        noro_hosp = paste0(format(round(mean(noro_hosp)), big.mark = ","), " (",
                           format(round(quantile(noro_hosp, 0.025)), big.mark = ","), "-", 
                           format(round(quantile(noro_hosp, 0.975)), big.mark = ","), ")"),
        noro_mortality = paste0(format(round(mean(noro_mortality)), big.mark = ","), " (",
                                format(round(quantile(noro_mortality, 0.025)), big.mark = ","), "-", 
                                format(round(quantile(noro_mortality, 0.975)), big.mark = ","), ")"),
        aki_hosp = paste0(format(round(mean(aki_hosp)), big.mark = ","), " (",
                          format(round(quantile(aki_hosp, 0.025)), big.mark = ","), "-",
                          format(round(quantile(aki_hosp, 0.975)), big.mark = ","), ")"),
        aki_mortality = paste0(format(round(mean(aki_mortality)), big.mark = ","), " (",
                               format(round(quantile(aki_mortality, 0.025)), big.mark = ","), "-",
                               format(round(quantile(aki_mortality, 0.975)), big.mark = ","), ")")
      ) %>% 
      ungroup() %>% 
      rename("GP attendance" = gp_attendance,
             "Norovirus hospitalisation" = noro_hosp,
             "AKI hospitalisation" = aki_hosp,
             "Norovirus mortality" = noro_mortality,
             "AKI mortality" = aki_mortality)
  }
  return(results)
}

qaly_table_discounted <- qalys_tables_run_scenarios(
  under5_data = averted_under5_vaccination,
  over65_data = averted_over65_vaccination, 
  combo_data = averted_combo_vaccination)

# Add "Section" names as the first row of each table
table_under5 <- qaly_table_discounted[[1]] %>%
  add_row(.before = 1) %>%
  mutate(Vaccination_Group = case_when(
    is.na(Age_Group) ~ "V1 under 5")) %>% 
  select(Vaccination_Group, everything())

table_over65 <- qaly_table_discounted[[2]] %>%
  add_row(.before = 1) %>%
  mutate(Vaccination_Group = case_when(
    is.na(Age_Group) ~ "V2 over 65")) %>% 
  select(Vaccination_Group, everything())

table_both <- qaly_table_discounted[[3]] %>%
  add_row(.before = 1) %>%
  mutate(Vaccination_Group = case_when(
    is.na(Age_Group) ~ "V3 under 5 and over 65")) %>% 
  select(Vaccination_Group, everything())

# Combine all tables
combined_qaly_table <- bind_rows(
  table_under5,
  table_over65,
  table_both
)

# Create and format the combined table
combined_qaly_table <- combined_qaly_table %>%
  rename("Vaccination strategy" = Vaccination_Group,
         "Age group" = Age_Group) %>%
  flextable::flextable() %>%
  autofit() %>%
  theme_zebra() %>%
  fontsize(size = 10, part = "all") %>%
  # Make the first row of each section bold
  bold(i = c(1, nrow(table_under5) + 1, nrow(table_under5) + nrow(table_over65) + 1)) %>%
  # Add some padding
  padding(padding = 3, part = "all") %>%
  # Add borders between sections
  hline(i = c(nrow(table_under5), nrow(table_under5) + nrow(table_over65))) %>%
  # Ensure proper alignment
  align(align = "left", part = "all") %>%
  align(align = "right", j = 2:ncol(combined_qaly_table), part = "body")

combined_qaly_table

# save_flextable_as_png(combined_qaly_table, "results/combined_qaly_table.png")


#' ## QALYS gained (un-discounted) by vaccination scenario

qaly_table_undiscounted <- qalys_tables_run_scenarios(
  under5_data = averted_under5_vaccination,
  over65_data = averted_over65_vaccination, 
  combo_data = averted_combo_vaccination,
  qaly_discount_rate = 0, cost_discount_rate = 0)

# Add "Section" names as the first row of each table
table_under5_undiscounted <- qaly_table_undiscounted[[1]] %>%
  add_row(.before = 1) %>%
  mutate(Vaccination_Group = case_when(
    is.na(Age_Group) ~ "V1 under 5")) %>% 
  select(Vaccination_Group, everything())

table_over65_undiscounted <- qaly_table_undiscounted[[2]] %>%
  add_row(.before = 1) %>%
  mutate(Vaccination_Group = case_when(
    is.na(Age_Group) ~ "V2 over 65")) %>% 
  select(Vaccination_Group, everything())

table_both_undiscounted <- qaly_table_undiscounted[[3]] %>%
  add_row(.before = 1) %>%
  mutate(Vaccination_Group = case_when(
    is.na(Age_Group) ~ "V3 under 5 and over 65")) %>% 
  select(Vaccination_Group, everything())

# Combine all tables
combined_qaly_table_undiscounted <- bind_rows(
  table_under5_undiscounted,
  table_over65_undiscounted,
  table_both_undiscounted
)

# Create and format the combined table
combined_qaly_table_undiscounted <- combined_qaly_table_undiscounted %>%
  rename("Vaccination strategy" = Vaccination_Group,
         "Age group" = Age_Group) %>%
  flextable::flextable() %>%
  autofit() %>%
  theme_zebra() %>%
  fontsize(size = 10, part = "all") %>%
  # Make the first row of each section bold
  bold(i = c(1, nrow(table_under5_undiscounted) + 1, nrow(table_under5_undiscounted) + nrow(table_over65_undiscounted) + 1)) %>%
  # Add some padding
  padding(padding = 3, part = "all") %>%
  # Add borders between sections
  hline(i = c(nrow(table_under5_undiscounted), nrow(table_under5_undiscounted) + nrow(table_over65_undiscounted))) %>%
  # Ensure proper alignment
  align(align = "left", part = "all") %>%
  align(align = "right", j = 2:ncol(combined_qaly_table_undiscounted), part = "body")

combined_qaly_table_undiscounted

#' ## Cost table (discounted) by vaccination scenario

# Define the function to loop through scenarios and print formatted results
cost_tables_run_scenarios <- function(under5_data, over65_data, combo_data, qaly_discount_rate = 0.035, cost_discount_rate = 0.035) {
  
  results <- list()
  data_list <- list(
    "averted_under5_vaccination" = under5_data,
    "averted_over65_vaccination" = over65_data, 
    "averted_combo_vaccination" = combo_data
  )
  
  for (scenario in names(data_list)) {
    cost_table <- cea_dynamic_model_probabilistic(
      data = data_list[[scenario]],
      n_iterations = n_iter,
      parameter_probabilistic_samples = parameter_probabilistic_samples,
      qaly_discount_rate = qaly_discount_rate,
      cost_discount_rate = cost_discount_rate,
      cost_table =  TRUE
    )
    
    results[[scenario]] <- cost_table %>%
      group_by(Age_Group) %>%
      summarise(
        gp_attendance_cost = paste0(format(round(mean(gp_attendance_cost)), big.mark = ","), " (", 
                                    format(round(quantile(gp_attendance_cost, 0.025)), big.mark = ","), "-",
                                    format(round(quantile(gp_attendance_cost, 0.975)), big.mark = ","), ")"),
        noro_hosp_cost= paste0(format(round(mean(noro_hosp_cost)), big.mark = ","), " (",
                               format(round(quantile(noro_hosp_cost, 0.025)), big.mark = ","), "-", 
                               format(round(quantile(noro_hosp_cost, 0.975)), big.mark = ","), ")"),
        aki_hosp_cost = paste0(format(round(mean(aki_hosp_cost)), big.mark = ","), " (",
                               format(round(quantile(aki_hosp_cost, 0.025)), big.mark = ","), "-",
                               format(round(quantile(aki_hosp_cost, 0.975)), big.mark = ","), ")")
      ) %>% 
      ungroup() %>% 
      rename("GP attendance cost" = gp_attendance_cost,
             "Norovirus hospitalisation cost" = noro_hosp_cost,
             "AKI hospitalisation cost" = aki_hosp_cost)
  }
  return(results)
}

cost_table_discounted <- cost_tables_run_scenarios( under5_data = averted_under5_vaccination,
                                                     over65_data = averted_over65_vaccination, 
                                                     combo_data = averted_combo_vaccination)

# Add "Section" names as the first row of each table
cost_table_under5 <- cost_table_discounted[[1]] %>%
  add_row(.before = 1) %>%
  mutate(Vaccination_Group = case_when(
    is.na(Age_Group) ~ "V1 under 5")) %>% 
  select(Vaccination_Group, everything())

cost_table_over65 <- cost_table_discounted[[2]] %>%
  add_row(.before = 1) %>%
  mutate(Vaccination_Group = case_when(
    is.na(Age_Group) ~ "V2 over 65")) %>% 
  select(Vaccination_Group, everything())

cost_table_both <- cost_table_discounted[[3]] %>%
  add_row(.before = 1) %>%
  mutate(Vaccination_Group = case_when(
    is.na(Age_Group) ~ "V3 under 5 and over 65")) %>% 
  select(Vaccination_Group, everything())

# Combine all tables
combined_cost_table <- bind_rows(
  cost_table_under5,
  cost_table_over65,
  cost_table_both
)

# Create and format the combined table
combined_cost_table <- combined_cost_table %>%
  rename("Vaccination strategy" = Vaccination_Group,
         "Age group" = Age_Group) %>%
  flextable::flextable() %>%
  autofit() %>%
  theme_zebra() %>%
  fontsize(size = 10, part = "all") %>%
  # Make the first row of each section bold
  bold(i = c(1, nrow(cost_table_under5) + 1, nrow(cost_table_under5) + nrow(cost_table_over65) + 1)) %>%
  # Add some padding
  padding(padding = 3, part = "all") %>%
  # Add borders between sections
  hline(i = c(nrow(cost_table_under5), nrow(cost_table_under5) + nrow(cost_table_over65))) %>%
  # Ensure proper alignment
  align(align = "left", part = "all") %>%
  align(align = "right", j = 2:ncol(combined_cost_table), part = "body")

combined_cost_table

#' ## Cost table (undiscounted) by vaccination scenario

cost_table_undiscounted <- cost_tables_run_scenarios(under5_data = averted_under5_vaccination,
                                                     over65_data = averted_over65_vaccination, 
                                                     combo_data = averted_combo_vaccination,
                                                     qaly_discount_rate = 0, cost_discount_rate = 0)

# Add "Section" names as the first row of each table
cost_table_under5_undiscounted <- cost_table_undiscounted[[1]] %>%
  add_row(.before = 1) %>%
  mutate(Vaccination_Group = case_when(
    is.na(Age_Group) ~ "V1 under 5")) %>% 
  select(Vaccination_Group, everything())

cost_table_over65_undiscounted <- cost_table_undiscounted[[2]] %>%
  add_row(.before = 1) %>%
  mutate(Vaccination_Group = case_when(
    is.na(Age_Group) ~ "V2 over 65")) %>% 
  select(Vaccination_Group, everything())

cost_table_both_undiscounted <- cost_table_undiscounted[[3]] %>%
  add_row(.before = 1) %>%
  mutate(Vaccination_Group = case_when(
    is.na(Age_Group) ~ "V3 under 5 and over 65")) %>% 
  select(Vaccination_Group, everything())

# Combine all tables
combined_cost_table_undiscounted<- bind_rows(
  cost_table_under5_undiscounted,
  cost_table_over65_undiscounted,
  cost_table_both_undiscounted
)

# Create and format the combined table
combined_cost_table_undiscounted <- combined_cost_table_undiscounted %>%
  rename("Vaccination strategy" = Vaccination_Group,
         "Age group" = Age_Group) %>%
  flextable::flextable() %>%
  autofit() %>%
  theme_zebra() %>%
  fontsize(size = 10, part = "all") %>%
  # Make the first row of each section bold
  bold(i = c(1, nrow(cost_table_under5_undiscounted) + 1, nrow(cost_table_under5_undiscounted) + nrow(cost_table_over65_undiscounted) + 1)) %>%
  # Add some padding
  padding(padding = 3, part = "all") %>%
  # Add borders between sections
  hline(i = c(nrow(cost_table_under5_undiscounted), nrow(cost_table_under5_undiscounted) + nrow(cost_table_over65_undiscounted))) %>%
  # Ensure proper alignment
  align(align = "left", part = "all") %>%
  align(align = "right", j = 2:ncol(combined_cost_table_undiscounted), part = "body")

combined_cost_table_undiscounted

#' ## Cost-effectiveness analysis by vaccination scenario

#' Full incremental analysis of vaccination strategies, with and without AKI outcomes included. Strategy B (V2 65+) is dominated by strategy C(V3 under 5 and 65+) 

source("R/13_full_incremental_analysis.R")

combined_full_incremental_table

save_flextable_as_png(combined_full_incremental_table, "results/combined_full_incremental_table.png")

#' ## Tornado plot sensitivity analysis
source("R/14_run_mean_trajectory.R")
source("R/15_cea_function_one_way_analysis.R")
source("R/16_generate_tornado_data.R")
source("R/17_tornado_plot_one_way.R")

aki_tornado_plot <- annotate_figure(
  ggarrange(
    tornado_plot_under5 + xlab(NULL) + ylab(NULL) + ggtitle(""), 
    tornado_plot_over65 + xlab(NULL) + ylab(NULL) + ggtitle(""), 
    tornado_plot_under5_over65 + xlab(NULL) + ylab(NULL) + ggtitle(""), 
    ncol = 1, 
    nrow = 3, 
    common.legend = TRUE, 
    labels = c("A", "B", "C"),
    font.label = list(size = 12),
    # label.y = 1.05,
    # label.x = c(0.65, 0.65),  # Reduced x values to move labels left
    widths = c(1, 1),
    heights = c(1)
  ),
  bottom = text_grob("Change in ICER (£/QALY)"),
  left = text_grob("Parameters", rot = 90)
)

aki_tornado_plot

save_plot(aki_tornado_plot, "results/aki_tornado_plot.png")

#' A.V1 under 5, average ICER -1651 B. V2 over 65, average ICER -571  C. V3 under 5 and over 65, average ICER -900

no_aki_tornado_plot <- annotate_figure(
  ggarrange(
    torando_plot_no_aki_under5 + xlab(NULL) + ylab(NULL) + ggtitle(""), 
    torando_plot_no_aki_over65  + xlab(NULL) + ylab(NULL) + ggtitle(""), 
    torando_plot_no_aki_under5_over65  + xlab(NULL) + ylab(NULL) + ggtitle(""), 
    ncol = 1, 
    nrow = 3, 
    common.legend = TRUE, 
    labels = c("A", "B", "C"),
    font.label = list(size = 12),
    # label.y = 1.05,
    # label.x = c(0.65, 0.65),  # Reduced x values to move labels left
    widths = c(1, 1),
    heights = c(1)
  ),
  bottom = text_grob("Change in ICER (£/QALY)"),
  left = text_grob("Parameters", rot = 90)
)

no_aki_tornado_plot

save_plot(no_aki_tornado_plot, "results/no_aki_tornado_plot.png")

#' A.V1 under 5, average ICER 54,243 B. V2 over 65, average ICER 198,429  C. V3 under 5 and over 65, average ICER 136,334


#' ## Probabilistic sensitivity analysis distributions

source("R/18_cea_probabilistic_analysis.R")

#' ## Cost-ffectivness plane

cea_plot <- annotate_figure(
  ggarrange(
    cea_plane + xlab(NULL) + ylab(NULL) + theme(aspect.ratio = 1.3, legend.title = element_text(size = 14), legend.text = element_text(size = 12)), 
    cea_plane_no_aki + xlab(NULL) + ylab(NULL) + theme(aspect.ratio = 1.3, legend.title = element_text(size = 14), legend.text = element_text(size = 12)), 
    ncol = 2, 
    nrow = 1, 
    common.legend = TRUE, 
    labels = c("A", "B"),
    font.label = list(size = 12),
    # label.y = 1.05,
    # label.x = c(0.65, 0.65),  # Reduced x values to move labels left
    widths = c(1, 1),
    heights = c(1)
  ),
  bottom = text_grob("Incremental QALYs"),
  left = text_grob("Incremental costs (£)", rot = 90)
)

cea_plot

cea_plot <- ggarrange(
  # First plot with full axis titles
  cea_plane + 
    xlab("Incremental QALYs") + 
    ylab("Incremental costs (£)") + 
    theme(
      aspect.ratio = 1.3, 
      legend.title = element_text(size = 14), 
      legend.text = element_text(size = 12),
      axis.title.x = element_text(margin = margin(t = 5)),
      axis.title.y = element_text(margin = margin(r = 5))
    ),
  
  # Second plot with x-axis title only
  cea_plane_no_aki + 
    xlab("Incremental QALYs") + 
    ylab(NULL) + 
    theme(
      aspect.ratio = 1.3, 
      legend.title = element_text(size = 14), 
      legend.text = element_text(size = 12),
      axis.title.x = element_text(margin = margin(t = 5))
    ),
  
  # Layout parameters
  ncol = 2, 
  nrow = 1, 
  common.legend = TRUE, 
  labels = c("A", "B"),
  font.label = list(size = 12),
  label.y = 0.95,
  label.x = c(0.1, 0.1),
  widths = c(1, 1),
  heights = c(1)
)

save_plot(cea_plot, "results/cea_plot.png")



#' ## Incremental Net Monetary Benefit (INMB) for different vaccination strategies

source("R/19_inmb_plot.R")
 
inmb_plot <- annotate_figure(
  ggarrange(
    inmb_plot + xlab(NULL) + ylab(NULL) + theme(aspect.ratio = 1.8), 
    no_aki_inmb_plot + xlab(NULL) + ylab(NULL) + theme(aspect.ratio = 1.8), 
    ncol = 2, 
    nrow = 1, 
    common.legend = TRUE, 
    labels = c("A", "B"),
    font.label = list(size = 12)
  ),
  bottom = text_grob("Willingness to pay (£/QALY)"),
  left = text_grob("Average incremental net monetary benefit (INMB)", rot = 90)
)

inmb_plot

save_plot(inmb_plot, "results/inmb_plot.png")

#' ## Cost-effective acceptibility curve and Expected value of perfect information

source("R/20_ceac_plot.R")
source("R/21_evpi_function.R")

ceac_combined_plot <- ggarrange(
  ceac_plot + xlab(NULL),
  ceac_plot_no_aki + xlab(NULL) + ylab(NULL),
  ceac_frontier_plot + xlab(NULL),
  ceac_frontier_plot_no_aki + xlab(NULL) + ylab(NULL),
  evpi_plot,
  evpi_plot_no_aki + ylab(NULL),
  ncol = 2,
  nrow = 3,
  common.legend = TRUE, 
  labels = c("A", "B", "C", "D", "E", "F", "G"),
  font.label = list(size = 12)
)

ceac_combined_plot

save_plot(ceac_combined_plot, "results/ceac_combined_plot.png")

#' ## threshold analysis of AKI hospitalisations rate in over 65

source("R/22_threshold_analysis.R")

threshold_analysis_plot <- annotate_figure(
  ggarrange(
    threshold_under5_plot + xlab(NULL) + ylab(NULL), 
    threshold_over65_plot + xlab(NULL) + ylab(NULL), 
    threshold_under5_over65_plot + xlab(NULL) + ylab(NULL),
    full_incr_threshold_plot + xlab(NULL) + ylab(NULL),
    ncol = 4, 
    nrow = 1, 
    common.legend = TRUE, 
    legend = "none", 
    labels = c("A", "B", "C", "D"),
    font.label = list(size = 8)
  ),
  bottom = text_grob("Proportion of symptomatic norovirus infections in over 65s with an AKI hospitalisation"),
  left = text_grob("Average incremental cost effectiveness ratio (ICER)", rot = 90)
)

threshold_analysis_plot

incremental_threshold

# save_plot(threshold_analysis_plot, "results/threshold_analysis_plot.png")

save_plot(incremental_threshold, "results/incremental_threshold.png")

#' Theshold analysis A. V1 under 5 B. V2 over 65 C. V3 under 5 and 65+

#' ## threshold analysis of vaccination cost

annotate_figure(
  ggarrange(
    vaccine_threshold_under5_plot + xlab(NULL) + ylab(NULL), 
    vaccine_threshold_over65_plot + xlab(NULL) + ylab(NULL),
    Vaccine_threshold_under5_over65_plot + xlab(NULL) + ylab(NULL), 
    ncol = 3, 
    nrow = 1, 
    common.legend = TRUE, 
    legend = "none",
    labels = c("A", "B", "C"),
    font.label = list(size = 8)
  ),
  bottom = text_grob("Vaccine cost"),
  left = text_grob("Incremental cost effectiveness ratio (ICER)", rot = 90)
)

#' Theshold analysis A. V1 under 5 B. V2 over 65 C. V3 under 5 and 65+
