process_vaccination_data <- function(file_path, iterations = 1:n_iter, suffix = "") {
  data <- read_parquet(file_path, ) %>%
    filter(Iteration %in% iterations)
  
  iter_results <- lapply(iterations, function(iter) {
    data %>%
      filter(Iteration == iter) %>%
      {
        df <- .
        start_index <- which(df$time >= 3535)[1]
        df$count <- 0
        df$count[start_index:nrow(df)] <- seq_len(nrow(df) - start_index + 1)
        df$season <- ceiling(pmax(df$count, 1) / 365) - 1
        
        df %>%
          mutate(season = case_when(
            season == 0 & count <= 0 ~ "-S1",
            season == 0 & count > 0 ~ "S0",
            season >= 1 & season <= 20 ~ paste0("S", season),
            TRUE ~ as.character(season)
          )) %>%
          filter(season != "-S1") %>%
          group_by(season) %>%
          summarise(
            total_Is1 = sum(Is1),
            total_Is2 = sum(Is2),
            total_Is3 = sum(Is3),
            total_Is4 = sum(Is4),
            total_under5_vaccinated = sum(new_vaccinations_v1_S_1) + sum(new_vaccinations_v1_R_1),
            total_5_14_vaccinated = sum(new_vaccinations_v1_S_2) + sum(new_vaccinations_v1_R_2),
            total_15_64_vaccinated = sum(new_vaccinations_v1_S_3) + sum(new_vaccinations_v1_R_3),
            total_65_plus_vaccinated = sum(new_vaccinations_v1_S_4) + sum(new_vaccinations_v1_R_4)
          ) %>%
          rename_at(vars(starts_with("total_Is")), 
                    ~paste0(., if(suffix != "") paste0("_", suffix) else "")) %>%
          mutate(Iteration = iter) %>% 
          filter(season != "S20")
      }
  })
  
  return(bind_rows(iter_results))
}


###

no_vaccination <- process_vaccination_data("data/no_vaccination.parquet")

total_no_vaccination <- no_vaccination %>%
  mutate(
    averted_under5 = total_Is1 - total_Is1,
    averted_5_14 = total_Is2 - total_Is2,
    averted_15_64 = total_Is3 - total_Is3,
    averted_over65 = total_Is4 - total_Is4
  ) %>% 
  select(season, Iteration, starts_with("averted"))

###

under_5_vaccination <- process_vaccination_data("data/under_5_vaccination.parquet", suffix = "vacc_u5")

averted_under5_vaccination <- no_vaccination %>% 
  select(season, Iteration, starts_with("total_Is")) %>% 
  left_join(under_5_vaccination, by = c("season", "Iteration")) %>%
  mutate(
    averted_under5 = total_Is1 - total_Is1_vacc_u5,
    averted_5_14 = total_Is2 - total_Is2_vacc_u5,
    averted_15_64 = total_Is3 - total_Is3_vacc_u5,
    averted_over65 = total_Is4 - total_Is4_vacc_u5
  ) %>% 
  select(season, Iteration, starts_with("averted"), ends_with("vaccinated"))

remove(under_5_vaccination)
gc()

### 

over_65_vaccination <- process_vaccination_data("data/over65_vaccination.parquet", suffix = "vacc_over65")

averted_over65_vaccination <- no_vaccination %>%
  select(season, Iteration, starts_with("total_Is")) %>% 
  left_join(over_65_vaccination, by = c("season", "Iteration")) %>%
  mutate(
    averted_under5 = total_Is1 - total_Is1_vacc_over65,
    averted_5_14 = total_Is2 - total_Is2_vacc_over65,
    averted_15_64 = total_Is3 - total_Is3_vacc_over65,
    averted_over65 = total_Is4 - total_Is4_vacc_over65
  ) %>% 
  select(season, Iteration, starts_with("averted"), ends_with("vaccinated"))

remove(over_65_vaccination)
gc()

###

combo_vaccination <- process_vaccination_data("data/under5_over65_vaccination.parquet", suffix = "vacc_combo")

averted_combo_vaccination <- no_vaccination %>%
  select(season, Iteration, starts_with("total_Is")) %>% 
  left_join(combo_vaccination, by = c("season", "Iteration")) %>%
  mutate(
    averted_under5 = total_Is1 - total_Is1_vacc_combo,
    averted_5_14 = total_Is2 - total_Is2_vacc_combo,
    averted_15_64 = total_Is3 - total_Is3_vacc_combo,
    averted_over65 = total_Is4 - total_Is4_vacc_combo
  ) %>% 
  select(season, Iteration, starts_with("averted"), ends_with("vaccinated"))

remove(combo_vaccination)
gc()



