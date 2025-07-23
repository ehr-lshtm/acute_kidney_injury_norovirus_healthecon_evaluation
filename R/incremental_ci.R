icer_v1_aki <- as.data.frame(cea_under5$raw_results) |>
  select(icer) |>
  rename(icer_v1 = icer)

icer_v2_aki <- as.data.frame(cea_over65$raw_results) |>
  select(icer) |>
  rename(icer_v2 = icer)

icer_v3_aki <- as.data.frame(cea_under5_over65$raw_results) |>
  select(icer) |>
  rename(icer_v3 = icer)

icer_combined_aki <- bind_cols(icer_v1_aki, icer_v2_aki, icer_v3_aki)

incremental_combined_aki <- icer_combined_aki |>
  mutate(icer_BvA = icer_v2 - icer_v1,
         icer_CvA = icer_v3 - icer_v1,
         icer_CvB = icer_v3 - icer_v2)

lower_ci_BvA = quantile(incremental_combined_aki$icer_BvA, 0.025)
upper_ci_BvA = quantile(incremental_combined_aki$icer_BvA, 0.975)

lower_ci_CvA = quantile(incremental_combined_aki$icer_CvA, 0.025)
upper_ci_CvA = quantile(incremental_combined_aki$icer_CvA, 0.975)

lower_ci_CvB = quantile(incremental_combined_aki$icer_CvB, 0.025)
upper_ci_CvB = quantile(incremental_combined_aki$icer_CvB, 0.975)


