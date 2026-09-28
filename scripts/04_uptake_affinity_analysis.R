# scripts/04_uptake_affinity_analysis.R
# Pipeline Step 4: Uptake Kinetics, Affinity Correlations, and Theoretical Simulations

source(here::here("R/00_config_and_palettes.R"))

# 1. Read Uptake Excel Data
uptake_file <- here::here("data/raw/Urea_uptake_data.xlsx")
uptake_file_sheet <- excel_sheets(uptake_file)

clean_uptake_column <- function(df) {
  df <- df %>% select(-any_of(c("Sta.", "Station")))
  for (col in c("NO3_Specific_Uptake [/H]", "Urea_Absolute_Uptake [uM/H]", "NH4_Absolute_Uptake [uM/H]", "NO3_Absolute_Uptake [uM/H]")) {
    if (col %in% colnames(df)) df[[col]] <- as.character(df[[col]])
  }
  return(df)
}

urea_uptake_data_clean <- map(uptake_file_sheet[-1], ~ read_excel(uptake_file, sheet = .x)) %>%
  set_names(uptake_file_sheet[-1]) %>%
  map(clean_uptake_column)

urea_uptake_long <- map2_df(
  urea_uptake_data_clean,
  uptake_file_sheet[-1],
  ~ bind_rows(
    mutate(.x, sheet = .y,
           NO3_Specific_Uptake_H = if ("NO3_Specific_Uptake [/H]" %in% colnames(.x)) as.numeric(`NO3_Specific_Uptake [/H]`) else NA_real_,
           Urea_Absolute_Uptake_uMH = if ("Urea_Absolute_Uptake [uM/H]" %in% colnames(.x)) as.numeric(`Urea_Absolute_Uptake [uM/H]`) else NA_real_
    )
  )
)

urea_uptake_data <- urea_uptake_long %>%
  drop_na(date, lat, lon, depth) %>%
  filter(depth < 50) %>%
  mutate(
    NH4_uptake_nMperH = as.numeric(`NH4_Absolute_Uptake [uM/H]`) * 1000,
    NO3_uptake_nMperH = as.numeric(`NO3_Absolute_Uptake [uM/H]`) * 1000,
    urea_uptake_nMperH = as.numeric(`Urea_Absolute_Uptake [uM/H]`) * 1000,
    urea_specific_uptake_perH = as.numeric(`Urea_Specific_Uptake [/H]`),
    NH4_specific_uptake_perH = as.numeric(`NH4_Specific_Uptake [/H]`),
    NO3_specific_uptake_perH = as.numeric(`NO3_Specific_Uptake [/H]`),
    lon = if_else(lon > 180, lon - 360, lon)
  )

urea_stn <- read_csv(here::here("outputs/tables/urea_stn.csv"))

urea_uptake_stn <- urea_uptake_data %>%
  mutate(year = year(date), month = month(date), lon = round(lon, 1), lat = round(lat, 1)) %>%
  group_by(year, month, lon, lat) %>%
  summarise(
    urea_uptake_nMperH = mean(urea_uptake_nMperH, na.rm = TRUE),
    NH4_uptake_nMperH = mean(NH4_uptake_nMperH, na.rm = TRUE),
    NO3_uptake_nMperH = mean(NO3_uptake_nMperH, na.rm = TRUE),
    urea_specific_uptake_perH = mean(urea_specific_uptake_perH, na.rm = TRUE),
    NH4_specific_uptake_perH = mean(NH4_specific_uptake_perH, na.rm = TRUE),
    NO3_specific_uptake_perH = mean(NO3_specific_uptake_perH, na.rm = TRUE),
    urea_affinity = mean(Urea_affinity, na.rm = TRUE),
    NH4_affinity = mean(NH4_affinity, na.rm = TRUE),
    NO3_affinity = mean(NO3_affinity, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  left_join(urea_stn %>% select(year, month, lon, lat, Urea, NH4, NOx, Chl = mean_CHL, PAR = mean_PAR, SST = temperature), by = c("year", "month", "lon", "lat")) %>%
  mutate(
    Urea = if_else(Urea < 0.01, 0.01, Urea), NH4 = if_else(NH4 < 0.01, 0.01, NH4), NOx = if_else(NOx < 0.01, 0.01, NOx),
    log_urea_ammonia = log10(Urea * 2 / NH4), log_urea_nitrate = log10(Urea * 2 / NOx),
    uptake_log_urea_ammonia = log10(urea_uptake_nMperH / NH4_uptake_nMperH),
    uptake_log_urea_nitrate = log10(urea_uptake_nMperH / NO3_uptake_nMperH),
    totalN_uptake = urea_uptake_nMperH + NH4_uptake_nMperH + NO3_uptake_nMperH,
    urea_contribution = urea_uptake_nMperH / totalN_uptake
  )

write_csv(urea_uptake_stn, here::here("outputs/tables/urea_uptake_stn.csv"))

# 2. Fig_uptake_vs_chl.pdf
plot_uptake_vs_chl <- (
  ggplot(urea_uptake_stn, aes(x = Chl, y = uptake_log_urea_nitrate)) +
    geom_point(shape = 16, color = "gray25") + stat_smooth(method = "lm") +
    scale_x_continuous(trans = "log10") + theme_test(base_size = 11) +
    ggplot(urea_uptake_stn, aes(x = Chl, y = uptake_log_urea_ammonia)) +
    geom_point(shape = 16, color = "gray25") + stat_smooth(method = "lm") +
    scale_x_continuous(trans = "log10") + theme_test(base_size = 11)
)

ggsave(plot = plot_uptake_vs_chl, filename = here::here("outputs/figures/Fig_uptake_vs_chl.pdf"), units = "mm", height = 70, width = 140)

# 3. Fig_uptake_ratio.pdf
Fig_uptake_ratio <- (
  ggplot(urea_uptake_stn, aes(x = Chl, y = urea_contribution * 100)) +
    geom_point(shape = 16, color = "gray25") + stat_smooth(method = "lm") +
    scale_x_continuous(trans = "log10") + theme_test(base_size = 11) +
    ggplot(urea_uptake_stn %>% drop_na(uptake_log_urea_ammonia, log_urea_ammonia), aes(log_urea_ammonia, uptake_log_urea_ammonia)) +
    geom_hline(yintercept = 0, lty = 2) + geom_vline(xintercept = 0, lty = 2) +
    geom_point(shape = 16, color = "gray25") + geom_abline(slope = 1, intercept = 0) + stat_smooth(method = "lm") +
    scale_x_continuous(limits = c(-2.5, 2.5)) + scale_y_continuous(limits = c(-2.5, 2.5)) + coord_equal() + theme_test(base_size = 11) +
    ggplot(urea_uptake_stn %>% drop_na(uptake_log_urea_nitrate, log_urea_nitrate), aes(log_urea_nitrate, uptake_log_urea_nitrate)) +
    geom_hline(yintercept = 0, lty = 2) + geom_vline(xintercept = 0, lty = 2) +
    geom_point(shape = 16, color = "gray25") + geom_abline(slope = 1, intercept = 0) + stat_smooth(method = "lm") +
    scale_x_continuous(limits = c(-3.1, 3.1)) + scale_y_continuous(limits = c(-3.1, 3.1)) + coord_equal() + theme_test(base_size = 11)
) + plot_layout(ncol = 3)

ggsave(plot = Fig_uptake_ratio, filename = here::here("outputs/figures/Fig_uptake_ratio.pdf"), units = "mm", height = 55, width = 165)

# 4. Healey Affinity Simulation
x_values <- runif(200, min = -2, max = 2)
results_df <- map_dfr(x_values, function(x) {
  plot_data <- tibble(
    CN1 = runif(100, 0.01, 10), CN2 = runif(100, 0.01, 10), KN1 = runif(100, 0.05, 0.1),
    VmaxN1 = runif(100, 8, 12), VmaxN2 = runif(100, 8, 12)
  ) %>%
    mutate(
      KN2 = VmaxN2 * KN1 * (10^x) / VmaxN1,
      logV1 = log10((VmaxN1 * CN1) / (KN1 + CN1)), logV2 = log10((VmaxN2 * CN2) / (KN2 + CN2)),
      uptake_ratio = logV1 - logV2, nutrient_ratio = log10(CN1 / CN2)
    )
  lm_model <- lm(uptake_ratio ~ nutrient_ratio, data = plot_data)
  tibble(
    x_value = x,
    intercept = summary(lm_model)$coefficients["(Intercept)", "Estimate"],
    slope = summary(lm_model)$coefficients["nutrient_ratio", "Estimate"],
    r_squared = summary(lm_model)$r.squared
  )
})

write_csv(results_df, here::here("outputs/tables/affinity_simulation_results.csv"))
message("Step 4 complete: All uptake figures and simulations exported.")