# scripts/03_surface_analysis_glm.R
# Pipeline Step 3: Surface Analysis, Longhurst Biomes, and GLMs

source(here::here("R/00_config_and_palettes.R"))
source(here::here("R/01_data_cleaning.R"))
source(here::here("R/02_spatial_utils.R"))
source(here::here("R/03_optical_model.R"))

dir.create(here::here("outputs/figures"), recursive = TRUE, showWarnings = FALSE)
dir.create(here::here("outputs/tables"), recursive = TRUE, showWarnings = FALSE)

# 1. Load Preprocessed Surface Data & Join Environmental Variables
message("[1/5] Loading surface data (upper 30m) & joining environmental grids...")

urea_env <- read_csv(here::here("outputs/tables/urea_data_unique_env.csv"), show_col_types = FALSE) %>%
  filter(depth <= 30) %>%
  append_coastline_distance() %>%
  filter(distance_to_land_km > 10)

urea_stn <- urea_env %>%
  mutate(
    year = year(date),
    month = month(date),
    day = day(date),
    lat = round(lat, 1),
    lon = round(lon, 1)
  ) %>%
  group_by(year, month, day, lon, lat) %>%
  summarise(
    Urea = mean(Urea, na.rm = TRUE),
    NH4 = mean(NH4, na.rm = TRUE),
    NOx = mean(NOx, na.rm = TRUE),
    distance_to_land_km = mean(distance_to_land_km, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    lat01 = round(floor(lat * 6) / 6 - 1/12, 2),
    lon01 = round(floor(lon * 6) / 6 - 1/12, 2)
  )

chl_df    <- read_csv(here::here("data/environmental/chl.csv"))
npp_df    <- read_csv(here::here("data/environmental/npp.csv"))
adg_df    <- read_csv(here::here("data/environmental/adg.csv"))
aph_df    <- read_csv(here::here("data/environmental/aph.csv"))
bbpm_df   <- read_csv(here::here("data/environmental/bbpm.csv"))
bbpsm_df  <- read_csv(here::here("data/environmental/bbpsm.csv"))
par_df    <- read_csv(here::here("data/environmental/par.csv"))
glorys_df <- read_csv(here::here("data/environmental/glorys.csv"))

urea_stn <- urea_stn %>%
  left_join(chl_df,    by = c("month", "lat01", "lon01")) %>%
  left_join(npp_df,    by = c("month", "lat01", "lon01")) %>%
  left_join(adg_df,    by = c("month", "lat01", "lon01")) %>%
  left_join(aph_df,    by = c("month", "lat01", "lon01")) %>%
  left_join(bbpm_df,   by = c("month", "lat01", "lon01")) %>%
  left_join(bbpsm_df,  by = c("month", "lat01", "lon01")) %>%
  left_join(par_df,    by = c("month", "lat01", "lon01")) %>%
  left_join(glorys_df, by = c("month", "lat01", "lon01")) %>%
  mutate(
    NH4 = if_else(NH4 < 0.01, 0.01, NH4),
    NO3 = if_else(NOx < 0.01, 0.01, NOx),
    ureaN = if_else(Urea < 0.005, 0.01, Urea * 2),
    log_urea_nitrate = log10(ureaN / NO3),
    log_urea_ammonia = log10(ureaN / NH4)
  )

# 2. Concentration Histograms & Ratios (Figure_conc_histo.pdf)
message("[2/5] Generating Figure_conc_histo.pdf...")
CPCOLS <- c("ureaN" = "#1f78b4", "NH4" = "#33a02c", "NO3" = "#e31a1c")

df_histo <- urea_stn %>%
  dplyr::select(NH4, ureaN, NO3) %>%
  pivot_longer(cols = everything(), names_to = "nutrient", values_to = "value")

p_conc <- ggplot(df_histo, aes(x = value)) +
  geom_density(aes(color = nutrient, fill = nutrient), alpha = 0.3) +
  scale_x_log10() +
  scale_color_manual(values = CPCOLS) +
  scale_fill_manual(values = CPCOLS) +
  scale_y_continuous(limits = c(0, 0.7)) +
  theme_bw(base_size = 11)

p_ratio <- ggplot(
  urea_stn %>%
    dplyr::select(log_urea_nitrate, log_urea_ammonia) %>%
    pivot_longer(cols = everything(), names_to = "con_ratio", values_to = "value"),
  aes(x = 10^value)
) +
  geom_density(aes(color = con_ratio, fill = con_ratio), alpha = 0.3) +
  scale_x_log10() +
  scale_color_manual(values = c("#5A2F82", "#C98A32")) +
  scale_fill_manual(values = c("#5A2F82", "#C98A32")) +
  scale_y_continuous(limits = c(0, 0.7)) +
  theme_bw(base_size = 11)

hist_conc <- p_conc + p_ratio
ggsave(here::here("outputs/figures/Figure_conc_histo.pdf"), hist_conc, width = 220, height = 60, units = "mm")

# 3. Longhurst Province Join (Fig02_difference_among_province.pdf)
message("[3/5] Processing Longhurst Provinces...")
lh_shape_path <- here::here("data/shapefiles/Longhurst_world_v4_2010/Longhurst_world_v4_2010.shp")

if (file.exists(lh_shape_path)) {
  lh <- st_read(lh_shape_path, quiet = TRUE)
  pts <- st_as_sf(urea_stn, coords = c("lon", "lat"), crs = 4326)
  sf::sf_use_s2(FALSE)
  longhurst <- st_join(pts, lh, join = st_intersects) %>% st_drop_geometry()
  
  urea_stn$ProvCode <- longhurst$ProvCode
  urea_stn$ProvDescr <- longhurst$ProvDescr
  urea_stn$biome <- sub(" -.*", "", urea_stn$ProvDescr)
  
  selected_province <- urea_stn %>%
    group_by(ProvCode, biome) %>%
    summarise(n = n(), lat = mean(lat, na.rm = TRUE), .groups = "drop") %>%
    filter(n > 30) %>%
    arrange(lat) %>%
    pull(ProvCode)
  
  p_prov <- ggplot(
    urea_stn %>% filter(ProvCode %in% selected_province) %>% mutate(ProvCode = factor(ProvCode, levels = selected_province)),
    aes(x = ProvCode, y = ureaN)
  ) +
    geom_quasirandom(aes(fill = biome), size = 1, shape = 21) +
    stat_summary(fun = median, geom = "point", size = 3, color = "black") +
    scale_fill_atlassian() +
    scale_y_continuous(trans = "log10", limits = c(0.01, 100)) +
    theme_bw() + theme(legend.position = "none")
  
  ggsave(here::here("outputs/figures/Fig02_difference_among_province.pdf"), p_prov, width = 160, height = 60, units = "mm")
} else {
  message("Warning: Longhurst shapefile not found. Skipping Fig02.")
}

# 4. Biome / Regional Category Analysis (Figure03_regional_diff_ratio.pdf)
message("[4/5] Regional Category Classification...")
urea_stn <- urea_stn %>%
  mutate(
    area = case_when(
      lat > 60 | lat < -60 ~ "Subpolar",
      lat >= 30 | lat <= -30 ~ "Mid-Lat",
      TRUE ~ "Low-Lat"
    ),
    chl_cat = if_else(mean_CHL <= 0.4, "Low", "High"),
    area3 = factor(
      paste0(area, "-", chl_cat, "-Chl"),
      levels = c("Low-Lat-Low-Chl", "Low-Lat-High-Chl", "Mid-Lat-Low-Chl", "Mid-Lat-High-Chl", "Subpolar-Low-Chl", "Subpolar-High-Chl")
    )
  )

p_reg_ratio <- ggplot(drop_na(urea_stn, area3), aes(x = area3, y = log_urea_ammonia)) +
  geom_quasirandom(aes(color = area3), size = 1, alpha = 0.4, shape = 19) +
  stat_summary(fun.data = median_qi, geom = "errorbar", width = 0, size = 0.8, color = "black") +
  stat_summary(fun = median, geom = "point", size = 3, color = "black") +
  scale_color_jama() +
  scale_y_continuous(limits = c(-3, 3), breaks = seq(-3, 3, 1)) +
  theme_bw() + theme(legend.position = "none")

p_reg_conc <- ggplot(drop_na(urea_stn, area3), aes(x = area3, y = ureaN)) +
  geom_quasirandom(aes(color = area3), size = 1, alpha = 0.4, shape = 19) +
  stat_summary(fun.data = median_qi, geom = "errorbar", width = 0, size = 0.8, color = "black") +
  stat_summary(fun = median, geom = "point", size = 3, color = "black") +
  scale_color_jama() +
  scale_y_continuous(limits = c(0.01, 100), trans = "log10", breaks = c(0.01, 0.1, 1, 10)) +
  theme_bw() + theme(legend.position = "none")

fig03 <- p_reg_conc / p_reg_ratio
ggsave(here::here("outputs/figures/Figure03_regional_diff_ratio.pdf"), fig03, width = 155, height = 120, units = "mm")

# 5. GLM Analysis & Environmental Responses (Figure4_env_response.pdf)
message("[5/5] Fitting GLMs and Generating Figure4_env_response.pdf...")
urea_stats <- urea_stn %>%
  drop_na(NH4, NO3, mean_CHL, temperature) %>%
  filter(salinity > 28, NH4 < 50) %>%
  mutate(
    log10_Urea = log10(Urea * 2),
    log10_NH4 = log10(NH4),
    log10_NOx = log10(NO3),
    log10_CHL = log10(mean_CHL)
  )

urea_ammonia_glm <- glm(
  log_urea_ammonia ~ log10_NOx + log10_CHL + temperature,
  data = urea_stats
)

pred_df <- urea_stats
pred_df$actual_ratio <- urea_stats$log_urea_ammonia
pred_df$pred_nox <- predict(urea_ammonia_glm, newdata = mutate(pred_df, log10_NOx = mean(urea_stats$log10_NOx)))
pred_df$pred_chl <- predict(urea_ammonia_glm, newdata = mutate(pred_df, log10_CHL = mean(urea_stats$log10_CHL)))
pred_df$pred_temp <- predict(urea_ammonia_glm, newdata = mutate(pred_df, temperature = mean(urea_stats$temperature)))

p_resp1 <- ggplot(pred_df, aes(x = log10_NOx, y = actual_ratio - pred_nox)) +
  geom_point(alpha = 0.2, color = "gray25", shape = 16, size = 0.5) +
  stat_smooth(method = "lm", formula = y ~ poly(x, 2)) +
  scale_y_continuous(limits = c(-3.1, 3.1)) + theme_linedraw()

p_resp2 <- ggplot(pred_df, aes(x = temperature, y = actual_ratio - pred_temp)) +
  geom_point(alpha = 0.2, color = "gray25", shape = 16, size = 0.5) +
  stat_smooth(method = "lm", formula = y ~ poly(x, 2)) +
  scale_y_continuous(limits = c(-3.1, 3.1)) + theme_linedraw()

p_resp3 <- ggplot(pred_df, aes(x = log10_CHL, y = actual_ratio - pred_chl)) +
  geom_point(alpha = 0.2, color = "gray25", shape = 16, size = 0.5) +
  stat_smooth(method = "lm", formula = y ~ poly(x, 2)) +
  scale_y_continuous(limits = c(-3.1, 3.1)) + theme_linedraw()

fig4 <- p_resp1 + p_resp2 + p_resp3 + plot_layout(ncol = 3)
ggsave(here::here("outputs/figures/Figure4_env_response.pdf"), fig4, width = 165, height = 65, units = "mm")

write_csv(urea_stn, here::here("outputs/tables/urea_stn.csv"))
message("SUCCESS: Step 3 complete! Surface analysis and GLM figures generated.")