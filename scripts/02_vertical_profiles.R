# scripts/02_vertical_profiles.R
# Pipeline Step 2: Vertical Profile Plotting (Fig_vertical_profile.pdf & Fig_vertical_profile_area.pdf)

source(here::here("R/00_config_and_palettes.R"))

# 1. Load Data & Ensure 'area' column exists
urea_data_unique_env <- read_csv(here::here("outputs/tables/urea_data_unique_env.csv"), show_col_types = FALSE)

# 表層NOxの計算と area 列の定義（未定義の場合に備えて自動作成）
if (!"surfaceNOx" %in% names(urea_data_unique_env)) {
  surface_nox <- urea_data_unique_env %>%
    filter(depth <= 30) %>%
    group_by(date, lon, lat) %>%
    summarise(surfaceNOx = mean(NOx, na.rm = TRUE), .groups = "drop")
  
  urea_data_unique_env <- urea_data_unique_env %>%
    left_join(surface_nox, by = c("date", "lon", "lat"))
}

urea_data_unique_env <- urea_data_unique_env %>%
  mutate(
    area = case_when(
      lat > 61 ~ "Arctic",
      lat <= 30 & lat >= -30 & mean_CHL < 0.4 & surfaceNOx < 1 ~ "LNLC",
      lat >= 40 & lat <= 60 & mean_CHL < 0.4 & surfaceNOx > 2 ~ "HNLC_NEP",
      lat >= -15 & lat <= 15 & lon <= -120 & mean_CHL < 0.4 & surfaceNOx > 2 ~ "HNLC_EQP",
      lat <= -40 & mean_CHL < 0.4 & surfaceNOx > 2 ~ "HNLC_Southern",
      TRUE ~ NA_character_
    ),
    area = factor(area, levels = c("Arctic", "LNLC", "HNLC_EQP", "HNLC_NEP", "HNLC_Southern"))
  ) %>%
  filter(!is.na(Urea)) %>%
  mutate(
    urea = if_else(Urea * 2 > 0.01, Urea * 2, 0.01),
    NH4  = if_else(NH4 > 0.01, NH4, 0.01),
    NOx  = if_else(NOx > 0.01, NOx, 0.01),
    trans_depth = if_else(depth > 200, (depth - 200) / 20 + 200, depth)
  )

# 2. Depth Binned Medians
urea_data_depth_median <- urea_data_unique_env %>%
  mutate(
    depth_bin = case_when(
      depth < 50  ~ floor(depth / 10) * 10 + 5,
      depth < 100 ~ floor(depth / 25) * 25 + 12.5,
      depth < 200 ~ floor(depth / 50) * 50 + 25,
      depth < 500 ~ floor(depth / 250) * 250 + 125,
      TRUE        ~ floor(depth / 500) * 500 + 250
    )
  ) %>%
  group_by(depth = depth_bin, area) %>%
  summarise(n = n(), NH4 = median(NH4, na.rm = TRUE), NOx = median(NOx, na.rm = TRUE), urea = median(urea, na.rm = TRUE), .groups = "drop") %>%
  bind_rows(
    urea_data_unique_env %>%
      mutate(
        depth_bin = case_when(
          depth < 50  ~ floor(depth / 10) * 10 + 5,
          depth < 100 ~ floor(depth / 25) * 25 + 12.5,
          depth < 200 ~ floor(depth / 50) * 50 + 25,
          depth < 500 ~ floor(depth / 250) * 250 + 125,
          TRUE        ~ floor(depth / 500) * 500 + 250
        )
      ) %>%
      group_by(depth = depth_bin) %>%
      summarise(area = "Total", n = n(), NH4 = median(NH4, na.rm = TRUE), NOx = median(NOx, na.rm = TRUE), urea = median(urea, na.rm = TRUE), .groups = "drop")
  ) %>%
  mutate(trans_depth = if_else(depth > 200, (depth - 200) / 20 + 200, depth)) %>%
  filter(n > 10)

# 3. Density Estimations
kde_urea <- kde2d(log10(urea_data_unique_env$depth + 1), log10(urea_data_unique_env$urea), n = 200)
urea_data_unique_env$kdensity_urea <- interp.surface(kde_urea, cbind(log10(urea_data_unique_env$depth + 1), log10(urea_data_unique_env$urea)))

valid_nh4 <- !is.na(urea_data_unique_env$NH4)
kde_nh4 <- kde2d(log10(urea_data_unique_env$depth[valid_nh4] + 1), log10(urea_data_unique_env$urea[valid_nh4] / urea_data_unique_env$NH4[valid_nh4]), n = 200)
urea_data_unique_env$kdensity_urea_NH4[valid_nh4] <- interp.surface(kde_nh4, cbind(log10(urea_data_unique_env$depth[valid_nh4] + 1), log10(urea_data_unique_env$urea[valid_nh4] / urea_data_unique_env$NH4[valid_nh4])))

valid_nox <- !is.na(urea_data_unique_env$NOx)
kde_nox <- kde2d(log10(urea_data_unique_env$depth[valid_nox] + 1), log10(urea_data_unique_env$urea[valid_nox] / urea_data_unique_env$NOx[valid_nox]), n = 200)
urea_data_unique_env$kdensity_urea_NOx[valid_nox] <- interp.surface(kde_nox, cbind(log10(urea_data_unique_env$depth[valid_nox] + 1), log10(urea_data_unique_env$urea[valid_nox] / urea_data_unique_env$NOx[valid_nox])))

# 4. Generate Fig_vertical_profile.pdf
p1 <- ggplot(urea_data_unique_env, aes(x = trans_depth, y = urea)) +
  geom_point(shape = 19, size = 1, aes(color = kdensity_urea)) +
  scale_color_gradientn(colors = met.brewer("Hokusai3", direction = -1), limits = c(0.01, 0.5), trans = "log10", oob = scales::squish) +
  geom_point(data = urea_data_depth_median %>% filter(area == "Total"), size = 2.5, shape = 21, color = "black", fill = "white") +
  coord_flip(ylim = c(0.01, 10), xlim = c(0, 200 + 3000 / 20)) +
  geom_vline(xintercept = 200, linewidth = 0.2) +
  scale_x_reverse(expand = c(0, 0), breaks = c(0, 50, 100, 150, 200, 200 + 15, 200 + 40, 200 + 90, 200 + 140)) +
  scale_y_continuous(trans = "log10") + theme_test() + theme(legend.position = "none")

p2 <- ggplot(urea_data_unique_env, aes(x = trans_depth, y = urea / NH4)) +
  geom_point(shape = 19, size = 1, aes(color = kdensity_urea_NH4)) +
  scale_color_gradientn(colors = met.brewer("Hokusai3", direction = -1), limits = c(0.01, 0.5), trans = "log10", oob = scales::squish) +
  geom_point(data = urea_data_depth_median %>% filter(area == "Total"), size = 2.5, shape = 21, color = "black", fill = "white") +
  scale_x_reverse(expand = c(0, 0), breaks = c(0, 50, 100, 150, 200, 200 + 15, 200 + 40, 200 + 90, 200 + 140)) +
  scale_y_continuous(trans = "log10") + geom_vline(xintercept = 200, linewidth = 0.2) + geom_hline(yintercept = 1, lty = 2) +
  coord_flip(ylim = c(0.01, 100), xlim = c(0, 200 + 3000 / 20)) + theme_test() + theme(legend.position = "none")

p3 <- ggplot(urea_data_unique_env, aes(x = trans_depth, y = urea / NOx)) +
  geom_point(shape = 19, size = 1, aes(color = kdensity_urea_NOx)) +
  scale_color_gradientn(colors = met.brewer("Hokusai3", direction = -1), limits = c(0.01, 0.5), trans = "log10", oob = scales::squish) +
  geom_point(data = urea_data_depth_median %>% filter(area == "Total"), size = 2.5, shape = 21, color = "black", fill = "white") +
  scale_x_reverse(expand = c(0, 0), breaks = c(0, 50, 100, 150, 200, 200 + 15, 200 + 40, 200 + 90, 200 + 140)) +
  scale_y_continuous(trans = "log10", breaks = c(0.001, 0.01, 0.1, 1, 10, 100, 1000)) +
  geom_vline(xintercept = 200, linewidth = 0.2) + geom_hline(yintercept = 1, lty = 2) +
  coord_flip(ylim = c(0.001, 1000), xlim = c(0, 200 + 3000 / 20)) + theme_test() + theme(legend.position = "none")

Fig_vertical_profile <- p1 + p2 + p3 + plot_layout(ncol = 3)
ggsave(filename = here::here("outputs/figures/Fig_vertical_profile.pdf"), plot = Fig_vertical_profile, width = 200, height = 100, units = "mm")

# 5. Generate Fig_vertical_profile_area.pdf
calc_kde_area <- function(df_area) {
  if (nrow(df_area) == 0) return(df_area)
  kde_u <- kde2d(log10(df_area$depth + 1), log10(df_area$urea), n = 200)
  df_area$kdensity_urea <- interp.surface(kde_u, cbind(log10(df_area$depth + 1), log10(df_area$urea)))
  
  v_nh4 <- !is.na(df_area$NH4)
  if (sum(v_nh4) > 5) {
    kde_nh <- kde2d(log10(df_area$depth[v_nh4] + 1), log10(df_area$urea[v_nh4] / df_area$NH4[v_nh4]), n = 200)
    df_area$kdensity_urea_NH4[v_nh4] <- interp.surface(kde_nh, cbind(log10(df_area$depth[v_nh4] + 1), log10(df_area$urea[v_nh4] / df_area$NH4[v_nh4])))
  }
  
  v_nox <- !is.na(df_area$NOx)
  if (sum(v_nox) > 5) {
    kde_no <- kde2d(log10(df_area$depth[v_nox] + 1), log10(df_area$urea[v_nox] / df_area$NOx[v_nox]), n = 200)
    df_area$kdensity_urea_NOx[v_nox] <- interp.surface(kde_no, cbind(log10(df_area$depth[v_nox] + 1), log10(df_area$urea[v_nox] / df_area$NOx[v_nox])))
  }
  return(df_area)
}

env_arctic <- calc_kde_area(urea_data_unique_env %>% filter(area == "Arctic"))
env_lnlc   <- calc_kde_area(urea_data_unique_env %>% filter(area == "LNLC"))

make_panel <- function(df, med_area, y_var, y_limits, hline_val = NULL) {
  col_name <- if_else(y_var == "urea", "kdensity_urea", paste0("kdensity_urea_", y_var))
  if (!col_name %in% names(df)) df[[col_name]] <- 0.1
  
  p <- ggplot(df, aes(x = trans_depth, y = .data[[y_var]])) +
    geom_point(shape = 19, size = 1, aes(color = .data[[col_name]])) +
    scale_color_gradientn(colors = met.brewer("Hokusai3", direction = -1), limits = c(0.01, 0.5), trans = "log10", oob = scales::squish) +
    geom_point(data = urea_data_depth_median %>% filter(area == med_area), size = 2.5, shape = 21, color = "black", fill = "white") +
    coord_flip(ylim = y_limits, xlim = c(0, 200 + 3000 / 20)) +
    geom_vline(xintercept = 200, linewidth = 0.2) +
    scale_x_reverse(expand = c(0, 0), breaks = c(0, 50, 100, 150, 200, 200 + 15, 200 + 40, 200 + 90, 200 + 140)) +
    scale_y_continuous(trans = "log10") + theme_test() + theme(legend.position = "none")
  if (!is.null(hline_val)) p <- p + geom_hline(yintercept = hline_val, lty = 2)
  return(p)
}

Fig_vertical_profile_area <- (
  make_panel(env_arctic, "Arctic", "urea", c(0.01, 10)) +
  make_panel(env_lnlc, "LNLC", "urea", c(0.01, 10)) +
  make_panel(env_arctic, "Arctic", "NH4", c(0.01, 100), 1) +
  make_panel(env_lnlc, "LNLC", "NH4", c(0.01, 100), 1) +
  make_panel(env_arctic, "Arctic", "NOx", c(0.001, 1000), 1) +
  make_panel(env_lnlc, "LNLC", "NOx", c(0.001, 1000), 1)
) + plot_layout(ncol = 3, byrow = FALSE)

ggsave(filename = here::here("outputs/figures/Fig_vertical_profile_area.pdf"), plot = Fig_vertical_profile_area, width = 200, height = 200, units = "mm")
message("SUCCESS: Step 2 complete! Vertical profile figures generated.")