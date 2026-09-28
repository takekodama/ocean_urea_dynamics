# scripts/01_data_preprocessing.R
# Pipeline Step 1: Preprocessing and Merging Environmental Grids

source(here::here("R/00_config_and_palettes.R"))
source(here::here("R/01_data_cleaning.R"))
source(here::here("R/02_spatial_utils.R"))

# MASSパッケージとの関数衝突を強制回避
select <- dplyr::select

# 出力フォルダが存在しない場合は自動作成
dir.create(here::here("outputs/tables"), recursive = TRUE, showWarnings = FALSE)
dir.create(here::here("outputs/figures"), recursive = TRUE, showWarnings = FALSE)

# 1. 観測データの読み込みとクレンジング
message("[1/4] Loading and cleaning observation sheets...")
excel_file <- here::here("data/raw/Urea_data.xlsx")

nanomolar <- read_and_fix_sheet(excel_file, "nanomolar") %>%
  mutate(Urea = urea / 1000)

micromolar <- bind_rows(
  read_and_fix_sheet(excel_file, "micromolar"),
  read_and_fix_sheet(excel_file, "bodc+sdn"),
  read_and_fix_sheet(excel_file, "PANGAEA")
) %>%
  mutate(
    Urea = clean_nutrient_values(Urea),
    Urea = replace_na(Urea, 0)
  ) %>%
  drop_na(lat, lon, depth) %>%
  mutate(NOx = NO3)

# データ結合と外れ値処理
urea_data <- micromolar %>%
  dplyr::select(-NO3) %>%
  bind_rows(dplyr::select(nanomolar, cruise, date, lat, lon, depth, Urea, NH4, NOx)) %>%
  mutate(
    date = if_else(
      is.na(as.numeric(date)),
      as_date(str_replace(date, "T.*", "")),
      as_date(as.numeric(date), origin = "1899-12-30")
    ),
    lon = if_else(lon > 180, lon - 360, lon),
    Urea = if_else(Urea < 0, 0, Urea),
    NOx = clean_nutrient_values(NOx),
    NH4 = clean_nutrient_values(NH4)
  ) %>%
  filter_nutrient_outliers()

# 2. 沖合10km以上のデータ抽出
message("[2/4] Filtering offshore stations (>10km)...")
urea_data <- append_coastline_distance(urea_data) %>%
  filter(distance_to_land_km > 10)

# ステーション概要の出力
sample_number <- urea_data %>%
  group_by(cruise) %>%
  summarise(sample_n = n(), .groups = "drop") %>%
  left_join(
    urea_data %>%
      distinct(cruise, lon, lat) %>%
      group_by(cruise) %>%
      summarise(stn_n = n(), .groups = "drop"),
    by = "cruise"
  )
write_csv(sample_number, here::here("outputs/tables/sample_number.csv"))

# ユニークプロファイルの平均化
urea_data_unique <- urea_data %>%
  dplyr::select(-cruise) %>%
  group_by(date, lat, lon, depth) %>%
  summarise(across(everything(), ~ mean(.x, na.rm = TRUE)), .groups = "drop")

write_csv(urea_data_unique, here::here("outputs/tables/urea_conc.csv"))

# 3. 環境データ（CHL & GLORYS）の結合（高速化版）
message("[3/4] Merging environmental data (CHL & GLORYS)...")

# 観測データの前処理（月とグリッド座標の付与）
urea_data_unique_env <- urea_data_unique %>%
  mutate(
    month = month(date),
    lat01 = round(floor(lat * 6) / 6 - 1/12, 2),
    lon01 = round(floor(lon * 6) / 6 - 1/12, 2)
  )

# 観測データが存在する「必要なグリッド（month, lat01, lon01）」のペアだけを抽出
target_grids <- urea_data_unique_env %>%
  distinct(month, lat01, lon01)

# CHLデータの爆速読み込み＆必要分のみに絞り込んでから集計
all_CHL_mean <- data.table::fread(here::here("data/environmental/all_CHL_mean.csv")) %>%
  mutate(lon01 = round(lon, 2), lat01 = round(lat, 2)) %>%
  dplyr::select(-lat, -lon) %>%
  semi_join(target_grids, by = c("month", "lat01", "lon01")) %>%  # ← 観測地点だけに先に絞り込み！
  group_by(month, lat01, lon01) %>%
  summarise(across(everything(), ~ mean(.x, na.rm = TRUE)), .groups = "drop")

# GLORYSデータの爆速読み込み＆必要分のみに絞り込んでから集計
GLORYS12V1_00 <- data.table::fread(here::here("data/environmental/GLORYS12V1_00.csv")) %>%
  semi_join(target_grids, by = c("month", "lat01", "lon01")) %>%  # ← 観測地点だけに先に絞り込み！
  group_by(month, lat01, lon01) %>%
  summarise(across(everything(), ~ mean(.x, na.rm = TRUE)), .groups = "drop")

# 結合処理（軽量化されたデータ同士なので一瞬）
urea_data_unique_env <- urea_data_unique_env %>%
  left_join(all_CHL_mean, by = c("month", "lat01", "lon01")) %>%
  left_join(GLORYS12V1_00, by = c("month", "lat01", "lon01"))

write_csv(urea_data_unique_env, here::here("outputs/tables/urea_data_unique_env.csv"))

# 4. マップの描画と保存
message("[4/4] Generating map.pdf...")
mp1 <- fortify(maps::map("world", fill = TRUE, plot = FALSE))
map_plot <- ggplot(data = urea_data_unique_env, aes(x = lon, y = lat)) +
  geom_polygon(data = mp1, aes(x = long, y = lat, group = group), fill = "gray60", color = NA) +
  geom_point(aes(color = Urea + 0.001), size = 1) +
  scale_color_gradientn(colours = viridis::turbo(9), trans = "log10") +
  coord_sf(xlim = c(-180, 180), ylim = c(-90, 90), expand = FALSE) +
  theme_bw()

ggsave(here::here("outputs/figures/map.pdf"), map_plot, width = 120, height = 60, units = "mm")

message("SUCCESS: Step 1 complete! urea_data_unique_env.csv and map.pdf generated.")