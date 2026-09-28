# R/02_spatial_utils.R
# Spatial Analysis Utilities

library(sf)
library(rnaturalearth)
library(dplyr)
library(tidyr)

#' Calculate distance to closest coastline in kilometers
#' 
#' @param df Dataframe containing latitude and longitude
#' @param lon_col Column name for longitude (default: "lon")
#' @param lat_col Column name for latitude (default: "lat")
#' @return Dataframe joined with distance_to_land_km
append_coastline_distance <- function(df, lon_col = "lon", lat_col = "lat") {
  # 既に列が存在する場合は重複結合せずにそのまま返す
  if ("distance_to_land_km" %in% names(df)) {
    return(df)
  }
  
  if (!lon_col %in% names(df) || !lat_col %in% names(df)) {
    stop(sprintf("Specified coordinate columns '%s' or '%s' do not exist in the dataset.", lon_col, lat_col))
  }
  
  sf_use_s2(FALSE)
  land <- ne_countries(scale = "large", returnclass = "sf") %>%
    st_make_valid() %>%
    st_union()
  sf_use_s2(TRUE)
  
  coords_df <- df %>%
    ungroup() %>%
    distinct(longitude = .data[[lon_col]], latitude = .data[[lat_col]]) %>%
    drop_na(longitude, latitude)
  
  if (nrow(coords_df) == 0) {
    df$distance_to_land_km <- NA_real_
    return(df)
  }
  
  coords_sf <- st_as_sf(coords_df, coords = c("longitude", "latitude"), crs = 4326)
  coords_df$distance_to_land_km <- as.numeric(st_distance(coords_sf, land)[, 1]) / 1000
  
  join_keys <- setNames(c("longitude", "latitude"), c(lon_col, lat_col))
  result_df <- left_join(df, coords_df, by = join_keys)
  
  return(result_df)
}