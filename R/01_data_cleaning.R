# R/01_data_cleaning.R
# Functions for Reading, Parsing, and Standardizing Nutrient Data

library(dplyr)
library(readxl)
library(lubridate)
library(stringr)

#' Read and fix sheet types from Excel
read_and_fix_sheet <- function(filepath, sheetname) {
  read_xlsx(filepath, sheet = sheetname) %>%
    mutate(
      date = as.character(date),
      Urea = if ("Urea" %in% names(.)) as.character(Urea) else NA_character_
    )
}

#' Parse non-numeric text values (e.g., "ND", "<0.1", "BDL")
clean_nutrient_values <- function(vec) {
  case_when(
    vec %in% c("ND", "nd") ~ NA_real_,
    str_detect(vec, "^<") | vec %in% c("bdl", "BDL") ~ 0,
    TRUE ~ suppressWarnings(as.numeric(vec))
  )
}

#' Clean outliers and fill ranges
filter_nutrient_outliers <- function(df) {
  df %>%
    mutate(
      NOx = if_else(NOx %in% c(-0.9, -999) | NOx > 100, NA_real_, NOx),
      NH4 = if_else(NH4 %in% c(-0.9, -999) | NH4 > 100, NA_real_, NH4)
    )
}

#' Assign ocean basin based on coordinates
assign_ocean_region <- function(df) {
  df %>%
    mutate(
      ocean = case_when(
        lat < -50 ~ "Southern",
        lat > 60 ~ "Arctic",
        lon > 20 & lon <= 100 & lat > 40 ~ "BlackSea",
        lon > 20 & lon <= 100 ~ "Indian",
        lon <= 20 & lon >= -100 ~ "Atlantic",
        TRUE ~ "Pacific"
      )
    )
}

#' Base helper for downloading missing external dataset files
download_if_missing <- function(file_path, url, is_zip = FALSE, extract_dir = NULL) {
  if (file.exists(file_path)) {
    return(file_path)
  }
  
  dest_dir <- dirname(file_path)
  dir.create(dest_dir, recursive = TRUE, showWarnings = FALSE)
  
  message(sprintf("Dataset file missing: %s\nDownloading from official source...", basename(file_path)))
  options(timeout = max(600, getOption("timeout"))) # タイムアウトを10分に延長
  
  if (is_zip) {
    tmp_zip <- file.path(dest_dir, "temp_download.zip")
    download.file(url, destfile = tmp_zip, mode = "wb", quiet = FALSE)
    message("Extracting zip archive...")
    unzip(tmp_zip, exdir = ifelse(is.null(extract_dir), dest_dir, extract_dir))
    if (file.exists(tmp_zip)) file.remove(tmp_zip)
  } else {
    download.file(url, destfile = file_path, mode = "wb", quiet = FALSE)
  }
  
  if (file.exists(file_path)) {
    message(sprintf("SUCCESS: %s downloaded successfully.", basename(file_path)))
  } else {
    warning(sprintf("Download completed, but target file '%s' was not found.", file_path))
  }
  
  return(file_path)
}

#' Ensure GLODAPv2.2023 merged master file exists
ensure_glodap <- function(dest_dir = here::here("data/external_databases")) {
  target_file <- file.path(dest_dir, "GLODAPv2.2023_Merged_Master_File.csv")
  url <- "https://www.glodap.info/glodap_files/v2.2023/GLODAPv2.2023_Merged_Master_File.csv"
  download_if_missing(target_file, url, is_zip = FALSE)
}

#' Ensure Hansell DOM (2022) dataset exists
ensure_hansell_dom <- function(dest_dir = here::here("data/external_databases")) {
  target_file <- file.path(dest_dir, "All_Basins_Data_Merged_Hansell_2022.xlsx")
  # NOAA NCEI Accession 0227166 直リンク
  url <- "https://www.ncei.noaa.gov/data/oceans/ncei/ocads/data/0227166/All_Basins_Data_Merged_Hansell_2022.xlsx"
  download_if_missing(target_file, url, is_zip = FALSE)
}

#' Ensure Longhurst World Marine Provinces shapefiles exist
ensure_longhurst <- function(dest_dir = here::here("data/shapefiles/Longhurst_world_v4_2010")) {
  target_file <- file.path(dest_dir, "Longhurst_world_v4_2010.shp")
  # Marine Regions 公式直リンク
  url <- "https://www.marineregions.org/direct_download.php?name=Longhurst_world_v4_2010.zip"
  download_if_missing(target_file, url, is_zip = TRUE, extract_dir = dest_dir)
}