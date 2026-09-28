# R/00_config_and_palettes.R
# Project Configurations, Color Palettes, and Global Constants

suppressPackageStartupMessages({
  library(tidyverse)
  library(magrittr)
  library(sf)
  library(patchwork)
  library(MetBrewer)
  library(ggsci)
  library(viridis)
  library(ggbeeswarm)
  library(ggdist)
  library(MASS)
  library(interp)
  library(car)
  library(readxl)
  library(data.table)
  library(here)
  library(fields)
})

# Custom Isle of Dogs palette
get_isle_of_dogs_palette <- function() {
  key <- rgb(
    c(0.145, 0.201, 0.290, 0.379, 0.478, 0.614, 0.751, 0.882, 0.911, 0.907, 0.904, 0.902, 0.949, 0.741, 0.607, 0.552, 0.566, 0.713, 0.531, 0.686, 0.853, 0.964, 0.820),
    c(0.141, 0.251, 0.424, 0.596, 0.731, 0.751, 0.763, 0.767, 0.662, 0.522, 0.382, 0.306, 0.944, 0.840, 0.982, 0.809, 0.637, 0.608, 0.897, 0.992, 0.973, 0.704, 0.453),
    c(0.365, 0.420, 0.508, 0.595, 0.643, 0.567, 0.484, 0.402, 0.358, 0.325, 0.292, 0.275, 0.779, 0.628, 0.723, 0.925, 0.991, 0.912, 0.480, 0.457, 0.431, 0.352, 0.277)
  )
  colorRampPalette(key[1:13])
}

# Red-Blue gradient palette
get_red_blue_palette <- function() {
  colorRampPalette(c("#00007F", "blue", "#007FFF", "white", "#FF7F00", "red", "#7F0000"))
}

# Standard nutrient colors
NUTRIENT_COLORS <- c(
  "ureaN" = "#1f78b4",
  "NH4"   = "#33a02c",
  "NO3"   = "#e31a1c"
)
select <- dplyr::select