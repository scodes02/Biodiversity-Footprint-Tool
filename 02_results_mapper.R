######### Biodiversity Footprint Mapper ##########################################################
# This script visualises the geographic distribution of biodiversity impacts associated with
# procurement activities.
# It provides a coarse, indicative overview.
# Some values may be inaccurate, and the results do not account for the specific characteristics of
# your organisation’s value chain.
# Three types of maps are created:
## 1. Overall PDF distribution across countries producing one map.
## 2. PDF distribution per procurement activity, producing one map for each activity (~200 maps).
## 3. PDF distribution per impact driver, producing one map for each type of impact driver (~26 maps).
# Therefore, this workflow should be used as an initial scoping tool to identify potential impact
# hotspots through a heatmap-style analysis.
# It must be supplemented with detailed, supplier-specific investigation as part of a comprehensive
# and context-sensitive procurement strategy.

######### Instructions ##########################################################
# After executing `01_biodiversity_footprint_tool.R`, the necessary input data for this workflow
# will be available.
# Run this script to generate spatial maps of biodiversity impact distributions.
# Processing time will vary depending on local machine specifications, typically ranging from 1 to 5 minutes.

# ---- Set up -------------------------------------------------------------------------------

## ---- Load Packages -----------------------------------------------------------------------

required_packages <- c("tidyverse", "rnaturalearth", "terra", "tidyterra")

installed <- required_packages %in% rownames(installed.packages())
if (any(!installed)) {
  install.packages(required_packages[!installed])
}

suppressPackageStartupMessages({
  library(tidyverse)
  library(rnaturalearth)
  library(terra)
  library(tidyterra)
})

## ---- Load Data -----------------------------------------------------------------------

# Loads procurement impact data and removes "Global", as these are climate impacts with
# the distribution not being relevant or provided geographically
procurement_endpoints <- read_csv("results/impact_maps/all_procurement_impact_data.csv") |>
  filter(LC_Impact_Countries != "Global")

## ---- World Map -----------------------------------------------------------------------

countries <- geodata::world(path = ".")

country_lookup <- procurement_endpoints |>
  mutate(
    LC_Impact_Countries = case_when(
      LC_Impact_Countries == "Byelarus" ~ "Belarus",
      LC_Impact_Countries == "Brunei Darussalam" ~ "Brunei",
      LC_Impact_Countries == "Cape Verde" ~ "Cabo Verde",
      LC_Impact_Countries == "Ivory Coast" ~ "Côte d'Ivoire",
      LC_Impact_Countries == "Curacao" ~ "Curaçao",
      LC_Impact_Countries == "Congo DRC" ~ "Democratic Republic of the Congo",
      LC_Impact_Countries == "Zaire" ~ "Democratic Republic of the Congo",
      LC_Impact_Countries == "Timore-Leste" ~ "East Timor",
      LC_Impact_Countries == "Swaziland" ~ "Eswatini",
      LC_Impact_Countries == "Gambia, The" ~ "Gambia",
      LC_Impact_Countries == "China, Hong Kong Special Administrative Region" ~ "Hong Kong",
      LC_Impact_Countries == "The Former Yugoslav Republic of Macedonia" ~ "Macedonia",
      LC_Impact_Countries == "Myanmar (Burma)" ~ "Myanmar",
      LC_Impact_Countries == "Russian Federation" ~ "Russia",
      LC_Impact_Countries == "Sao Tome and Principe" ~ "São Tomé and Príncipe",
      LC_Impact_Countries == "South Georgia" ~ "South Georgia and the South Sandwich Islands",
      LC_Impact_Countries == "Svalbard" ~ "Svalbard and Jan Mayen",
      LC_Impact_Countries == "Tanzania, United Republic of" ~ "Tanzania",
      LC_Impact_Countries == "US Virgin Islands" ~ "Virgin Islands, U.S.",
      TRUE ~ LC_Impact_Countries
    )
  ) |>
  mutate(
    Exiobase_region = case_when(
      LC_Impact_Countries == "Brunei" ~ "RoW Asia and Pacific",
      LC_Impact_Countries == "Belarus" ~ "RoW Europe",
      TRUE ~ Exiobase_region
    )
  ) |>
  filter(
    !(LC_Impact_Countries == "Western Sahara" &
      Exiobase_region == "RoW Asia and Pacific"),
    !(LC_Impact_Countries == "Democratic Republic of the Congo" &
      Exiobase_region == "RoW Middle East")
  ) |>
  select(LC_Impact_Countries, Exiobase_region) |>
  distinct()


# ---- Generate Maps ------------------------------------------------------------------

## ---- Overall PDF distribution ------------------------------------------------------

# Creates a map showing the overall PDF distribution across countries
# Impacts tend to be concentrated in Australia
out_dir <- "results/impact_maps/overall_PDF_distribution"
if (!dir.exists(out_dir)) {
  dir.create(out_dir, recursive = TRUE)
}

overall_pdf_distribution <- procurement_endpoints |>
  select(
    LC_Impact_Countries,
    Endpoint_Impact
  ) |>
  group_by(LC_Impact_Countries) |>
  summarise(
    Total_PDF = sum(Endpoint_Impact, na.rm = TRUE),
    .groups = "drop"
  )

# Prepare map data (join countries with total PDF per country)
map_data_overall_pdf <- countries |>
  tidyterra::left_join(
    country_lookup |>
      left_join(overall_pdf_distribution, by = "LC_Impact_Countries"),
    by = c("NAME_0" = "LC_Impact_Countries")
  )

# Save PDF map
pdf("results/impact_maps/overall_PDF_distribution/map_Overall_Total_PDF.pdf", width = 10, height = 6)

print(
  ggplot(map_data_overall_pdf) +
    geom_spatvector(aes(fill = Total_PDF)) +
    paletteer::scale_fill_paletteer_c(
      "ggthemes::Sunset-Sunrise Diverging",
      na.value = "#b3b3b3",
      name = "Total PDF"
    ) +
    ggtitle("Total PDF Impact by Country") +
    theme_bw() +
    theme(plot.title = element_text(hjust = 0.5))
)

dev.off()


## ---- PDF per activity ------------------------------------------------------

# For each economic activity, this produces a map showing the distribution of the underlying PDF
# across countries. This is useful to see the distribution of impacts from different procurement activities.

# Aggregate PDF for each procurement activity by country
total_product_impact <- procurement_endpoints |>
  select(
    LC_Impact_Countries,
    Endpoint_Impact,
    Activity
  ) |>
  group_by(
    LC_Impact_Countries,
    Activity
  ) |>
  summarise(
    Total_PDF = sum(Endpoint_Impact, na.rm = TRUE),
    .groups = "drop"
  )

# Output directory
out_dir <- "results/impact_maps/PDF_per_activity"
if (!dir.exists(out_dir)) {
  dir.create(out_dir, recursive = TRUE)
}

# Generate a map for every individual procurement activity
for (impact in unique(total_product_impact$Activity)) {
  map_data_activity <- countries |>
    tidyterra::left_join(
      country_lookup,
      by = c("NAME_0" = "LC_Impact_Countries")
    ) |>
    tidyterra::left_join(
      total_product_impact |>
        filter(Activity == impact),
      by = c("NAME_0" = "LC_Impact_Countries")
    )

  file_stub <- gsub("[^[:alnum:]_.\\-]+", "_", impact, perl = TRUE)

  pdf(
    file = file.path(out_dir, sprintf("PDF_from_%s.pdf", file_stub)),
    width = 10,
    height = 6
  )
  print(
    ggplot(map_data_activity) +
      geom_spatvector(aes(fill = Total_PDF)) +
      paletteer::scale_fill_paletteer_c(
        "ggthemes::Sunset-Sunrise Diverging",
        na.value = "#b3b3b3",
        name = "Total PDF"
      ) +
      ggtitle(impact) +
      theme_bw() +
      theme(plot.title = element_text(hjust = 0.5))
  )
  dev.off()
}

## ---- PDF impact driver ------------------------------------------------------

# For each type of impact, this produces a map showing the distribution of the underlying PDF
# across countries. This is useful to see the distribution of impact types.

# Aggregate PDF for each procurement impact driver by country
total_impact_per_driver <- procurement_endpoints |>
  select(
    LC_Impact_Countries,
    Endpoint_Impact,
    Impact_Driver
  ) |>
  group_by(
    LC_Impact_Countries,
    Impact_Driver
  ) |>
  summarise(
    Total_PDF = sum(Endpoint_Impact, na.rm = TRUE),
    .groups = "drop"
  )

# Output directory
out_dir <- "results/impact_maps/PDF_per_impact_driver"
if (!dir.exists(out_dir)) {
  dir.create(out_dir, recursive = TRUE)
}

# Generate a map for every individual impact driver
for (impact in unique(total_impact_per_driver$Impact_Driver)) {
  map_data_impact_driver <- countries |>
    tidyterra::left_join(
      country_lookup,
      by = c("NAME_0" = "LC_Impact_Countries")
    ) |>
    tidyterra::left_join(
      total_impact_per_driver |>
        filter(Impact_Driver == impact),
      by = c("NAME_0" = "LC_Impact_Countries")
    )

  file_stub <- gsub("[^[:alnum:]_.\\-]+", "_", impact, perl = TRUE)

  pdf(
    file = file.path(out_dir, sprintf("PDF_from_%s.pdf", file_stub)),
    width = 10,
    height = 6
  )
  print(
    ggplot(map_data_impact_driver) +
      geom_spatvector(aes(fill = Total_PDF)) +
      paletteer::scale_fill_paletteer_c(
        "ggthemes::Sunset-Sunrise Diverging",
        na.value = "#b3b3b3",
        name = "Total PDF"
      ) +
      ggtitle(impact) +
      theme_bw() +
      theme(plot.title = element_text(hjust = 0.5))
  )
  dev.off()
}
