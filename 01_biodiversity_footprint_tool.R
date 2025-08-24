######### Biodiversity Footprint Tool ##########################################################
# This workflow performs a biodiversity footprint assessment for an organisation.
# The modelling methods (impact factors) are optimised for universities operating in Victoria, Australia.
# This workflow can still be applied to any type of organisation in any location within Australian.
# If a more accurate assessment is desired, the workflow can be tweaked to suit your specific organisations.
# Contact sam.hickman@unimelb.edu.au to ask how this could be completed.

######### Instructions #########################################################################
# To use this workflow, update the "input_file.xlsx" with your organisation's activity data.
# Once configured, this workflow runs end-to-end and generates an Excel workbook containing the results.

# ---- Set-up ---------------------------------------------------------------

## ---- Packages --------------------------------------------------

required_packages <- c("tidyverse", "readxl", "writexl")

installed <- required_packages %in% rownames(installed.packages())
if (any(!installed)) {
  install.packages(required_packages[!installed])
}

suppressPackageStartupMessages({
  library(tidyverse)
  library(readxl)
  library(writexl)
})

## ---- Functions --------------------------------------------------

# Assists in converting AUD 2023 to basic prices EUR 2019
adjust_to_eur2019_basic <- function(aud_2023, inflation_2023_2019, basic_price_factor, eur_aud_conversion) {
  aud_2023 *
    inflation_2023_2019 *
    basic_price_factor *
    eur_aud_conversion
}

## ---- Load data ------------------------------------------------------------

# Input file -> UPDATE THIS FILE with your organisational activities
input_data <- read_excel("input_file.xlsx")

# Impact factors used to model the impacts activities on biodiversity
nonproc_midpoint_factors <- read_csv("processed_data/non_procurement_midpoints.csv")
exiobase_wide_factors <- read_csv("processed_data/procurement_midpoints.csv")
lc_impact_endpoint_factors <- read_csv("processed_data/lc_impact_factors.csv")

# Currency / inflation / basic price adjustment values
# Created in `basic_price_code.R` and `exiobase_currency_lookup.R`
exiobase_price_adjust <- read_csv("currency_handling/final_currency_converter.csv")

## ---- Reshape EXIOBASE factors ---------------------------------------------

exiobase_long_factors <- exiobase_wide_factors |>
  pivot_longer(
    cols = -c(
      LC_Impact_Countries,
      Exiobase_region,
      Exiobase_region_abbreviation,
      Sheet_tab,
      Impact_Driver,
      Unit
    ),
    names_to = "Process",
    values_to = "Factor_Value"
  ) |>
  select(-Unit)

# Define midpoint factors
exiobase_midpoint_factors <- exiobase_long_factors |>
  filter(Sheet_tab != "PDFperEuro")

# Define endpoint factors
exiobase_endpoint_factors <- exiobase_long_factors |>
  filter(Sheet_tab == "PDFperEuro")


# ---- Midpoint impact calculations -----------------------------------------------------

## Non-procurement
nonproc_midpoints <- input_data |>
  filter(Group != "Procurement") |>
  left_join(nonproc_midpoint_factors, by = "Process") |>
  # Remove toxicity rows due to LC-Impact not supporting them appropriately
  filter(Impact_Factor_Unit != "kg 1,4-DCB") |>
  mutate(
    Midpoint_Impact = Quantity * Impact_Factor
  ) |>
  rename(Midpoint_Unit = Impact_Factor_Unit) |>
  select(
    Group,
    Activity,
    Quantity,
    Unit,
    Process,
    Damage_Pathway,
    Midpoint_Unit,
    Midpoint_Impact
  )

## Procurement
procurement_midpoints <- input_data |>
  filter(Group == "Procurement") |>
  # Attach price/inflation/currency factors
  left_join(
    exiobase_price_adjust,
    by = c("Process" = "BFD_EXIOBASE_Name")
  ) |>
  # Adjust for basic prices, inflation, and currency conversion
  mutate(
    Adjusted_Quantity = adjust_to_eur2019_basic(
      aud_2023 = Quantity,
      inflation_2023_2019 = inflation_2023_2019,
      basic_price_factor = basic_price_factor,
      eur_aud_conversion = EUR_AUD_coversion
    )
  ) |>
  # Join exiobase factors
  left_join(exiobase_midpoint_factors, by = "Process") |>
  # Calculate midpoint impacts (e.g. CO2-eq, m^3 of water, m^2 of land, etc.)
  mutate(Midpoint_Impact = Adjusted_Quantity * Factor_Value)

# Clean and aggregate midpoint procurement impacts by activity & driver
procurement_midpoints_clean <- procurement_midpoints |>
  select(
    Group,
    Activity,
    Quantity,
    Unit,
    Process,
    Impact_Driver,
    Midpoint_Impact
  ) |>
  group_by(
    Activity,
    Impact_Driver
  ) |>
  summarise(
    Total_Midpoint_Impact = sum(Midpoint_Impact, na.rm = TRUE),
    Group = first(Group),
    Quantity = first(Quantity),
    Unit = first(Unit),
    Process = first(Process),
    .groups = "drop"
  )


# ---- Endpoint (biodiversity) impact calculations -------------------------------------

## Non-procurement endpoints via LC-Impact
nonproc_endpoints <- nonproc_midpoints |>
  left_join(
    lc_impact_endpoint_factors,
    by = c("Damage_Pathway" = "Impact")
  ) |>
  # Remove toxicity rows due to LC-Impact not supporting them appropriately
  filter(Input_Unit != "kg 1,4-DCB") |>
  mutate(Endpoint_Impact = Midpoint_Impact * Endpoint_Factor) |>
  group_by(
    Group,
    Activity,
    Quantity,
    Unit,
    Process
  ) |>
  summarise(
    Endpoint_Impact = sum(Endpoint_Impact, na.rm = TRUE),
    Endpoint_Unit = first(Endpoint_Unit),
    .groups = "drop"
  )

## Procurement endpoints via EXIOBASE PDFperEuro
procurement_endpoints <- input_data |>
  filter(Group == "Procurement") |>
  left_join(
    exiobase_price_adjust,
    by = c("Process" = "BFD_EXIOBASE_Name")
  ) |>
  # Adjust for basic prices, inflation, and currency conversion
  mutate(
    Adjusted_Quantity = adjust_to_eur2019_basic(
      aud_2023 = Quantity,
      inflation_2023_2019 = inflation_2023_2019,
      basic_price_factor = basic_price_factor,
      eur_aud_conversion = EUR_AUD_coversion
    )
  ) |>
  # Join exiobase factors
  left_join(exiobase_endpoint_factors, by = "Process") |>
  # Calculate endpoint impacts (potentially disappear fraction (PDF))
  mutate(Endpoint_Impact = Adjusted_Quantity * Factor_Value)

## Aggregate procurement endpoints by activity & driver (retain driver)
procurement_endpoints_clean <- procurement_endpoints |>
  select(
    Group,
    Activity,
    Quantity,
    Unit,
    Process,
    Impact_Driver,
    Endpoint_Impact
  ) |>
  group_by(Activity, Impact_Driver) |>
  summarise(
    Total_Endpoint_Impact = sum(Endpoint_Impact, na.rm = TRUE),
    Group = first(Group),
    Quantity = first(Quantity),
    Unit = first(Unit),
    Process = first(Process),
    .groups = "drop"
  )

# ---- Wide/long tables for excel sheet of results --------------------------------------

## Midpoints (wide)
final_nonproc_midpoints_wide <- nonproc_midpoints |>
  mutate(Damage_Pathway = str_c(Damage_Pathway, " (", Midpoint_Unit, ")")) |>
  select(-Midpoint_Unit) |>
  pivot_wider(
    id_cols = c(Group, Activity, Quantity, Unit, Process),
    names_from = Damage_Pathway,
    values_from = Midpoint_Impact
  )

final_procurement_midpoints_wide <- procurement_midpoints_clean |>
  pivot_wider(
    id_cols = c(Group, Activity, Quantity, Unit, Process),
    names_from = Impact_Driver,
    values_from = Total_Midpoint_Impact
  )

## Endpoints (already aggregated above)

final_nonproc_endpoints <- nonproc_endpoints

final_procurement_endpoints_wide <- procurement_endpoints_clean |>
  pivot_wider(
    id_cols = c(Group, Activity, Quantity, Unit, Process),
    names_from = Impact_Driver,
    values_from = Total_Endpoint_Impact
  )

## Long forms and total per activity

# Non-procurement: single aggregated endpoint value per row; carry unit into a label
nonproc_endpoints_long <- final_nonproc_endpoints |>
  mutate(
    Impact_Category = str_c("LC-Impact aggregate (", Endpoint_Unit, ")")
  ) |>
  select(
    Group,
    Activity,
    Quantity,
    Unit,
    Process,
    Impact_Category,
    Endpoint_Impact
  )

# Procurement: multiple drivers are columns -> melt to long
procurement_endpoints_long <- final_procurement_endpoints_wide |>
  pivot_longer(
    cols = -c(Group, Activity, Quantity, Unit, Process),
    names_to = "Impact_Category",
    values_to = "Endpoint_Impact"
  ) |>
  mutate(Endpoint_Impact = as.numeric(Endpoint_Impact))

# Combine
combined_endpoints_long <- bind_rows(
  nonproc_endpoints_long,
  procurement_endpoints_long
)

# Total per activity
summary_endpoint_impacts <- combined_endpoints_long |>
  group_by(
    Group,
    Activity,
    Quantity,
    Unit,
    Process
  ) |>
  summarise(
    Total_Endpoint_Impact = sum(Endpoint_Impact, na.rm = TRUE),
    .groups = "drop"
  )


# ---- Export excel file ---------------------------------------------------------------

# Save excel file of all results
## Ensure results folder exists
if (!dir.exists("results")) {
  dir.create("results", recursive = TRUE)
}

## Save Excel file of all results
write_xlsx(
  list(
    # Wide tables for readability
    "Non-procurement midpoints" = final_nonproc_midpoints_wide,
    "Procurement midpoints" = final_procurement_midpoints_wide,
    # Long + summary
    "Endpoints_Summary_By_Act" = summary_endpoint_impacts
  ),
  path = "results/biodiversity_impact_workbook.xlsx"
)

# Save csv of procurement impacts with geographic distribution for `02_results_maper.R`
all_procurement_impact_data <- bind_rows(
  procurement_endpoints,
  procurement_midpoints
)
write_csv(all_procurement_impact_data, "results/impact_maps/all_procurement_impact_data.csv")
