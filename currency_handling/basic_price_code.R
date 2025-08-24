## Mapping EXIOBASE to IOIC for Basic Price Conversion -------------------------

# Packages and Data Import and Clean -------------------------------------------
library(tidyverse)
library(readxl)
library(janitor)
library(fuzzyjoin)
library(writexl)

# Procurement Import and Clean --------------------------------------------
procurement <- read_excel(
  'procurement_data/procurement_data/2023 Procurement Data.xlsx'
)

# Import the most recent ABS Data ----------------------------------------------
ABS_IOT <- read_excel(
  'basic_prices/Australian National Accounts_Input-Output Tables_2021_22.xlsx',
  sheet = 'Table 4'
)

# Clean the data: Use the first cell in row 1 as the header and remove all other row 1 data
ABS_IOT_clean <- ABS_IOT |>
  rename_with(
    ~ ifelse(. == names(ABS_IOT)[1], ABS_IOT[[1, 1]], .),
    .cols = 1
  ) |>
  slice(-1) |>
  slice((-117:-134)) |>
  remove_empty(which = "rows") |>
  rename(
    ABS_Product = `Table 4. RECONCILIATION OF FLOWS AT BASIC PRICES AND AT PURCHASERS' PRICES BY PRODUCT GROUP`
  ) |>
  select(-`PRODUCT GROUP`)

# Import ABS to EXIOBASE mapping file from Richard Wood ------------------------
exiobase_ABS_mapping <- read_excel(
  'basic_prices/IOIC_EXIOBASEp.xlsx',
  sheet = "IOIC2019_EXIOp"
)


# Rename column one to make sense
exiobase_ABS_mapping <- exiobase_ABS_mapping |>
  rename(ABS_Product = 1) |>
  slice(4:n()) |>
  select(-2)

# Now pivot all other columns into “EXIOBASE_Code” + “value”.
# Filter down to the rows where value==1, i.e. the row indicates a match
abs_lookup_table <- exiobase_ABS_mapping |>
  pivot_longer(
    cols = -ABS_Product,
    names_to = "EXIOBASE_Code",
    values_to = "indicator"
  ) |>
  filter(indicator == 1) |>
  select(ABS_Product, EXIOBASE_Code)

# Load the EXIOBASE categories that we have used -------------------------------
ISIC_EXIOBASE_NACE <- read_csv(
  'alternative_exiobase_mapping/EXIOBASE_NACE_ISIC.csv'
)

# Only retain the unique columns
EXIOBASE_categories <- ISIC_EXIOBASE_NACE |>
  select(EXIOBASE) |>
  distinct()

# Match the different codes ----------------------------
fuzzy_joined_data <- stringdist_left_join(
  abs_lookup_table,
  EXIOBASE_categories,
  by = c("EXIOBASE_Code" = "EXIOBASE"),
  method = "jw",
  max_dist = 0.2
)

# Where a fuzzy join was successful, replace the "EXIOBASE_CODE" with the "EXIOBASE" code
cleaned_ABS_exiobase_mapping <- fuzzy_joined_data |>
  mutate(
    EXIOBASE_Code = case_when(
      !is.na(EXIOBASE) ~ EXIOBASE,
      TRUE ~ EXIOBASE_Code
    )
  )

# Mapping EXIOBASE to IOIC in ABS Data ----

# Combine the ABS data with the EXIOBASE classifications and only retain the columns necessary for our basic price conversion
ABS_with_EXIOBASE <- ABS_IOT_clean |>
  mutate(
    Product_Group_Lower = tolower(ABS_Product)
  ) |>
  left_join(
    cleaned_ABS_exiobase_mapping |>
      mutate(ABS_Product_Lower = tolower(ABS_Product)),
    by = c("Product_Group_Lower" = "ABS_Product_Lower")
  ) |>
  select(-Product_Group_Lower, -ABS_Product.y) |>
  rename(ABS_Product = ABS_Product.x) |>
  relocate(EXIOBASE_Code, .after = 1) |>
  rename(
    Basic_Prices = `Final Uses ;\r\n Basic Prices`,
    Taxes = `Final Uses ;\r\n Net Taxes on Products`,
    Margin = `Final Uses ;\r\n Margin`,
    Purchaser_Price = `Final Uses ;\r\n Purchaser's Prices`
  ) |>
  relocate(EXIOBASE_Code, .after = 1) |>
  select(
    ABS_Product,
    EXIOBASE_Code,
    Basic_Prices,
    Taxes,
    Margin,
    Purchaser_Price
  )

# Calculate the basic price factors, then take the mean BP factor for each EXIOBASE Product
EXIOBASE_basic_price_factors <- ABS_with_EXIOBASE |>
  mutate(
    basic_price_factor = Basic_Prices / Purchaser_Price, # Calculate the basic price factor
  ) |>
  group_by(EXIOBASE_Code) |>
  summarise(
    basic_price_factor = mean(basic_price_factor, na.rm = TRUE) # Calculate the mean basic price factor for each EXIOBASE code
  ) |>
  mutate(
    basic_price_factor = case_when(
      basic_price_factor == Inf ~ 1, # Replace infinite values with 1
      basic_price_factor < 0 ~ 1, # Replace negative values with 1
      basic_price_factor > 1 ~ 1, # Sets a maximum of 1 for the basic price conversion
      TRUE ~ basic_price_factor
    )
  )

fuzzy_exiobase <- stringdist_left_join(
  procurement_clean,
  EXIOBASE_basic_price_factors,
  by = c("Process" = "EXIOBASE_Code"),
  method = "jw",
  max_dist = 0.2
)

data <- left_join(
  procurement_clean,
  EXIOBASE_basic_price_factors,
  by = c("Process" = "EXIOBASE_Code")
)

################################################################################
# Making an excel file for all the EXIOBASE classifications --------------------
################################################################################

## Make the EXIOBASE Mapping file
write_xlsx(data, 'basic_prices/procurement_clean.xlsx')

# ENCORE Exiobase Classifications
ENCORE_EXIOBASE <- ISIC_EXIOBASE_NACE |>
  select(EXIOBASE) |>
  rename(ENCORE_EXIOBASE_Name = EXIOBASE) |>
  distinct()

# write_xlsx(ENCORE_EXIOBASE, 'alternative_exiobase_mapping/ENCORE_EXIOBASE.xlsx')

# ABS Exiobase Classifications
ABS_EXIOBASE_Factors <- EXIOBASE_basic_price_factors |>
  select(EXIOBASE_Code) |>
  rename(ABS_EXIOBASE_Name = EXIOBASE_Code)

# write_xlsx(ABS_EXIOBASE_Factors, 'alternative_exiobase_mapping/ABS_EXIOBASE_Factors.xlsx')

# Biodiversity Footprint Database Factors
BDFD_Factors <- all_exiobase_data_clean |>
  select(-c(1:6)) |>
  pivot_longer(
    cols = everything(),
    names_to = "Column_Name",
    values_to = "Value"
  ) |>
  select(-Value) |>
  rename(BDFD_EXIOBASE_Name = Column_Name) |>
  distinct(BDFD_EXIOBASE_Name, .keep_all = TRUE)

# write_xlsx(BDFD_Factors, 'alternative_exiobase_mapping/BDFD_Factors.xlsx')

################################################################################
# Import the mapped data and test it
################################################################################

mapped_all_three <- read_xlsx(
  'alternative_exiobase_mapping/ABS_ENCORE_BFD.xlsx'
)

# Check for duplicates and merging: ABS ----

# number of unique ABS columns
unique_ABS <- mapped_all_three |>
  select(ABS_EXIOBASE_Name) |>
  filter(!is.na(ABS_EXIOBASE_Name)) |>
  mutate(Check = "YES")

length(unique(unique_ABS$ABS_EXIOBASE_Name))

duplicates <- unique_ABS |>
  group_by(ABS_EXIOBASE_Name) |>
  filter(n() > 1) |>
  distinct()

abs_merge_check <- left_join(
  abs_lookup_table,
  unique_ABS,
  by = c("EXIOBASE_Code" = "ABS_EXIOBASE_Name")
)

abs_merge_check <- stringdist_left_join(
  abs_lookup_table,
  unique_ABS,
  by = c("EXIOBASE_Code" = "ABS_EXIOBASE_Name"),
  max_dist = 5, # Set the maximum allowable distance for matching
  distance_col = "string_distance" # Optional: include a column for the string distance
)

# Check for duplicates and merging: ENCORE ----

unique_ENCORE <- mapped_all_three |>
  select(ENCORE_EXIOBASE_Name) |>
  filter(!is.na(ENCORE_EXIOBASE_Name))

length(unique(unique_ENCORE$ENCORE_EXIOBASE_Name))

duplicates <- unique_ENCORE |>
  group_by(ENCORE_EXIOBASE_Name) |>
  filter(n() > 1) |>
  distinct()

# Check for duplicates and merging: BFD ----

unique_BFD <- mapped_all_three |>
  select(BFD_EXIOBASE_Name) |>
  filter(!is.na(BFD_EXIOBASE_Name))

length(unique(unique_BFD$BFD_EXIOBASE_Name))

duplicates <- unique_BFD |>
  group_by(BFD_EXIOBASE_Name) |>
  filter(n() > 1) |>
  distinct()
