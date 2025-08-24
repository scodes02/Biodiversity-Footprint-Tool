# THIS DOES NOT NEED TO BE RUN BY PEOPLE SEEKING TO APPLY THIS FOOTPRINT TOOL

# Load Packages ----

library(tidyverse)

# Load in data ----

exiobase_lookup <- read_excel(
  'alternative_exiobase_mapping/ABS_ENCORE_BFD.xlsx'
)

basic_price_factors <- read_csv(
  'currency_handling/EXIOBASE_basic_price_factors.csv'
)

# Extract relevant values ----

## Basic prices ----

# Basic prices are in `basic_price_factors`, and values for different EXIOBASE accounts
# were calculated using ABS data

## Inflation from 2019 to 2023 ----

# inflation_dataframe <- retrieve_inflation_data('Australia')

# inflation_adjustment <- adjust_for_inflation(
#   price = 1,
#   from_date = 2023,
#   to_date = 2019,
#   country = 'Australia',
#   inflation_dataframe = inflation_dataframe
# )

inflation = 0.8900236

## Currency adjustment at 2019 from AUD to EUR ----

# currency_adjustment <- convert_currencies(price_start = 1,
#                                           from = 'AUD',
#                                           to = 'EUR',
#                                           date = lubridate::ymd(20190630))

# This is the extracted value
currency_adjustment <- 0.6180943


# Create dataframe ----

basic_price_exiobase <- basic_price_factors |>
  left_join(
    exiobase_lookup,
    by = c('EXIOBASE_Code' = 'ABS_EXIOBASE_Name')
  ) |>
  rename(ABS_EXIOBASE_Name = EXIOBASE_Code) |>
  mutate(
    EUR_AUD_coversion = currency_adjustment,
    inflation_2023_2019 = inflation
  ) |>
  select(
    BFD_EXIOBASE_Name,
    basic_price_factor,
    EUR_AUD_coversion,
    inflation_2023_2019
  )

# Write to file ----

write_csv(
  basic_price_exiobase,
  'currency_handling/final_currency_converter.csv'
)
