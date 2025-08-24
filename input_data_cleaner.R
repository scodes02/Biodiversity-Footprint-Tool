# THIS DOES NOT NEED TO BE RUN BY PEOPLE SEEKING TO APPLY THIS FOOTPRINT TOOL

# THIS SECTION ONLY HAS RELEVANCE TO THE ANALYSIS COMPLETED IN 2024
# THIS DATA WILL HAVE TO BE EXTRACTED/RECALCULATED AND PUT INTO `INPUT_DATA_FILE.XLSX`
# BY THE UNIVERSITY OF MELBOURNE IN FUTURE ANALYSIS AND BY OTHER ORGANISATIONS WISHING
# TO REPLICATE THIS ANALYSIS

# MUCH OF THE UNDERYLING FILES WILL NOT BE SHARED ON THE PUBLIC GITHUB DUE TO COMMERICAL
# SENSITIVITY OF THE UNIVERSITY'S DATA

# THIS SEGMENT IS STILL SHARED SO THAT THERE IS FULL VISIBLITY INTO THE WORKFLOW

# Load Packages ----

library(tidyverse)
library(rvest)
library(rnaturalearth)
library(tidygeocoder)
library(geosphere)
library(priceR)
library(readxl)
library(writexl)

# Load Data ----

## Investment Data

investment_url <- "https://www.unimelb.edu.au/sustainabilityreport/domain-3-walking-the-talk-in-our-operations/responsible-investments"

webpage <- read_html(investment_url)

tables <- webpage |>
  html_table(fill = TRUE)

## Greenhouse Gas report

ghg_report_path <- 'GHG Data/University of Melbourne CY23 CA_Internal Inventory v8.1 (August 2023) VA.01.xlsx'

## Travel Data

student_commute <- read_excel(
  'travel_data/student/commuting/student_commute.xlsx'
)

international_data <- read_excel(
  'travel_data/student/international/international_students.xlsx'
)

## Procurement

procurement <- read_excel(
  'procurement_data/procurement_data/2023 Procurement Data.xlsx'
)

procurement_mapping <- read_csv(
  'alternative_exiobase_mapping/procurement_with_isic_and_exiobase.csv'
)

## EXIOBASE

exiobase_crosswalk <- read_excel('exiobase/different_exiobase_mappings.xlsx')

exiobase_lookup <- read_excel(
  "alternative_exiobase_mapping/ABS_ENCORE_BFD.xlsx"
)

## Basic Price Factors

basic_price_factors <- read_csv("basic_prices/EXIOBASE_basic_price_factors.csv")

# Extract and clean data for analysis ----

## Investment data ----

# Extract the desired data in table number `2` (adjust if needed)
emissions_table <- tables[[2]]

# Ensure the column names are correctly identified
colnames(emissions_table) <- make.names(colnames(emissions_table))

# Extract required information
investments_clean <- emissions_table |>
  filter(
    Equities.investments.as.of.31.Dec.2023 == "Total (Australian and International Equities)"
  ) |>
  rename(Quantity = UoM.financed.carbon.emissions) |>
  mutate(
    Quantity = as.numeric(gsub("[^0-9.-]", "", Quantity)), # Remove non-numeric characters
    Quantity = Quantity * 1e3, # Convert to kg CO2-e
    Group = "Investments",
    Activity = "Investments",
    Unit = "kg CO2-e",
    Process = "Investments"
  ) |>
  select(Group, Activity, Quantity, Unit, Process)

## Campus Operations ----

# This includes all on-campus activities. All data is extracted from the 2023 carbon report
# completed by Anthesis Australia. The code below then assigns it to an AusLCI modelling process.
# In the case of refrigerants, the reported GHG emissions figure is taken as there is no way
# to model refrigerant impacts with AusLCI.

## Extract the relevant data from the excel workbook ----

stationary_combustion <- read_excel(
  ghg_report_path,
  sheet = "Stationary Combustion",
  range = "B20:P30"
)

refrigerants <- read_excel(
  ghg_report_path,
  sheet = "Refrigerants",
  range = "B12:J13"
)

electricity <- read_excel(
  ghg_report_path,
  sheet = "Electricity",
  range = "C24:K40"
)

water <- read_excel(ghg_report_path, sheet = "Water", range = "B12:F14")

waste <- read_excel(ghg_report_path, sheet = "Waste", range = "B12:E16")

## Process combustion information ----

combustion_data <- stationary_combustion |>
  rename(Emission_Source = `Emission Source`) |>
  group_by(Emission_Source, UoM...9) |>
  summarise(Quantity = sum(Quantity, na.rm = TRUE), .groups = "drop") |>
  # Convert LPG from L to MJ by multiplying by 25
  mutate(
    Quantity = ifelse(
      Emission_Source == "Liquefied petroleum gas",
      Quantity * 25,
      Quantity
    ),
    Unit = ifelse(Emission_Source == "Liquefied petroleum gas", "MJ", UoM...9),
    Process = case_when(
      Emission_Source == "Liquefied petroleum gas" ~
        "heat, liquefied petroleum gas, at industrial furnace >100kW/MJ/RER",
      Emission_Source == "Natural Gas VIC (metro) (GJ)" ~ "natural gas,combusted, Victoria NGA values /AU U"
    ),
  ) |>
  select(Emission_Source, Quantity, Unit, Process) |>
  mutate(Process = as.character(Process))


## Process refrigerant data ----

# Clean refrigerant data - since we can only model the GHG emissions, and there
# is a lot of types that can be bracketed into "Refrigerants",

refrigerants_data <- refrigerants |>
  rename(Quantity = `2023`, Unit = `UoM`) |>
  mutate(Process = "Refrigerants") |>
  select(`Emission Source`, Quantity, Unit, Process) |>
  rename(Emission_Source = `Emission Source`) |>
  mutate(Process = as.character(Process))

## Process electricity data ----

electricity_data <- electricity |>
  rename(Emission_Source = `...1`, Quantity = `2023`) |>
  filter(
    Emission_Source %in%
      c("Total renewable electricity (grid + non grid)", "Residual Electricity")
  ) |>
  select(Emission_Source, Quantity) |>
  mutate(
    Unit = "kWh",
    Process = case_when(
      Emission_Source == "Residual Electricity" ~ "electricity, low voltage, Victoria/AU U",
      Emission_Source == "Total renewable electricity (grid + non grid)" ~ "IMPACTS ASSUMED TO BE IMMATERIAL"
    )
  ) |>
  select(Emission_Source, Quantity, Unit, Process) |>
  mutate(Process = as.character(Process))

## Process water consumption data ----

water_data <- water |>
  rename(
    Emission_Source = `Emissions source`,
    Quantity = `2023`,
    Unit = UoM
  ) |>
  mutate(Unit = "m3", Process = "Water and Wastewater Melbourne") |>
  select(Emission_Source, Quantity, Unit, Process) |>
  mutate(Process = as.character(Process))

## Process waste data ----

# Removes "Wastewater treatment - Melbourne"
# Combines "General waste (municipal waste)" and "Clinical waste incineration"
# Assigns AusLCI category

waste_data <- waste |>
  rename(Emission_Source = `Waste Type`, Unit = UoM) |>
  mutate(
    Emission_Source = case_when(
      Emission_Source %in%
        c("General waste (municipal waste)", "Clinical waste incineration") ~
        "General waste",
      TRUE ~ Emission_Source
    ),
    Process = case_when(
      Emission_Source == "General waste" ~ "general waste at landfill",
      Emission_Source == "Recycling" ~ "IMPACTS ASSUMED TO BE IMMATERIAL",
      TRUE ~ NA_character_
    )
  ) |>
  group_by(Emission_Source, Unit, Process) |>
  summarise(
    Quantity = sum(as.numeric(`2023`), na.rm = TRUE),
    .groups = "drop"
  ) |>
  filter(Emission_Source != "Wastewater treatment - Melbourne") |>
  mutate(
    Quantity = Quantity * 1000, # convert from tonne to kg
    Unit = "KG"
  ) |>
  select(Emission_Source, Quantity, Unit, Process) |>
  mutate(Process = as.character(Process))

## Combine to a single data frame ----

campus_data_clean <- bind_rows(
  combustion_data,
  refrigerants_data,
  electricity_data,
  water_data,
  waste_data
) |>
  mutate(Group = "Campus Operations") |>
  select(Group, Emission_Source, Quantity, Unit, Process) |>
  rename(Activity = Emission_Source)

## Travel ----

### Student Travel ----

## Process Data

student_commute_renamed <- student_commute |>
  rename(
    days = `How many days per week do you commute to university?`,
    first_method = `How do you primarily travel to university on a typical day?`,
    first_distance = `Approximately how many kilometres do you travel using your primary commuting method to university?`,
    second_method = `If you use a secondary method of travel to get to the university on a typical day, what is it?`,
    second_distance = `If you use a secondary method, how many kilometres do you typically travel to university with it?`,
    third_method = `If you use a tertiary method of travel to get to the university on a typical day, what is it?`,
    third_distance = `If you use a tertiary method, how many kilometres do you typically travel to university with it?`,
    fourth_method = `If you use a quaternary method of travel to get to the university on a typical day, what is it?`,
    fourth_distance = `If you use a quaternary method, how many kilometres do you typically travel to university with it?`
  ) |>
  select(
    ID,
    days,
    first_method,
    first_distance,
    second_method,
    second_distance,
    third_method,
    third_distance,
    fourth_method,
    fourth_distance
  )

student_commute_long <- student_commute_renamed |>
  pivot_longer(
    cols = c(
      first_method,
      first_distance,
      second_method,
      second_distance,
      third_method,
      third_distance,
      fourth_method,
      fourth_distance
    ),
    names_to = c("method_order", ".value"),
    names_pattern = "(first|second|third|fourth)_(method|distance)"
  ) |>
  # Convert method_order strings to numeric indicators
  mutate(
    method_number = case_when(
      method_order == "first" ~ 1,
      method_order == "second" ~ 2,
      method_order == "third" ~ 3,
      method_order == "fourth" ~ 4
    ),
    distance = as.numeric(distance)
  ) |>
  select(ID, days, method_number, method, distance) |>
  # Optionally remove rows with NA method or distance if desired
  filter(!is.na(method) & !is.na(distance))

# There are some serious outliers where students are taking the mickey
# with their responses.
# Students are probably not:
# - Walking 800km to university
# - Catching the tram for 5000km
# - Catching an 'other' means of transport for 1600km for a daily commute
# - Coming to campus 0 days per week if they have filled out this survey
#
# To resolve this we will set the following benchmarks:
# - Maximum walking distance is 20km. This is still very high, but given there are no
#         emissions, walking is immaterial regarding impacts regardless of 'true' distances.
# - Maximum tram route is 25km. The longest tram route in Melbourne is [22.8 km]
#         (https://en.wikipedia.org/wiki/Melbourne_tram_route_75#:~:text=Melbourne%20tram%20route%2075%20is,longest%20route%20on%20the%20network.),
#         but we will be generous for rounding.
# - Other transport will be ignored as there was only five responses.
# - Will transform 0 values into 1 values, assuming these students come to campus at least once per week.

student_commute <- student_commute_long |>
  select(ID, days, method_number, method, distance) |>
  filter(
    (method == "Tram" & distance <= 25) | # Include "Tram" with distance ≤ 25
      (method == "Walk" & distance <= 20) | # Include "Walk" with distance ≤ 20
      !(method %in% c("Tram", "Walk")) # Keep all other methods regardless of distance
  ) |>
  filter(method != "Other") |>
  mutate(days = ifelse(days == 0, 1, days)) # Replace 0 in 'days' column with 1

# Estimate student travel
#
# This method will estimate the total distance travelled by the student population, applying
# the same approach used by the consultant in 2023 to estimate travel distances for staff in the GHG report.
#
# 1. Aggregate Student Travel Distances
#    Use the survey data to sum the total kilometres travelled by responding students for each
#    transportation mode (e.g., public transport, car, bicycle, walking). This provides the
#    total distance travelled by mode.
#
# 2. Normalize by Respondent Count
#    Divide the total kilometres travelled (per transportation mode) by the total number of student
#    respondents, and multiply by two assuming a return trip per day.
#
#    This yields the average distance per student per day for each transportation mode.
#
# 3. Scale to the Entire Student Population
#    Multiply the average one-way trip distance (calculated in Step 2) by the total number of enrolled students
#
#    This estimates the total distance travelled by the student body for each day they come to campus.
#
# 4. Account for Commute Frequency
#    Calculate the average number of commuting days per week based on survey data.
#
#    Multiply the total one-way trip distance (from Step 3) by this average commute frequency to
#    estimate the total weekly commuting distance.
#
# 5. Estimate Semester Travel Distance
#    Determine the number of teaching weeks in a semester (e.g., 12--14 weeks).
#
#    Multiply the total weekly commuting distance (from Step 4) by the number of teaching weeks to
#    calculate the total student travel distance for the semester.
#

# Aggregate travel impacts by method

annual_distance <- student_commute |>
  group_by(method) |>
  summarise(distance = sum(distance, na.rm = TRUE), .groups = "drop") |>
  mutate(
    student_per_day = ((distance * 2) / length(unique(student_commute$ID))), # distance per week per student
    per_day_travel = student_per_day * 52000, #  52,000 students at unimelb
    annual_distance = per_day_travel *
      26 *
      mean(as.numeric(student_commute$days))
  ) |> # 26 weeks = 12 (teaching weeks + SWOTVAC) X 2
  select(method, distance, student_per_day, per_day_travel, annual_distance)

# colnames(student_commute)

annual_distance_final <- annual_distance |>
  mutate(
    method = recode(
      method,
      "Bus" = "Bus",
      "Car" = "Personal Car",
      "Ride/cycle" = "Bicycle",
      "Taxi" = "Taxi/rideshare",
      "Train" = "Train",
      "Tram" = "Tram",
      "Walk" = "Walk"
    )
  ) |>
  filter(method != "Electric Scooter")

student_commute_clean <- annual_distance_final |>
  rename(`Activity` = method, Quantity = annual_distance) |>
  mutate(
    Group = "Student Travel",
    Unit = "km/year",
    Process = case_when(
      # Match the appropriate effect factor.
      Activity == "Bus" ~ "Bus",
      Activity == "Personal Car" ~ "Petrol: Medium Car",
      Activity == "Bicycle" ~ "Bicycle",
      Activity == "Taxi/rideshare" ~ "Taxi - Melbourne",
      Activity == "Train" ~ "Train",
      Activity == "Tram" ~ "Light rail and tram",
      Activity == "Walk" ~ "Walk"
    )
  ) |>
  select(Group, Activity, Quantity, Unit, Process)

### International Student Travel ----

### Process Data

# The data contains three columns:
# - `country`: The countries the international students come from.
# - `students`: The number of students.
# - `city`: The most populous city which was added manually.

#### Get City coordinates

data_coordinates <- international_data |>
  geocode(city, method = 'osm', lat = latitude, long = longitude)

#### Calculate distances from Melbourne

melbourne_coordinates <- c(144.9631, -37.8136)

distance_data <- data_coordinates |>
  mutate(
    distance_to_melbourne_km = mapply(
      function(lat, lon) {
        distHaversine(c(lon, lat), melbourne_coordinates) / 1000 # Convert meters to kilometers
      },
      latitude,
      longitude
    )
  )

#### Calculate the p.km flown each year by students from each country

# Assumptions:
# - One return flight per year for each student.
# - All students come from the most populous city in their country of origin.
# - Because we don't have any flight details, we apply a 8% loading to the distance to account
#       for indirect flights and connections. 5% is typically applied to major hubs, and 10%
#       to more remote locations. 8% is a reasonable medium.

distance_flown <- distance_data |>
  select(country, students, distance_to_melbourne_km) |>
  mutate(
    passenger_km_per_student = distance_to_melbourne_km * 2 * 1.08, # First calculates the passenger.km flow by each student. Assumes one return trip, and applies an 8% uplift factor to account for the fact flights will not be perfectly direct.
    annual_passenger_km_per_country = passenger_km_per_student * students
  ) # Multiples the passenger_km_per_student by the number of students from each country

#### Add a new row with the total distance flown by studentsand the relevant effect factor

international_travel_clean <- tibble(
  Group = "Student Travel",
  `Activity` = "International Student Flights",
  Quantity = sum(distance_flown$annual_passenger_km_per_country, na.rm = TRUE),
  Unit = "pax.km"
) |>
  mutate(Process = "air passenger travel, international/AU U")

### Staff Commuting ----

### Process Data

staff_commute <- read_excel(
  ghg_report_path,
  sheet = "Staff Commuting",
  range = "B72:O78"
)

# Filter relevant row and pivot longer

relevant_data <- staff_commute |>
  filter(`Breakdown by type of transport` == "Total company (km/year)") |>
  select(-`Total`, -`Breakdown by type of transport`)

# length(unique(colnames(relevant_data)))

staff_commuting <- relevant_data |>
  pivot_longer(
    cols = colnames(relevant_data),
    names_to = "method",
    values_to = "distance"
  )

staff_commuting_clean <- staff_commuting |>
  rename(Activity = method, Quantity = distance) |>
  mutate(
    Group = "Staff Travel",
    Unit = "km/year",
    Process = case_when(
      Activity == "Bicycle" ~ "Bicycle",
      Activity == "Motorbike/scooter" ~ "Motorbike/scooter",
      Activity == "Hybrid: Medium Car" ~ "Hybrid: Medium Car",
      Activity == "Diesel : Medium Car" ~ "Diesel : Medium Car",
      Activity == "Petrol: Medium Car" ~ "Petrol: Medium Car",
      Activity == "Battery electric vehicle (BEV): medium car" ~ "Battery electric vehicle (BEV): medium car",
      Activity == "Company fleet car (Medium Car: unknown fuel)" ~ "Petrol: Medium Car", # Assuming petrol as default
      Activity == "Light rail and tram" ~ "Light rail and tram",
      Activity == "Bus" ~ "Bus",
      Activity == "Train" ~ "Train",
      Activity == "Ferry : Foot passenger" ~ "Ferry : Foot passenger",
      Activity == "Walk" ~ "Walk"
    )
  ) |>
  select(Group, Activity, Quantity, Unit, Process)

### University Fleet ----

### Process Data

diesel_and_petrol <- read_excel(
  ghg_report_path,
  sheet = "Transport Combustion",
  range = "B12:F15"
)

fuel_data <- diesel_and_petrol |>
  rename(Emission_Source = `Emission Source`, Quantity = `2023`, Unit = UoM) |>
  select(Emission_Source, Quantity, Unit) |>
  bind_rows(
    diesel_and_petrol |>
      rename(
        Emission_Source = `Emission Source`,
        Quantity = `2023`,
        Unit = UoM
      ) |>
      filter(
        Emission_Source %in%
          c("Diesel oil post-2004", "Petrol / Gasoline post-2004")
      ) |>
      mutate(
        Emission_Source = case_when(
          Emission_Source == "Diesel oil post-2004" ~ "Purchased Diesel (Indirect)",
          Emission_Source == "Petrol / Gasoline post-2004" ~ "Purchased Petrol (Indirect)"
        )
      ) |>
      select(Emission_Source, Quantity, Unit)
  ) |>
  mutate(
    Activity = case_when(
      Emission_Source == "Diesel oil post-2004" ~ "Purchased Diesel",
      Emission_Source == "Diesel oil post-2004 (Indirect)" ~ "Purchased Diesel (Indirect)",
      Emission_Source == "Petrol / Gasoline post-2004" ~ "Purchased Petrol",
      Emission_Source == "Petrol / Gasoline post-2004 (Indirect)" ~ "Purchased Petrol (Indirect)",
      TRUE ~ Emission_Source
    ),
    Process = case_when(
      Activity == "Purchased Diesel" ~ "Diesel oil post-2004",
      Activity == "Purchased Diesel (Indirect)" ~ "Diesel oil post-2004 (indirect emissions)",
      Activity == "Purchased Petrol" ~ "Petrol / Gasoline post-2004",
      Activity == "Purchased Petrol (Indirect)" ~ "Petrol / Gasoline post-2004 (indirect emissions)",
      Activity == "Battery electric vehicle (BEV): medium car" ~ "IMPACTS ASSUMED TO BE IMMATERIAL",
      TRUE ~ NA_character_
    )
  ) |>
  mutate(
    Group = "Business Travel (Fleet)",
    Unit = case_when(
      Unit == "KWH" ~ "kWh",
      TRUE ~ Unit
    ),
    Process = as.character(Process)
  ) |>
  select(Group, Activity, Quantity, Unit, Process)

### Staff Flights ----

### Process Data

# Extract and clean staff flights data. Unlike the carbon report, AusLCI doesn't weight emissions
# according to ticket type ie: higher pax.km factor for a first class ticket vs an economy ticket.
# That is an assummption made for consistency. An alternative approach would be use the
# differentiated GHG emission factors.
#
# For the AusLCI Processs, it is assumed the long flights are international, and the rest are domestic.

staff_flights <- read_excel(
  ghg_report_path,
  sheet = "Air Travel",
  range = "B12:E19"
)

staff_flights_clean <- staff_flights |>
  rename(Activity = `Emission source`, Quantity = `2023`, Unit = `UoM`) |>
  mutate(
    Group = "Business Travel (Flights)",
    Process = case_when(
      Activity %in%
        c(
          "Long business class flights (>3,700km)",
          "Long economy class flights (>3,700km)",
          "Long first class flights (>3,700km)",
          "Long premium economy class flights (>3,700km)"
        ) ~
        "air passenger travel, international/AU U",
      Activity %in%
        c(
          "Short business class flights (>400km, ≤3,700km)",
          "Short economy class flights (>400km, ≤3,700km)",
          "Very short flights (≤400km)"
        ) ~
        "air passenger travel, domestic/AU U"
    )
  ) |>
  select(Group, Activity, Quantity, Unit, Process)

## Procurement ----

### Clean Data ----

# Only retain necessary columns, and create a new column called "Group" that combines Level 2
# and Level 3 to enable matching

EXIOBASE_mapping <- procurement_mapping |>
  mutate(
    L23 = paste(`Level 2 (Taxonomy)`, `Level 3 (Taxonomy)`, sep = " | ")
  ) |>
  select(L23, EXIOBASE)

# Summarise Procurement Data

procurement_spend <- procurement |>
  mutate(
    L23 = paste(`Level 2 (Taxonomy)`, `Level 3 (Taxonomy)`, sep = " | ")
  ) |>
  group_by(L23) |>
  filter(Spend > 0) |>
  summarise(Total_Spend = sum(Spend))

### Add EXIOBASE classifications ----

lower_procurement_exiobase <- procurement_spend |>
  mutate(L23 = tolower(L23)) |> # Convert to lowercase
  left_join(
    EXIOBASE_mapping |>
      mutate(L23 = tolower(L23)),
    by = "L23"
  )

procurement_exiobase <- lower_procurement_exiobase |>
  # mutate(`Exiobase Spend Multiple`= Total_Spend * Allocation) |>
  group_by(EXIOBASE) |>
  summarise(`EXIOBASE Spend` = sum(Total_Spend)) |>
  filter(!is.na(`EXIOBASE Spend`)) |>
  mutate(Group = "Procurement", Unit = "AUD 2023") |>
  rename(Activity = `EXIOBASE`, Quantity = `EXIOBASE Spend`) |>
  filter(!is.na(Activity)) |>
  select(Group, Activity, Quantity, Unit)

# The bioscope classification descriptions were used to complete the mapping. While the
# same categories, these have slightly different titles. This code will change the name
# of the EXIOBASE categories in the "Activity" column to match, then it assigns a new "Process"
# column to enable matching of impacts when modelling later. It then removes the numbers and
# brackets from the strings in the `Activity` column for presentability.

procurement_clean <- procurement_exiobase |>
  left_join(exiobase_crosswalk, by = c("Activity" = "exiobase_list")) |>
  mutate(
    Activity = coalesce(fin_exiobase_list, Activity),
    Process = Activity,
    Activity = gsub(
      "[0-9\\(\\)]", # Remove numbers and brackets for presentability
      "",
      Activity
    )
  ) |>
  select(-fin_exiobase_list)

### Basic Price Adjustment ----

## Adjustment expenditure to basic prices.

procurement_clean <- procurement_clean |>
  left_join(exiobase_lookup, by = c("Process" = "ENCORE_EXIOBASE_Name")) |>
  left_join(
    basic_price_factors,
    by = c("ABS_EXIOBASE_Name" = "EXIOBASE_Code")
  ) |>
  mutate(
    Adjusted_Quantity = Quantity * basic_price_factor,
    BFD_EXIOBASE_Name = case_when(
      Process == "Other land transportation services" ~ "Other land transportation services",
      Process == "Paper and paper products" ~ "Paper and paper products",
      Process == "Petroleum Refinery" ~
        "Crude petroleum and services related to crude oil extraction, excluding surveying",
      TRUE ~ BFD_EXIOBASE_Name # Fallback to handle rows that don't match any condition
    )
  )

### Inflation Adjustment ----

## Obtain inflation information

inflation_dataframe <- retrieve_inflation_data("Australia")

## Calculate inflation adjustment

inflation_adjustment <- adjust_for_inflation(
  price = 1,
  from_date = 2023,
  to_date = 2019,
  country = "Australia",
  inflation_dataframe = inflation_dataframe
)

## Currency conversion

### Requires an API key. 100 uses per month free

# THE CODE BELOW ORIGINALLY USED AN API KEY TO RETRIEVE A CONVERSION FACTOR.
# AS THE CONVERSION FACTOR WAS FIXED AT THE TIME OF RETRIEVAL AND WILL NOT CHANGE,
# THE VALUE IS NOW HARDCODED BELOW WITH AN EXPLANATORY COMMENT.

# This code runs sets the API key to the system. UNCOMMENT the line below if needed

# This code extracts the values. UNCOMMENT the line below if needed.
# currency_adjustment <- convert_currencies(price_start = 1,
#                                           from = "AUD",
#                                           to = "EUR",
#                                           date = lubridate::ymd(20190630))

# This is the extracted value
currency_adjustment <- 0.6180943

procurement_clean <- procurement_clean |>
  mutate(
    Adjusted_Quantity = Adjusted_Quantity *
      inflation_adjustment *
      currency_adjustment,
    Unit = "EUR 2019 (Basic Price)"
  )

# Create universal currency conversation file

basic_price_exiobase <- basic_price_factors |>
  left_join(
    exiobase_lookup,
    by = c("EXIOBASE_Code" = "ABS_EXIOBASE_Name")
  ) |>
  rename(ABS_EXIOBASE_Name = EXIOBASE_Code) |>
  mutate(
    EUR_AUD_coversion = 0.6180943,
    inflation_2023_2019 = 0.8900236
  ) |>
  select(
    ABS_EXIOBASE_Name,
    BFD_EXIOBASE_Name,
    ENCORE_EXIOBASE_Name,
    basic_price_factor,
    EUR_AUD_coversion,
    inflation_2023_2019
  )

# Combine Into Single Inputs Data Frame ----

## Process Data

# Ensure all data frames have consistent column types
campus_data_clean <- campus_data_clean |>
  mutate(Activity = as.character(Activity))

international_travel_clean <- international_travel_clean |>
  mutate(Activity = as.character(Activity))

procurement_clean <- procurement_clean |>
  mutate(Activity = as.character(Activity))

staff_commuting_clean <- staff_commuting_clean |>
  mutate(Activity = as.character(Activity))

staff_flights_clean <- staff_flights_clean |>
  mutate(Activity = as.character(Activity))

student_commute_clean <- student_commute_clean |>
  mutate(Activity = as.character(Activity))

fuel_data_clean <- fuel_data |>
  mutate(Activity = as.character(Activity))

investments_data_clean <- investments_clean |>
  mutate(Activity = as.character(Activity))

# Bind rows after type consistency
input_data <- bind_rows(
  campus_data_clean,
  international_travel_clean,
  procurement_clean,
  staff_commuting_clean,
  staff_flights_clean,
  student_commute_clean,
  fuel_data_clean,
  investments_data_clean
) |>
  mutate(
    Adjusted_Quantity = ifelse(
      is.na(Adjusted_Quantity),
      Quantity,
      Adjusted_Quantity
    )
  )

# Create input file for analysis replication
input_data_general <- input_data |>
  select(Group, Activity, Quantity, Unit, Process)

# Write To File ----

write_csv(input_data, "processed_data/cleaned_input_data.csv")

write_xlsx(input_data_general, "processed_data/input_data_file.xlsx")

# FINAL ADDITIONS TO CREATE PUBLIC REPLICABLE AND TRANSFERABLE WORKFLOW

basic_prices <- write_csv('basic_prices/EXIOBASE_basic_price_factors.csv')

# Create procurement handling file
procurement_processing <- procurement_clean |>
  mutate(
    inflation_factor = 0.8900236,
    currency_conversion = 0.6180943,
    basic_price_factor = coalesce(basic_price_factor, 1)
  ) |>
  select(
    Activity,
    BFD_EXIOBASE_Name,
    inflation_factor,
    currency_conversion,
    basic_price_factor
  )
