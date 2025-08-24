# THIS DOES NOT NEED TO BE RUN BY PEOPLE SEEKING TO APPLY THIS FOOTPRINT TOOL

# Midpoint factor extraction ----
## Load Packages ----

library(tidyverse)
library(janitor)
library(readxl)

## Load Data ----
### GHG ----

# FILE REMOVED DUE TO COMMERCIAL SENSITIVITY
# ghg_report_path <- 'GHG Data/University of Melbourne CY23 CA_Internal Inventory v8.1 (August 2023) VA.01.xlsx'

### AusLCI ----

# Path to folder containing the Excel files
path_to_auslci <- "auslci"

# List all Excel files in the folder (files ending in .xlsx)
excel_files <- list.files(
  path = path_to_auslci,
  pattern = "\\.xlsx$",
  full.names = TRUE
)

# Define the cell ranges to extract from the "Impacts" sheet
target_ranges <- c("B5:E7", "B11:E13", "B16:E16", "B18:E20")

### EXIOBASE ----

# Path to folder containing the Excel files
path_to_exiobase <- "Exiobase/all_excel_files"

# List all Excel files in the folder (files ending in .xlsx)
exiobase_excel_files <- list.files(
  path = path_to_exiobase,
  pattern = "\\.xlsx$",
  full.names = TRUE
)

### Greenhouse Gas Factors ----

# REQUIRED FILE REMOVED DUE TO COMMERCIAL SENSITIVITY

# ghg_factors <- read_excel(
#   ghg_report_path,
#   sheet = "Emission Factors",
#   range = "C12:M94"
# )

# ghg_factors_clean <- ghg_factors |>
#   rename(
#     Process = `Emission Source`,
#     Impact_Factor_Input = `Emission fcator \r\nUOM`,
#     Impact_Factor = EF
#   ) |>
#   mutate(
#     Damage_Pathway = "Global warming",
#     Impact_Factor_Unit = "kg CO2 eq"
#   ) |>
#   select(
#     Process,
#     Damage_Pathway,
#     Impact_Factor_Input,
#     Impact_Factor,
#     Impact_Factor_Unit
#   )

# READ IN FOR WORKFLOW CONTINUITY
ghg_factors_clean <- read_csv('processed_data/ghg_factors.csv')

### AusLCI ----

# AusLCI lookup

# Extract from files

# Initialize an empty list to store output

all_data_list <- list()

# Loop over each Excel file

for (f in excel_files) {
  # Read the "Product Name" from sheet "Calculation setup" cell C3
  product_name <- read_excel(
    path = f,
    sheet = "Calculation setup",
    range = "C3",
    col_names = FALSE
  )[[1]]

  # Read the "Per Amount" from sheet "Calculation setup" cell C7
  per_amount <- read_excel(
    path = f,
    sheet = "Calculation setup",
    range = "C7",
    col_names = FALSE
  )[[1]]

  # For each specified range on the "Impacts" sheet, read the data
  impacts_list <- lapply(target_ranges, function(rg) {
    tmp <- read_excel(
      path = f,
      sheet = "Impacts",
      range = rg,
      col_names = FALSE
    )
    # Label columns consistently to extract them easily
    colnames(tmp) <- c("colB", "colC", "colD", "colE")
    return(tmp)
  })

  # Combine the data from the ranges on the "Impacts" sheet
  impacts_data <- do.call(rbind, impacts_list)

  # Build a data frame with the required columns
  out_df <- data.frame(
    `Process` = product_name,
    `Damage_Pathway` = impacts_data$colC,
    `Impact_Factor_Input` = per_amount,
    `Impact_Factor` = impacts_data$colE,
    `Impact_Factor_Unit` = impacts_data$colD,
    stringsAsFactors = FALSE
  )

  # Store each file's result in the list
  all_data_list[[f]] <- out_df
}

# Combine everything into a single data frame

AusLCI_impacts <- do.call(rbind, all_data_list)

# Because the waste categories in the GHG report dont neatly fit into any of the
# AusLCI processes, this section of the code creates a new one-to-many category
# called "gener waste at landfill"

selected_products <- c(
  "waste treatment, corrugated containers, at landfill",
  "waste treatment, food, at landfill",
  "waste treatment, garden and green, at landfill",
  "waste treatment, mixed paper, at landfill",
  "waste treatment, office paper, at landfill",
  "waste treatment, rubber and leather, at landfill",
  "waste treatment, textiles, at landfill",
  "waste treatment, wood and woodwaste, at landfill"
)

# Filter the data to include only the selected products

filtered_data <- AusLCI_impacts |>
  filter(Process %in% selected_products)

# Compute the mean values for the selected products

assorted_waste_row <- filtered_data |>
  group_by(Damage_Pathway, Impact_Factor_Unit) |>
  summarise(
    Process = "general waste at landfill",
    Impact_Factor_Input = "1.0 kg",
    Impact_Factor = mean(Impact_Factor, na.rm = TRUE),
    .groups = "drop"
  )

# Combine the original data with the new row

AusLCI_impacts_clean <- bind_rows(AusLCI_impacts, assorted_waste_row) |>
  mutate(
    Damage_Pathway = recode(
      Damage_Pathway,
      "Ozone formation, Terrestrial ecosystems" = "Photochemical Ozone Formation"
    )
  )

### EXIOBASE ---- ### NOTE THESE FILES ARE NOT IN THE GIT AS THEY ARE >6GB
#### Custom functions ----

# Convert Excel-style column letters to numeric indices

excel_col_to_num <- function(col_letters) {
  letters_vec <- unlist(strsplit(col_letters, split = ""))

  col_index <- 0

  for (ch in letters_vec) {
    col_val <- match(ch, LETTERS) # A=1, B=2,..., Z=26

    col_index <- col_index * 26 + col_val
  }

  return(col_index)
}

# Read disjoint column blocks (A:C and JXV:KFM) from row 2 onward
# Returns NULL if the sheet does not exist in the file

read_disjoint_blocks <- function(file, sheet_name) {
  # If sheet not present, return NULL
  if (!sheet_name %in% excel_sheets(file)) {
    return(NULL)
  }

  # Left block: columns A:C => numeric indices [1..3], from row=2 onward
  df_left <- read_excel(
    path = file,
    sheet = sheet_name,
    range = cell_limits(
      ul = c(2, 1), # row=2, col=1
      lr = c(NA, 3)
    ), # row=∞, col=3
    col_names = TRUE
  )

  # Right block: columns JXV:KFM => numeric indices [7406..7605]
  left_col_jxv <- excel_col_to_num("JXV") # 7406
  right_col_kfm <- excel_col_to_num("KFM") # 7605

  df_right <- read_excel(
    path = file,
    sheet = sheet_name,
    range = cell_limits(ul = c(2, left_col_jxv), lr = c(NA, right_col_kfm)),
    col_names = TRUE
  )

  # Combine side-by-side
  df_combined <- cbind(df_left, df_right)

  return(df_combined)
}

#### Extract Data ----

# Potential second-sheet candidates
possible_sheets <- c("KgPerEuro", "KgperEuro", "m2perEuro", "m3perEuro")

# Potential multipliers sheets
multipliers_sheets <- c("ExiobaseMultipliers", "Exiobase_Multipliers")

# Potential PDF sheet name variants
pdf_variants <- c("PDFperEuro", "PDFPerEuro", "pdfpereuro", "PDFPEREURO")

# Initialize a list to store data from each file

all_exiobase_data_list <- list()

for (f in exiobase_excel_files) {
  # (A) Identify which Multipliers sheet is present, read "Impact Driver" from B3

  sheet_names_in_file <- excel_sheets(f)
  found_multipliers_sheet <- intersect(multipliers_sheets, sheet_names_in_file)

  if (length(found_multipliers_sheet) == 1) {
    multipliers_sheet <- found_multipliers_sheet[1]

    product_name <- read_excel(
      path = f,
      sheet = multipliers_sheet,
      range = "B3",
      col_names = FALSE
    )[[1]]
  } else {
    product_name <- NA_character_
  }

  # (B) Identify which PDF variant is present, read that sheet if found

  found_pdf_sheet <- intersect(pdf_variants, sheet_names_in_file)

  if (length(found_pdf_sheet) >= 1) {
    pdf_sheet_name <- found_pdf_sheet[1]

    # 1) Read data blocks
    pdf_data <- read_disjoint_blocks(file = f, sheet_name = pdf_sheet_name)

    # Add columns: "Sheet tab" and "Specific Product"
    if (!is.null(pdf_data)) {
      pdf_data[["Sheet tab"]] <- pdf_sheet_name

      pdf_data[["Specific Product"]] <- product_name
    } else {
      pdf_data <- NULL
    }
  } else {
    pdf_data <- NULL
  }

  # (C) Identify which second sheet is present, read that sheet if found

  found_second_sheet <- intersect(possible_sheets, sheet_names_in_file)

  if (length(found_second_sheet) >= 1) {
    second_sheet_name <- found_second_sheet[1]

    second_data <- read_disjoint_blocks(
      file = f,
      sheet_name = second_sheet_name
    )

    if (!is.null(second_data)) {
      second_data[["Sheet tab"]] <- second_sheet_name

      second_data[["Specific Product"]] <- product_name
    } else {
      second_data <- NULL
    }
  } else {
    second_data <- NULL
  }

  # (D) Combine PDF data and second-sheet data for this file

  if (!is.null(pdf_data) && !is.null(second_data)) {
    combined_file_data <- rbind(pdf_data, second_data)
  } else if (!is.null(pdf_data)) {
    combined_file_data <- pdf_data
  } else if (!is.null(second_data)) {
    combined_file_data <- second_data
  } else {
    combined_file_data <- NULL
  }

  # Store if we have something

  if (!is.null(combined_file_data) && nrow(combined_file_data) > 0) {
    all_exiobase_data_list[[f]] <- combined_file_data
  }
}

# Combine all files, rename columns, reorder columns

if (length(all_exiobase_data_list) > 0) {
  all_exiobase_data <- do.call(rbind, all_exiobase_data_list)
} else {
  # No data found at all
  all_exiobase_data <- data.frame()
}

if (nrow(all_exiobase_data) > 0) {
  # Rename "Specific Product" -> "Impact Driver"
  names(all_exiobase_data)[
    names(all_exiobase_data) == "Specific Product"
  ] <- "Impact Driver"

  # Reorder columns to:
  #   1) LC-Impact Countries
  #   2) Exiobase region
  #   3) Exiobase region abbreviation
  #   4) Sheet tab
  #   5) Impact Driver
  #   6) Everything else

  desired_order <- c(
    "LC-Impact Countries",
    "Exiobase region",
    "Exiobase region abbreviation",
    "Sheet tab",
    "Impact Driver"
  )

  # Build a final column order that starts with any of the above *that exist*,
  # followed by all remaining columns.
  existing_columns <- names(all_exiobase_data)
  reorder_front <- intersect(desired_order, existing_columns)
  # Then the rest of the columns not in reorder_front
  reorder_back <- setdiff(existing_columns, reorder_front)
  final_col_order <- c(reorder_front, reorder_back)

  all_exiobase_data <- all_exiobase_data[, final_col_order, drop = FALSE]

  # Write out CSV

  write_csv(all_exiobase_data, "Exiobase/all_excel_files/all_exiobase_data.csv")
}

# Add a unit column

all_exiobase_data_clean <- all_exiobase_data |>
  rename(
    Sheet_tab = `Sheet tab`,
    LC_Impact_Countries = `LC-Impact Countries`,
    Exiobase_region = `Exiobase region`,
    Exiobase_region_abbreviation = `Exiobase region abbreviation`,
    Impact_Driver = `Impact Driver`
  ) |>
  mutate(
    Sheet_tab = case_when(
      Sheet_tab %in% c("PDFperEuro", "PDFPerEuro") ~ "PDFperEuro",
      Sheet_tab %in% c("KgPerEuro", "KgperEuro") ~ "KGperEuro",
      TRUE ~ Sheet_tab
    ),
    Unit = case_when(
      Sheet_tab == "PDFperEuro" ~ "PDF",
      Sheet_tab == "KGperEuro" ~ "KG",
      Sheet_tab == "m3perEuro" ~ "m3",
      Sheet_tab == "m2perEuro" ~ "m2",
      TRUE ~ NA_character_
    )
  ) |>
  select(
    LC_Impact_Countries,
    Exiobase_region,
    Exiobase_region_abbreviation,
    Sheet_tab,
    Impact_Driver,
    Unit,
    everything()
  )

# Ensuring all the EXIOBASE data is there

# Do I have 200 unique product columns? -> If not, it means the fixed header scrapping may
# have pooled in non-Australian specific data

length(unique(names(all_exiobase_data_clean)))

# 206. Great! There is six non-product headers suggesting the code has worked!!!

#### Extract GHG data for EXIOBASE ----

# Extract the first few columns that provide context

ghg_midpoints_exiobase_left <- read_xlsx(
  'Exiobase/all_excel_files/ClimateChange_BiodiversityFootprint.xlsx',
  sheet = "ExiobaseMultipliers",
  range = "A1:B500"
)

# Extract the columns with the useful datapoints

ghg_midpoints_exiobase_right <- read_xlsx(
  'Exiobase/all_excel_files/ClimateChange_BiodiversityFootprint.xlsx',
  sheet = "ExiobaseMultipliers",
  range = "JXS1:KFJ500"
)

# Combine the datasets

ghg_midpoint_combined <- cbind(
  ghg_midpoints_exiobase_left,
  ghg_midpoints_exiobase_right
)

# Cut the dataframe up, so that there is only useful datapoints. It retains the
# ghg emissions for the relevant categories
# This is not pretty, but gets the job done

ghg_midpoint <- ghg_midpoint_combined |>
  filter(!is.na(region) & region != "") |>
  select(-`...1`) |>
  row_to_names(row_number = 1) |>
  slice(-(1:19)) |>
  rename(Impact_Driver = sector)

# Add the columns necessary so it merges with the broader dataset

ghg_midpoint_data <- ghg_midpoint |>
  mutate(
    LC_Impact_Countries = "Global",
    Exiobase_region = "Global",
    Exiobase_region_abbreviation = "Global",
    Sheet_tab = "ExiobaseMultipliers"
  ) |> # could be changed to "KGperEuro"
  select(
    LC_Impact_Countries,
    Exiobase_region,
    Exiobase_region_abbreviation,
    Sheet_tab,
    Impact_Driver,
    everything()
  )

# Do the same for the endpoint data for ghg factors

# Load the first part of the dataframe

ghg_endpoints_exiobase_left <- read_xlsx(
  'Exiobase/all_excel_files/ClimateChange_BiodiversityFootprint.xlsx',
  sheet = "BiodiversityFootprint_Factors",
  range = "A1:B500"
)

# Load the second part of the dataframe

ghg_endpoints_exiobase_right <- read_xlsx(
  'Exiobase/all_excel_files/ClimateChange_BiodiversityFootprint.xlsx',
  sheet = "BiodiversityFootprint_Factors",
  range = "JXS1:KFJ500"
)

# Combine the two

ghg_endpoint_combined <- cbind(
  ghg_endpoints_exiobase_left,
  ghg_endpoints_exiobase_right
)

# Clean the dataframe to only retain usefulpoints

ghg_endpoint <- ghg_endpoint_combined |>
  select(-1) |>
  slice(1:3) |>
  row_to_names(row_number = 1) |>
  rename(Impact_Area = sector)

# Add the necessary columns

ghg_endpoint_data <- ghg_endpoint |>
  mutate(
    LC_Impact_Countries = "Global",
    Exiobase_region = "Global",
    Exiobase_region_abbreviation = "Global",
    Sheet_tab = "PDFperEuro",
    Impact_Driver = "Global warming"
  ) |>
  select(
    LC_Impact_Countries,
    Exiobase_region,
    Exiobase_region_abbreviation,
    Sheet_tab,
    Impact_Driver,
    everything()
  ) |>
  select(-Impact_Area)

# Add into the broader overall biodiversity dataframe

all_ghg_data <- bind_rows(ghg_endpoint_data, ghg_midpoint_data)

# Define numeric columns based on ghg_endpoint

numeric_columns <- colnames(ghg_endpoint_data)[6:ncol(ghg_endpoint_data)]

# Convert specified columns to numeric in both dataframes

convert_to_numeric <- function(df, cols) {
  df |>
    mutate(across(all_of(cols), ~ as.numeric(.)))
}

ghg_endpoint_data <- convert_to_numeric(ghg_endpoint_data, numeric_columns)

ghg_midpoint_data <- convert_to_numeric(ghg_midpoint_data, numeric_columns)

# Ensure column order matches

ghg_midpoint_data <- ghg_midpoint_data |>
  select(all_of(colnames(ghg_endpoint_data)))

# Combine the dataframes

all_ghg_data <- bind_rows(ghg_endpoint_data, ghg_midpoint_data)

# Combine all the data

all_exiobase_data <- bind_rows(
  ghg_endpoint_data,
  ghg_midpoint_data,
  all_exiobase_data_clean
)


## Write Midpoint Factors Lookups To File ----

# Make investments tibble as it is not added elsewhere
investments_clean <- tibble(
  Process = "Investments",
  Damage_Pathway = "Global warming",
  Impact_Factor_Input = "kg CO2-e/kg CO2-e",
  Impact_Factor = 1,
  Impact_Factor_Unit = "kg CO2 eq"
)

## Combine AusLCI and GHG ----

midpoint_factors <- bind_rows(
  AusLCI_impacts_clean,
  ghg_factors_clean,
  investments_clean
)

## Write to file ----

write_csv(midpoint_factors, "processed_data/non_procurement_midpoints.csv")

write_csv(all_exiobase_data, "processed_data/procurement_midpoints.csv")

# LC-Impact endpoint characterisation factor extraction ----
## Load Data ----

### Greenhouse Gases ----

ghg_values <- read_excel(
  'lc_impact/LC-Impact/2-climate change/Climate change CFs.xlsx',
  sheet = " Characterization factors",
  range = "A1:Z600",
  col_names = FALSE
) # Prevents reading G3 and K3 as column names

### Photochemical ozone formation ----

pco <- read_excel(
  'lc_impact/LC-Impact/5-photochemical ozone formation/Photochemical_Ozone_formation.xlsx',
  sheet = "CFs per country",
  range = "A1:E100",
  col_names = FALSE
)

### Terrestrial acidification ----

t_acid <- read_excel(
  'lc_impact/LC-Impact/7-terrestrial acidification/CF_terrestrial_acidification.xlsx',
  sheet = "CF per continent and global",
  range = "A1:D100",
  col_names = TRUE
)

### Freshwater eutrophication ----

f_eut <- read_excel(
  'lc_impact/LC-Impact/8-freshwater eutrophication/CF_FWEutrophication.xlsx',
  sheet = "Country CFs",
  range = "A1:D100"
)

### Marine eutrophication ----

m_eut <- read_excel(
  'lc_impact/LC-Impact/9-marine eutrophication/CFs_marine_eutrophication.xlsx',
  sheet = "country CFs",
  range = "A1:F242"
)

## Toxicity ----

toxicity <- read_excel(
  'lc_impact/LC-Impact/10-toxicity/USEtox2.1_LC-Impact_v2_results_ECOTOX_20190326.xlsx',
  sheet = "all impacts, long-term",
  range = "A1:AA372"
)

### Land Use ----

land_use <- read_excel(
  'lc_impact/LC-Impact/11-Land stress/CFs_land_Use_average.xlsx',
  sheet = "transf. avg country 100y",
  range = "A1:S200"
)

### Water Use ----

# Old sheet tab extraction
# water_use <- read_excel('lc_impact/LC-Impact/12-water consumption/CFs_water_consumption_ecosystems_20180831.xlsx',
#                         sheet = "CF per countries",
#                         range = "A1:C200")

water_use <- read_excel(
  'lc_impact/LC-Impact/12-water consumption/CFs_water_consumption_ecosystems_20180831.xlsx',
  sheet = "CF per continent",
  range = "A1:C200"
)

## Create Impact Factor Tables ----

### Greenhouse Gases ----

lc_ghg_clean <- tibble(
  Impact = "Global warming",
  Impact_Region = "Global",
  Ecosystem_Impacted = c("Terrestrial ecosystems", "Freshwater Ecosystems"),
  Impact_Pathway = "",
  Input_Unit = "kg CO2 eq",
  Endpoint_Factor = as.numeric(c(ghg_values[[3, 7]], ghg_values[[3, 11]])), # Correctly accessing G3 and K3
  Endpoint_Unit = "PDF*y/kg"
)

### Photochemical ozone formation ----

lc_pco_clean <- tibble(
  Impact = "Photochemical Ozone Formation",
  Impact_Region = "Australia",
  Ecosystem_Impacted = "Terrestrial ecosystems",
  Impact_Pathway = "",
  Input_Unit = "kg NOx eq",
  Endpoint_Factor = as.numeric(pco[[10, 5]]), # takes the values for Australia. Might be best to take the global values instead. Bulk of impacts should be on Australia though.
  Endpoint_Unit = "PDF.yr/kg"
)

### Terrestrial acidification ----

lc_t_acid_clean <- tibble(
  Impact = "Terrestrial acidification",
  Impact_Region = "Australia",
  Ecosystem_Impacted = "Terrestrial ecosystems",
  Impact_Pathway = "",
  Input_Unit = c("kg SO2 eq"), # Only took Sox -> saved as SO2 for consistency with ReCiPe
  Endpoint_Factor = as.numeric(c(t_acid[[7, 4]])),
  Endpoint_Unit = "PDF.yr/kg"
)

### Freshwater eutrophication ----

f_eut_clean <- tibble(
  Impact = "Freshwater eutrophication",
  Impact_Region = "Australia",
  Ecosystem_Impacted = "Freshwater ecosystems",
  Impact_Pathway = c("P emissions to water", "P emissions to soil"),
  Input_Unit = "kg P eq",
  Endpoint_Factor = as.numeric(c(f_eut[[8, 2]], f_eut[[8, 3]])),
  Endpoint_Unit = "PDF.yr/kg"
)

### Marine eutrophication ----

m_eut_clean <- tibble(
  Impact = "Marine eutrophication",
  Impact_Region = "Australia",
  Ecosystem_Impacted = "Marine ecosystems",
  Impact_Pathway = c(
    "N emission to soil",
    "N emission to freshwater (river)",
    "N emission to marine system"
  ),
  Input_Unit = "kg N eq",
  Endpoint_Factor = as.numeric(c(m_eut[[6, 2]], m_eut[[6, 3]], m_eut[[13, 6]])),
  Endpoint_Unit = "PDF.yr/kg"
)

### Toxicity (Fresh Water) ----

# There is low confidence in these values
freshwater_toxicity_values <- toxicity |>
  filter(Name == "1,4-dichlorobenzene") |>
  select(
    `Endpoint Ecotox. Charact. factor [PDF.m3.d/kgemitted]`,
    `...5`,
    `...6`,
    `...7`,
    `...8`,
    `...9`,
    `...10`,
    `...11`
  ) |>
  unlist(use.names = FALSE) # Convert to a vector

toxicity_freshwater_clean <- tibble(
  Impact = "Freshwater ecotoxicity",
  Impact_Region = "Global",
  Ecosystem_Impacted = "Freshwater ecosystems",
  Impact_Pathway = c(
    "Em.hom.airI",
    "Em.ind.airI",
    "Em.airU",
    "Em.airC",
    "Em.fr.waterC",
    "Em.sea.waterC",
    "Em.nat.soilC",
    "Em.agr.soilC"
  ),
  Input_Unit = "kg 1,4-DCB",
  Endpoint_Factor = as.numeric(freshwater_toxicity_values), # Insert as a single column
  Endpoint_Unit = "PDF.m3.day/kg"
)

### Toxicity (Marine) ----

# There is low confidence in these values
marine_toxicity_values <- toxicity |>
  filter(Name == "1,4-dichlorobenzene") |>
  select(
    `...12`,
    `...13`,
    `...14`,
    `...15`,
    `...16`,
    `...17`,
    `...18`,
    `...19`
  ) |>
  unlist(use.names = FALSE) # Convert to a vector

# There is low confidence in these values
toxicity_marine_clean <- tibble(
  Impact = "Marine ecotoxicity",
  Impact_Region = "Global",
  Ecosystem_Impacted = "Marine ecosystems",
  Impact_Pathway = c(
    "Em.hom.airI",
    "Em.ind.airI",
    "Em.airU",
    "Em.airC",
    "Em.fr.waterC",
    "Em.sea waterC",
    "Em.nat.soilC",
    "Em.agr.soilC"
  ),
  Input_Unit = "kg 1,4-DCB",
  Endpoint_Factor = as.numeric(marine_toxicity_values),
  Endpoint_Unit = "PDF.m3.day/kg"
)

### Toxicity (Terrestrial) ----

# There is low confidence in these values
terrestrial_toxicity_values <- toxicity |>
  filter(Name == "1,4-dichlorobenzene") |>
  select(
    `...20`,
    `...21`,
    `...22`,
    `...23`,
    `...24`,
    `...25`,
    `...26`,
    `...27`
  ) |>
  unlist(use.names = FALSE) # Convert to a vector

# There is low confidence in these values
toxicity_terrestrial_clean <- tibble(
  Impact = "Terrestrial ecotoxicity",
  Impact_Region = "Global",
  Ecosystem_Impacted = "Terrestrial ecosystems",
  Impact_Pathway = c(
    "Em.hom.airI",
    "Em.ind.airI",
    "Em.airU",
    "Em.airC",
    "Em.fr.waterC",
    "Em.sea.waterC",
    "Em.nat.soilC",
    "Em.agr.soilC"
  ),
  Input_Unit = "kg 1,4-DCB",
  Endpoint_Factor = as.numeric(terrestrial_toxicity_values),
  Endpoint_Unit = "PDF.m3.day/kg"
)

### Land Use ----

land_use_clean <- tibble(
  Impact = "Land use",
  Impact_Region = "Australia",
  Ecosystem_Impacted = "Terrestrial ecosystems",
  Impact_Pathway = "",
  Input_Unit = "m2a crop eq",
  Endpoint_Factor = as.numeric(land_use[[15, 2]]), # takes the median for annual crops given that is our midpoint
  Endpoint_Unit = "PDF.yr/m2"
)

### Water Use ----

# Takes the value for Australia
# water_use_clean <- tibble(
#   Impact = "Water consumption",
#   Impact_Region = "Australia",
#   Ecosystem_Impacted = "Freshwater ecosystems",
#   Impact_Pathway = "",
#   Input_Unit = "Water consumption",
#   Endpoint_Factor = as.numeric(water_use[[9,3]]),
#   Endpoint_Unit = "PDF.yr/m3"
# )

# Takes the value for Australia
water_use_clean <- tibble(
  Impact = "Water consumption",
  Impact_Region = "Australia",
  Ecosystem_Impacted = "Freshwater ecosystems",
  Impact_Pathway = "",
  Input_Unit = "Water consumption",
  Endpoint_Factor = as.numeric(water_use[[9, 3]]),
  Endpoint_Unit = "PDF.yr/m3"
)

## Create LC-Impact Factors Lookup ----

# Combine objects

LC_Impact_Factors <- bind_rows(
  lc_ghg_clean,
  lc_pco_clean,
  lc_t_acid_clean,
  f_eut_clean,
  m_eut_clean,
  toxicity_freshwater_clean,
  toxicity_marine_clean,
  toxicity_terrestrial_clean,
  land_use_clean,
  water_use_clean
)

## Write To File ----

write_csv(LC_Impact_Factors, "processed_data/LC_impact_factors.csv")
