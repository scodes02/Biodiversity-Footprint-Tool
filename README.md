# Biodiversity Footprint Assessment Workflow

## Overview

This repository implements an end-to-end biodiversity footprint assessment for organisations. Although optimised for universities operating in Victoria, Australia, the core procedures are applicable at the national level and can be to any organisation within Australia. The workflow quantifies both midpoint and endpoint biodiversity impacts associated with organisational activities and provides geographic visualisations of procurement-related impacts.

## Repository structure
```text
.
├── 01_biodiversity_footprint_tool.R   # Core biodiversity footprint calculation
├── 02_results_mapper.R                # Geographic visualisation of procurement impacts
├── impact_data_cleaner.R              # [Reference] Extracts LC-Impact, EXIOBASE & AusLCI factors
│                                      # (EXIOBASE raw files removed due to >6GB size)
├── input_data_cleaner.R               # [Reference] Prepares input_file.xlsx for UniMelb procurement
├── input_file.xlsx                    # Template for organisational activity data
│
├── processed_data/                    # Cleaned and processed impact factors
├── results/                           # Generated Excel and spatial outputs
├── auslci/                            # Original AusLCI data files
└── currency_handling/                 # ABS & other sources for inflation/currency factors
```

## Primary scripts

- `01_biodiversity_footprint_tool.R`  
  - Calculates midpoint impacts (e.g. CO₂, water, land use) for non-procurement and procurement activities.  
  - Converts midpoints to biodiversity endpoints (Potentially Disappeared Fraction, PDF).  
  - Exports an Excel workbook with separate sheets for non-procurement midpoints, procurement midpoints, and a summary of total endpoint impacts by activity.

- `02_results_mapper.R`  
  - Reads the procurement endpoint dataset.  
  - Generates:  
    1. A global PDF distribution map  
    2. One map per procurement activity (~192)
    3. One map per impact driver (~24)
  - Outputs PDF files in `results/impact_maps/`

## Reference Scripts

- `impact_data_cleaner.R`  
  Demonstrates how raw LC-Impact, AusLCI and GHG characterisation factors were extracted, transformed and combined into midpoint and endpoint factor tables.

- `input_data_cleaner.R`  
  Illustrates preparation of organisational activity data at the University of Melbourne (e.g. campus operations, travel, procurement mapping) for use in `input_file.xlsx`.

## Prerequisites

- R ≥ 4.0.0  
- Required R packages:  
  - tidyverse  
  - readxl  
  - writexl  
  - rnaturalearth  
  - terra  
  - tidyterra  

Scripts will install any missing packages automatically.

## Data preparation

1. Complete the `input_file.xlsx` with activity data specific to your organisation.  
2. Include only relevant activities. Where both non-procurement and procurement data are available for the same activity, prioritise the non-procurement version. For instance, the University of Melbourne retained approximately 50 of the 192 procurement categories derived from the Biodiversity Footprint Database (which is based on Exiobase products/industries). Additionally, if direct activity data is available—such as flight distances—it should be used in preference to expenditure-based procurement records for the same activity.  
3. Save the completed file in the repository root directory.

## Execution instructions

1. Download the repository
2. Run `01_biodiversity_footprint_tool.R` to compute impacts and generate `results/biodiversity_impact_workbook.xlsx`.  
3. Run `02_results_mapper.R` to produce spatial maps in `results/impact_maps/`.

## Outputs

- **Excel workbook** (`results/biodiversity_impact_workbook.xlsx`)  
  - Non-procurement midpoints  
  - Procurement midpoints  
  - Endpoints_Summary_By_Activity  

- **Spatial maps** (`results/impact_maps/`)  
  - Overall PDF distribution  
  - PDF per activity  
  - PDF per impact driver

## Interpreting results

- **Midpoints**: Measurable environmental pressures (e.g., CO₂-eq, m³ water, m² land).  
- **Endpoints**: Biodiversity impact (PDF), calculated as:  

Endpoint = ∑(Midpoint × Characterisation Factor)

Note: LC-Impact’s PDF metrics are best interpreted as relative indicators of biodiversity pressure rather than precise extinction risk estimates. We are highly skeptical about the quality of the LC-Impact factors (and all LCIA methods), and believe the PDF metric it produces is incorrect when trying understand an organisations true impacts on biodiversity. 

## References

- The Biodiversity Footprint Database (EXIOBASE data utilised. Early access provided by El Geneidy, S): El Geneidy, S., Baumeister, S., Peura, M., & Kotiaho, J. S. (2023). The Biodiversity Footprint Database (Version v1) [Data set]. Zenodo. https://doi.org/10.5281/zenodo.8369650  
- AusLCI LCA Database used: https://www.lifecycles.com.au  
- The original AusLCI datasets from ALCAS: https://www.alcas.asn.au
