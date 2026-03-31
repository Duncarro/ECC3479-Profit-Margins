This project explores the cyclicality of profit margins across business cycle measures.

ECC3479-Profit-Margins/
│── data/
│   ├── raw/        # Raw data downloaded from ABS
│   └── clean/      # Cleaned and processed datasets
│
│── docs/           # Supporting documents (e.g. notes, references)
│
│── output/         # Final outputs (figures and results)
│   ├── gdp_level.png
│   ├── gop_margin.png
│   ├── gos_margin.png
│   └── thw_level.png
│
│── src/            # All R scripts
│   ├── library.R                   # Loads and installs required packages
│   ├── data_cleaning.R            # Imports and cleans raw data
│   ├── eda.R                      # Exploratory data analysis and plots
│   └── unconditional_cyclicality.R# Cyclicality analysis
│
│── .gitignore
│── ECC3479-Profit-Margins.Rproj   # R project file
│── README.md

#Execution order:

This project requires Rstudio to run.

## Script Execution Order

The scripts must be run in the following order:

1. **library.R**  
   - Installs and loads all required packages  
   -*This script is executed at the start of all other scripts by default.

2. **data_cleaning.R**  
   - Downloads data from ABS and cleans it  
   - Outputs cleaned datasets to `data/clean/`  

3. **eda.R**  
   - Generates summary statistics and visualisations  
   - Saves figures to `output/`  

4. **unconditional_cyclicality.R**  
   - Performs cyclical analysis on the cleaned data 


**Necessary R Packages**
"tidyverse" 
"fpp3"
"readabs"
"scales"