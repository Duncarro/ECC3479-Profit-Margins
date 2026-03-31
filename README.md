This project explores the cyclicality of profit margins across business cycle measures.

---

### Repository Structure

ECC3479-Profit-Margins  
- data/
  - raw: Raw data downloaded from ABS  
  - clean: Cleaned and processed datasets  

- output/  
  - Final outputs (figures and results)  

- src/  
  - R scripts  

- README.md

- ECC3479-Profit-Margins.Rproj: R project file

*This project requires Rstudio to run.

---

## Script Execution Order

The scripts must be run in the following order:

1. **library.R**  
   - Installs and loads all required packages  
   - *This script is executed at the start of all other scripts by default.

2. **data_cleaning.R**  
   - Downloads data from ABS and cleans it  
   - Outputs cleaned datasets to `data/clean/`  

3. **eda.R**  
   - Generates summary statistics and visualisations  
   - Saves figures to `output/`  

4. **unconditional_cyclicality.R**  
   - Performs cyclical analysis on the cleaned data  

---

### How To Run
1. Open `ECC3479-Profit-Margins.Rproj` in RStudio  
2. Run `data_cleaning.R` (requires internet connection to download ABS data)  
   - Required packages are loaded automatically via `library.R`
3. Clear R memory after extracting cleaned datasets (optional)  
4. Run `eda.R` and other analytical scripts  

All outputs (figures and results) will be saved in the `output/` folder.

**Necessary R Packages**  
"tidyverse"  
"fpp3"  
"readabs"  
"scales"  
