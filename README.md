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

- docs/  
   - Contains write-ups for analysis 

   
- ECC3479-Profit-Margins.Rproj: R project file

*This project requires Rstudio to run.

---

## Script Execution Order

The scripts must be run in the following order:

1. **library.R**  
   - Installs and loads all required packages  
   - *This script is executed at the start of all other scripts by default

2. **functions.R**  
   - Runs necessary functions 

3. **data_cleaning.R**  
   - Downloads data from ABS and cleans it  
   - Outputs cleaned datasets to `data/clean/` 

4. **eda.R**  
   - Generates summary statistics and graphics
  
5. **output.R**
   - Generates visuals and exports them to  `output/`

6. **conditional_cyclicality.R**  
   - Filters and adjusts data  

7. **conditional_cyclicality_regressions.R**  
   - Runs conditional regressions on filtered data (requires `unconditional_cyclicality.R`) 
 
8. **ardl_extension.R**  
   - Runs an augmented ARDL model including a COVID dummy variable 

9. **ardl_comp.R**  
   - Compares the ARDL model against potential alternatives (requires `ardl_extension.R`) 
 

**Dont need to be executed:**


10. **bnf_aug_run.R**  
   - Runs BN filter (Credit to Kamber, Morley, & Wong (2025))

11. **bnf_fnc.R**  
   - Runs BN filter (Credit to Kamber, Morley, & Wong (2025))



---

### How To Run
1. Open `ECC3479-Profit-Margins.Rproj` in RStudio  
2. Run `data_cleaning.R` (requires internet connection to download ABS data)  
   - Required packages are loaded automatically via `library.R`
   - !Importing raw ABS data is computationally demanding. The specific files required have been stored separately in data/raw
   - This only needs to be run once, as cleaned data sets are saved into `library.R` which is sourced in all preceeding sheets
3. Clear R memory after extracting cleaned datasets (optional)  
4. Run `eda.R` and other analytical scripts  
5. Run `conditional_cyclicality.R` 
6. Run `conditional_cyclicality_regressions.R` 
7. Run `ardl_extension.R` 
8. Run `ardl_comp.R` 

9. **FOR PRIMARY ECONOMETRIC ANALYSIS:** Run `docs/primary_analysis.Rmd`
   - This document runs all above documents and compiles the relevant analysis into a html output.

10. **FOR ROBUSTNESS:** Run `docs/robustness.Rmd`
   - This document is self-contained and pulls from relevant documents listed, compiling the relevant analysis into a html output.

All outputs (figures and results) will be saved in the `output/` folder.

**Necessary R Packages**  
"tidyverse"  
"fpp3"  
"readabs"  
"scales"  
