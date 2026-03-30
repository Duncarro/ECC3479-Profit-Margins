This project explores the cyclicality of profit margins across business cycle measures.


#Repository Structure:
data/ 
    raw/ - contains raw series from ABS
    clean/ - contains margin variable and business cycle measures
scripts/ - contains code to run project
output/  - contains visual outputs (graphs, tables,etc.)



#Execution order:

This project has been coded using Rstudio.

1. Run scripts/library.R
This will: 
*   Install all required packages
**  This package is tethered at the top of all other scripts, ensuring all necesarry libraries are installed.

2. Run scripts/data_cleaning.R 
This will:
*   Download raw ABS data
*   Clean datasets
*   Output to data/clean/