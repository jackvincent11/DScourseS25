# Liverpool F.C. Performance Analysis

## Project Overview
This project analyzes Liverpool Football Club's performance across the 2023-24 and 2024-25 Premier League seasons, comparing results under Jürgen Klopp and Arne Slot.

## Research Question
What performance metrics most significantly influenced Liverpool's championship victory under Arne Slot following Jürgen Klopp's departure?

## Files in this Repository
- `DS_Final_Project.R`: R script with data collection, analysis, and visualization code
- `liverpool_regression_data_complete.csv`: Processed dataset with match statistics
- `WrittenReport_Vincent.pdf`: Full academic report with methodology and findings
- `WrittenReport_Vincent.tex`: Latex code used to generate pdf file

## Required R Packages
- worldfootballR (install with: devtools::install_github("JaseZiv/worldfootballR"))
- dplyr
- lubridate
- ggplot2
- car
- gridExtra
- effects

## How to Reproduce the Analysis
1. Install the required packages
2. Run the R script which will:
   - Collect match data from fbref.com
   - Process and combine data across seasons
   - Perform regression analysis
   - Generate visualizations

## Key Findings
- Shot conversion rate had the strongest effect on points earned
- The relationship between conversion efficiency and points weakened under Slot's management
- Home advantage remained consistently important across both managerial approaches
