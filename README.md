# README - Roads, Green Space, Air Pollution, and Mortality: A Study in China and the UK

The code performs modeling and visualization of the relationship between infrastructure, environmental factors, and mortality outcomes in older adults.

Note: A small simulated dataset (demo.csv) is included to test the code structure and workflow.

## System Requirements
* Operating system: macOS, Linux, or Windows
* R version: 4.2.1 (2022-06-23)
* Non-standard hardware: None

Software Dependencies

The analysis is implemented in R. The scripts rely on commonly used CRAN packages for data manipulation, survival analysis, and visualization (e.g., survival, dplyr, ggplot2).

## Installation Guide
1. Install R (version 4.2.1 or later within the 4.2.x series).
2. Download or clone this repository.
3. Open R or RStudio and set the working directory to the repository root.
   
Typical installation time: < 10 minutes on a standard desktop computer.

## Demo Dataset
The file demo_data.csv is a simulated dataset created solely for demonstration and reproducibility checks. It includes:
* Time-to-event outcome variables (survival time and event indicator)
* Environmental exposure variables (e.g., road density, air pollution, greenness)
* Individual-level covariates (e.g., functional status, cognitive impairment)
* Categorical environmental context variables (e.g., distance to coast, distance to major river)
The demo data does not reproduce the original study data or results.

Run below code to load the demo dataset:

dt_model <- read.csv("demo.csv")

## Expected output:
* Model coefficient tables
* Example figures illustrating exposure–mortality associations
  
Expected runtime: < 10 minutes on a standard desktop computer.
