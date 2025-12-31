# Environmental Epidemiology Project
**Cohort Analysis: Association Between Environmental Noise and Hypertension in Rome**

**Author:** Saud Hamad  
**Institution:** Imperial College London  
**Course:** Environmental Epidemiology  
**Date:** December 31, 2025

##  Project Overview
This project investigates the association between long-term exposure to environmental noise (road traffic and aircraft) and the incidence of hypertension in a longitudinal cohort of adults residing in Rome, Italy.

Using data from **1,539 participants** followed over a 10-year period, the analysis aims to provide high-quality evidence for noise control policies regarding hypertension and cardiovascular outcomes. 

##  Research Objectives
The analysis is structured around three primary objectives:

* **Objective 1 (Exploratory Data Analysis):** To characterize the study population, explore the distribution of noise exposures (Night-Air, Day-Air, Road), and assess collinearity among predictors.
* **Objective 2 (Survival Estimates):** To estimate the unadjusted probability of "Hypertension-free survival" over the 10-year follow-up period using Kaplan-Meier curves.
* **Objective 3 (Risk Quantification):** To quantify the hazard ratios (HR) associated with each 10 dB(A) increase in noise exposure using Cox Proportional Hazards models, adjusting for confounders.

##  Data Dictionary
The dataset consists of 1,539 observations with the following key variables:

| Variable | Description |
|:---|:---|
| `ht_incid` | Hypertension Incidence (1 = Yes, 0 = No) |
| `time_ht` | Time to hypertension event or censoring (months) |
| `age` | Age of the participant (years) |
| `gender` | Participant gender (Male/Female) |
| `bmi_bs` | Body Mass Index ($kg/m^2$) |
| `smoke_bs` | Smoking status (Smoker/Non-smoker) |
| `alcohol_bs` | Alcohol consumption (units/week) |
| `saltconsumption_bs` | Salt intake habits (Always vs. Sometimes/Seldom/Never) |
| `education_bs` | Education level (categorized by years) |
| `exercise` | Frequency of physical activity |
| `open_windows_bs` | Habitual window opening (Yes/No) |
| `noise_night_air_bs` | Night-time aircraft noise exposure (23:00-07:00) in dB(A) |
| `noise_day_air_bs` | Day-time aircraft noise exposure (07:00-23:00) in dB(A) |
| `noise_allday_road_bs` | 24-hour mean road traffic noise exposure in dB(A) |

##  Methodology
The statistical analysis was conducted using **R (v4.3.1)**. The workflow includes:

1.  **Data Cleaning:** Complete case analysis (excluding 3.2% missing data) and factor recoding.
2.  **Descriptive Stats:** Table 1 generation using the `tableone` package.
3.  **Survival Analysis:**
    * **Kaplan-Meier:** Visualizing crude survival probabilities[cite: 106].
    * **Cox Proportional Hazards:** Modeling the hazard ratio for each noise exposure separately due to high collinearity between night and day aircraft noise ($r=0.70$).
4.  **Diagnostics:** Testing the proportional hazards assumption using Schoenfeld residuals.

##  Key Findings
* **Road Traffic Noise:** A statistically significant association was found. [cite_start]Each **10 dB(A) increase** in day-time road traffic noise was associated with a **9% increased risk** of developing hypertension (HR 1.09, 95% CI 1.01–1.17; $p=0.02$).
* **Aircraft Noise:** Neither night-time (HR 1.02) nor day-time (HR 1.09) aircraft noise showed a statistically significant association with hypertension in this cohort.
* **BMI:** Body Mass Index was a strong independent predictor, with each additional unit increasing hazard by approximately 2%.

##  Dependencies
To replicate this analysis, ensure the following R packages are installed:

```r
install.packages(c("tidyverse", "survival", "survminer", "broom", "readr", "car", "tableone", "corrplot"))

** Data used are for practice purposes and it is not real **
