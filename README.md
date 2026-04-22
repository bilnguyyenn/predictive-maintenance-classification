# Predictive Maintenance & Failure Classification 

## Objective
This project analyzes the AI4I 2020 Predictive Maintenance Dataset to identify the operational drivers of manufacturing equipment failure. By analyzing sensor metrics like torque, rotational speed, and temperature, we built predictive models to classify machine failures before they occur.

## Methodology
* **Language:** R
* **Libraries:** `tidyverse`, `ggplot2`, `caret`, `class`, `pROC`
* **Statistical Inference:** Welch's Two-Sample t-test
* **Models Deployed:** Logistic Regression, K-Nearest Neighbors (KNN)

## Repository Contents
* `ai4i2020.csv`: The raw dataset containing 10,000 operational records.
* `predictive_maintenance.R`: The complete R script for data preprocessing, statistical testing, and machine learning models.
* `ISyE3030_Project.pdf`: The final analytical report detailing our methodology, model adequacy, and conclusions.
* `*.png`: Various exported data visualizations (Correlation Heatmaps, ROC Curves, Boxplots).

## Key Results
Our final K-Nearest Neighbors model achieved an accuracy of **96.75%** on the testing data, outperforming the baseline Logistic Regression model in sensitivity and balanced accuracy.
