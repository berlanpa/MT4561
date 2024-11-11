# Health Data Analysis Repository

This repository contains a comprehensive analysis of health data, focusing on various statistical techniques, data cleaning, and visualization methods. The primary objective is to understand the relationships between health indicators across different U.S. cities and their socioeconomic contexts.

## Features

- **Data Cleaning and Preprocessing**: The code includes multiple steps to clean the raw dataset, handle missing values, and standardize data.
- **Exploratory Data Analysis (EDA)**: Various visualizations like histograms, density plots, and correlation matrices are created to explore the distribution and relationships in the data.
- **Principal Component Analysis (PCA)**: PCA is used to reduce the dimensionality of the data, and visualizations are generated to interpret the principal components.
- **Clustering and Correlation Analysis**: Hierarchical clustering and correlation analysis are applied to explore the relationships between health indicators.
- **Statistical Modeling**: Stepwise regression is performed to identify significant predictors for health outcomes. Random Forest models are used to assess feature importance.
- **Outlier Detection**: The analysis includes detecting and handling outliers using studentized residuals for each regression model.
- **Evaluation Metrics**: The models are evaluated using RMSE, MAE, and Adjusted R-squared, with results saved for further use.
- **Final Data Export**: The cleaned and processed data is saved as a CSV, ready for further analysis or reporting.

## File Descriptions

- **`README.md`**: This file, which provides an overview of the repository, including its features, file descriptions, and instructions for use.
  
- **`airqualitytrendsbycity2000-2023.csv`**: Contains air quality data trends for cities in the U.S. from 2000 to 2023, focusing on pollutants like PM2.5. This file is used for analysis related to environmental factors.

- **`big-cities-health-data.csv`**: The main dataset used for the health data analysis, containing health indicators across various U.S. cities. It includes information on mortality rates, disease diagnoses, and other health-related metrics.

- **`city_data.csv`**: Contains additional data for each city, such as demographic and geographic information. This file is used to augment the analysis with more context.

- **`cook_pvi_2013.csv`**: Contains the Cook Partisan Voting Index (CPVI) data for U.S. congressional districts in 2013. This dataset is used for analyzing the political context of health data.

- **`data_pivot_numeric.csv`**: A cleaned and transformed version of the main health dataset, where health indicators are pivoted into columns, making it easier for analysis and modeling.

- **`final.R`**: The primary R script that contains the code for data analysis, including data cleaning, statistical modeling, PCA, and visualization. This script is used to process the datasets and generate results.

- **`us-ansi-codes.csv`**: Contains ANSI (American National Standards Institute) codes for U.S. states and counties, used for geocoding and location-based analysis.

## Requirements

The following R packages are required to run the scripts in this repository:

- `tidyverse`  
- `httr`  
- `jsonlite`  
- `plotly`  
- `stringdist`  
- `randomForest`  

## Usage

1. Clone the repository:
   ```bash
   git clone https://github.com/your-username/health-data-analysis.git
