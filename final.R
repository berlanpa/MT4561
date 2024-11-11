# Load necessary libraries
library(tidyverse)      # Data manipulation and visualization
library(httr)           # HTTP requests
library(jsonlite)       # JSON parsing
library(plotly)         # Interactive plots
library(stringdist)     # String distance metrics
library(broom)          # Tidying model outputs

# 1. Read the dataset
# Input: Dataset "big-cities-health-data.csv" in the working directory
# Output: Dataframe containing the health data
data <- read_csv("big-cities-health-data.csv")

# 2. Overview of Dataset Structure
# Display the first few rows and check data structure
# Input: Dataframe `data`
# Output: Glimpse of the dataset structure
glimpse(data)

# Summary statistics to understand data types and distributions
# Input: Dataframe `data`
# Output: Summary statistics of the dataframe
summary(data)

# 3. Distribution Analysis for Numerical Variable "Value"
# Plot histogram for the distribution of health indicator values
# Input: Column `Value` from `data`, non-NA values
# Output: Histogram showing distribution of values
data %>%
  filter(!is.na(Value)) %>%
  ggplot(aes(x = Value)) +
  geom_histogram(bins = 30, fill = "skyblue", color = "black") +
  labs(title = "Distribution of Health Indicator Values", x = "Value", y = "Frequency") +
  xlim(0, 100) +
  theme_minimal()

# Density plot as an alternative view of the distribution
# Input: Column `Value` from `data`, non-NA values
# Output: Density plot showing distribution of values
data %>%
  filter(!is.na(Value)) %>%
  ggplot(aes(x = Value)) +
  geom_density(fill = "coral", alpha = 0.7) +
  labs(title = "Density of Health Indicator Values", x = "Value", y = "Density") +
  xlim(0, 100) +
  theme_minimal()

# 4. Missing Values Analysis
# Count missing values for each column
# Input: Dataframe `data`
# Output: Number of missing values for each column
missing_counts <- data %>%
  summarise(across(everything(), ~ sum(is.na(.))))

print("Count of missing values per column:")
print(missing_counts)

# Visualize missing values
# Input: Dataframe `data`
# Output: Bar chart showing missing values per column
data %>%
  summarise(across(everything(), ~ sum(is.na(.)))) %>%
  pivot_longer(cols = everything(), names_to = "Variable", values_to = "Missing_Count") %>%
  ggplot(aes(x = reorder(Variable, -Missing_Count), y = Missing_Count, fill = Missing_Count)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  labs(title = "Missing Values per Variable", x = "Variable", y = "Count of Missing Values") +
  scale_fill_viridis_c(option = "D") +
  theme_minimal()

# Remove rows where Value or Year is missing
# Input: Dataframe `data`
# Output: Cleaned dataframe with missing values removed
data <- data %>%
  filter(!is.na(Value) & !is.na(Year))

# 5. Basic Statistical Analysis of the "Value" Column
# Compute summary statistics (mean, median, standard deviation, min, max) for the "Value" column
# Input: Column `Value` from `data`, non-NA values
# Output: Summary statistics for "Value"
data %>%
  filter(!is.na(Value)) %>%
  summarise(
    mean_value = mean(Value, na.rm = TRUE),
    median_value = median(Value, na.rm = TRUE),
    sd_value = sd(Value, na.rm = TRUE),
    min_value = min(Value, na.rm = TRUE),
    max_value = max(Value, na.rm = TRUE)
  )

# 6. Initial Analysis of Key Categorical Variables
# Count the records per Indicator Category
# Input: Column `Indicator Category` from `data`
# Output: Bar plot showing record counts by Indicator Category
data %>%
  count(`Indicator Category`) %>%
  ggplot(aes(x = reorder(`Indicator Category`, n), y = n, fill = n)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  labs(title = "Count of Records per Indicator Category", x = "Indicator Category", y = "Count") +
  scale_fill_viridis_c(option = "D") +
  theme_minimal()

# Count the records per Gender
# Input: Column `Gender` from `data`
# Output: Bar plot showing record counts by Gender
data %>%
  count(Gender) %>%
  ggplot(aes(x = reorder(Gender, n), y = n, fill = Gender)) +
  geom_bar(stat = "identity") +
  labs(title = "Count of Records by Gender", x = "Gender", y = "Count") +
  theme_minimal()

# Count the records per Race/Ethnicity
# Input: Column `Race/ Ethnicity` from `data`
# Output: Bar plot showing record counts by Race/Ethnicity
data %>%
  count(`Race/ Ethnicity`) %>%
  ggplot(aes(x = reorder(`Race/ Ethnicity`, n), y = n, fill = `Race/ Ethnicity`)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  labs(title = "Count of Records by Race/Ethnicity", x = "Race/Ethnicity", y = "Count") +
  theme_minimal()

# Line plot of "AIDS Diagnoses Rate" over time
# Input: Filtered `data` with `Indicator` == "AIDS Diagnoses Rate"
# Output: Line plot of AIDS diagnoses rate over years
data %>%
  filter(Indicator == "AIDS Diagnoses Rate (Per 100,000 people)") %>%
  group_by(Year) %>%
  summarise(Average_Value = mean(Value, na.rm = TRUE)) %>%
  ggplot(aes(x = Year, y = Average_Value)) +
  geom_smooth(method = "lm", color = "skyblue") +
  geom_point(color = "coral", size = 2) +
  labs(title = "Yearly Trend of AIDS Diagnoses Rate (Per 100,000 people)", x = "Year", y = "Average Value") +
  theme_minimal()

# Box plot of "AIDS Diagnoses Rate" by Place
# Input: Filtered `data` with `Indicator` == "AIDS Diagnoses Rate"
# Output: Box plot showing AIDS diagnoses rate by Place
data %>%
  filter(Indicator == "AIDS Diagnoses Rate (Per 100,000 people)") %>%
  ggplot(aes(x = reorder(Place, Value, FUN = median), y = Value, fill = Place)) +
  geom_boxplot() +
  coord_flip() +
  labs(title = "AIDS Diagnoses Rate by Place", x = "Place", y = "AIDS Diagnoses Rate (Per 100,000)") +
  theme_minimal()

# Get unique values of key categorical columns for exploration
# Input: Dataframe `data`
# Output: Unique values for each key categorical column
unique(data$Place)
unique(data$`Indicator Category`)
unique(data$Gender)
unique(data$`Race/ Ethnicity`)
unique(data$Indicator)
unique(data$Year)

# Filter the data to exclude indicators with non-comparable data and clean Year column
# Input: Dataframe `data`
# Output: Cleaned `data` excluding certain indicators
data <- data %>%
  filter(!grepl("\\*These data should not be compared", Indicator) & !grepl("\\*Comparisons of these data are difficult", Indicator))

# 7. Pivot data to create a numeric format for analysis
# Filter data for "Both" Gender and "All" Race/Ethnicity
# Input: Dataframe `data`
# Output: Pivoted dataframe `data_pivot` with health indicators as columns
data_pivot <- data %>%
  filter(Gender == "Both") %>%
  filter(`Race/ Ethnicity` == "All") %>%
  filter(is.na(`Year`) == FALSE) %>%
  select(Place, Year, Indicator, Value) %>%
  distinct(Place, Year, Indicator, .keep_all = TRUE) %>%
  pivot_wider(names_from = Indicator, values_from = Value) %>%
  arrange(Place, Year)

# 8. Calculate NA ratios and identify years with high missing data
# Input: Pivoted `data_pivot`
# Output: Dataframe showing NA ratios for each indicator by year
data_pivot %>%
  group_by(Year) %>%
  summarise(across(everything(), ~ sum(is.na(.)) / n())) %>%
  pivot_longer(cols = everything(), names_to = "Indicator", values_to = "NA_Ratio") %>%
  filter(NA_Ratio != 1) %>%
  print(n = Inf, width = Inf)

# 9. Remove columns with high missing value ratios and select specific year
# Input: Pivoted `data_pivot`, high NA ratio indicators
# Output: Cleaned `data_pivot` with selected columns
data_pivot <- data_pivot %>%
  select(-c(as.character(bad_indicators$Indicator))) %>%
  filter(Year == 2012) %>%
  select(-c(Year))

# Save cleaned data to a new CSV
# Input: Cleaned `data_pivot`
# Output: Saved CSV file "cleaned_data.csv"
write_csv(data_pivot, "cleaned_data.csv")

# 10. Visualize standardized data for each indicator colored by place
# Input: Cleaned `data_pivot`
# Output: Scatter plot showing standardized values for each indicator, colored by place
data_pivot %>%
  pivot_longer(cols = -c(Place), names_to = "Indicator", values_to = "Value") %>%
  mutate(Indicator = str_replace(Indicator, "\\s*\\(.*?\\)", "")) %>%
  mutate(Place = str_replace(Place, "\\s*\\(.*?\\)", "")) %>%
  ggplot(aes(x = Place, y = Value, color = Place)) +
  geom_point() +
  facet_wrap(~Indicator, scales = "free_y") +
  theme_minimal()

# 11. Get coordinates for each city using a geolocation API
# Function to fetch latitude and longitude for a given city
# Input: City name
# Output: Data frame with the city name, latitude, and longitude
get_coordinates <- function(city) {
  url <- paste0("https://nominatim.openstreetmap.org/search?q=", URLencode(city), "&format=json&limit=1")
  res <- GET(url)
  if (http_status(res)$category == "Success") {
    data <- fromJSON(content(res, as = "text"), flatten = TRUE)
    if (length(data) > 0) {
      return(data.frame(City = city, Latitude = as.numeric(data$lat), Longitude = as.numeric(data$lon)))
    }
  }
  return(data.frame(City = city, Latitude = NA, Longitude = NA))
}

# Get coordinates for each city in `data_pivot`
# Input: Dataframe `data_pivot` with places
# Output: Dataframe `coordinates` with city names, latitudes, and longitudes
coordinates <- map_dfr(data_pivot$Place, get_coordinates)

# 12. Perform Hierarchical Clustering on Correlation Matrix
# Calculate the correlation matrix
# Input: Dataframe `data_pivot_numeric` (numeric-only data)
# Output: Correlation matrix (`cor_matrix`)
cor_matrix <- cor(data_pivot_numeric, use = "complete.obs")

# Perform hierarchical clustering on the correlation matrix
# Input: `cor_matrix` (correlation matrix)
# Output: Hierarchical clustering results (`cluster`)
dist_matrix <- as.dist(1 - cor_matrix)  # Convert correlation matrix to distance matrix
cluster <- hclust(dist_matrix)  # Apply hierarchical clustering

# Reorder the correlation matrix based on the cluster order
# Input: `cor_matrix`, `cluster$order`
# Output: Sorted correlation matrix (`cor_matrix_sorted`)
original_order <- colnames(cor_matrix)
cor_matrix_sorted <- cor_matrix[cluster$order, cluster$order]

# Convert the sorted correlation matrix to a long format for ggplot2
# Input: `cor_matrix_sorted` (sorted correlation matrix)
# Output: Dataframe `cor_df` in long format for ggplot2
cor_df <- as.data.frame(cor_matrix_sorted) %>%
  rownames_to_column(var = "Variable1") %>%
  pivot_longer(cols = -Variable1, names_to = "Variable2", values_to = "Correlation")

# Check if the order of the variables changed after clustering
# Output: Message confirming if the order changed
new_order <- colnames(cor_matrix_sorted)
if (identical(original_order, new_order)) {
  message("The order of variables did not change.")
} else {
  message("The order of variables changed.")
}

# 13. Plot the Correlation Matrix as a Heatmap
# Input: `cor_df` (long format correlation matrix)
# Output: Heatmap plot of correlations
cor_df %>% mutate(Variable1 = str_replace(Variable1, "\\s*\\(.*?\\)", "")) %>%
  mutate(Variable2 = str_replace(Variable2, "\\s*\\(.*?\\)", "")) %>%
  ggplot(aes(x = Variable1, y = Variable2, fill = Correlation)) +
  geom_tile(color = "white") +
  scale_fill_gradient2(low = "blue", high = "red", mid = "white", 
                       midpoint = 0, limit = c(-1, 1), name = "Correlation") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1)) +
  labs(title = "Correlation Matrix Heatmap", x = "Variables", y = "Variables")

# 14. Estimate Missing Values Using Correlation and Mean Imputation
# Function to estimate missing values based on correlated columns and overall mean
# Input: `data` (dataframe with NA values)
# Output: Imputed `data` with missing values filled
estimate_na_values <- function(data, cor_df, country_mean) {
  
  data <- data %>%
    select(-any_of(c("Latitude", "Longitude")))  # Remove geographical columns
  
  # Calculate standard deviations for each column
  std_devs <- data %>%
    summarize(across(everything(), \(x) sd(x, na.rm = TRUE))) %>%
    as.list()
  
  # Create a copy of the data for imputing values
  data_imputed <- data
  
  # Iterate over each column to impute missing values
  for (col_name in names(data)) {
    na_indices <- which(is.na(data[[col_name]]))  # Identify NA indices for the column
    
    if (length(na_indices) > 0) {
      mean_col <- country_mean[[col_name]]
      sd_col <- std_devs[[col_name]]
      
      if (!is.null(mean_col) && !is.null(sd_col) && !is.na(mean_col) && !is.na(sd_col)) {
        for (i in na_indices) {
          # Estimate NA values based on correlated columns
          estimated_value <- mean_col
          adjusted <- FALSE
          
          for (other_col_name in names(data)) {
            if (other_col_name != col_name && !is.na(data[[other_col_name]][i])) {
              # Adjust the estimate based on correlation and standard deviation
              correlation <- cor_df[col_name, other_col_name, drop = TRUE]
              if (!is.na(correlation) && !is.null(correlation)) {
                estimated_value <- estimated_value + 
                  correlation * (data[[other_col_name]][i] - country_mean[[other_col_name]]) * (sd_col / std_devs[[other_col_name]])
                adjusted <- TRUE
              }
            }
          }
          
          if (adjusted) {
            data_imputed[[col_name]][i] <- estimated_value
          } else {
            data_imputed[[col_name]][i] <- mean_col
          }
        }
      }
    }
  }
  
  return(data_imputed)
}

# 15. Principal Component Analysis (PCA) on the Numeric Data
# Perform PCA on the numeric data
# Input: `data_pivot_numeric` (numeric-only data)
# Output: PCA results including scores and loadings
pca_result <- prcomp(data_pivot_numeric, center = TRUE, scale. = TRUE)

# Extract PCA scores and loadings
# Input: `pca_result` (PCA results)
# Output: Dataframes `scores` and `loadings` containing PCA scores and loadings
scores <- as_tibble(pca_result$x) %>% mutate(Observation = rownames(data_pivot_numeric))
loadings <- as_tibble(pca_result$rotation)

# 16. Plot PCA Results (2D and 3D)
# 2D PCA plot
# Input: `scores`, `loadings`
# Output: 2D PCA biplot
ggplot() +
  geom_point(data = scores, aes(x = PC1, y = PC2), color = "gray50", size = 3, alpha = 0.6) +
  geom_segment(data = loadings, aes(x = 0, y = 0, xend = PC1, yend = PC2, color = Label),
               arrow = arrow(length = unit(0.3, "cm")), size = 1) +
  geom_text(data = loadings, aes(x = PC1, y = PC2, label = Number), hjust = -0.3, vjust = -0.3) +
  labs(x = "PC1", y = "PC2", title = "2D PCA Biplot with Vectors") +
  theme_minimal()

# 3D PCA plot
# Input: `scores`, `loadings`
# Output: 3D PCA biplot using plotly
pca_3d_plot <- plot_ly() %>%
  add_trace(type = 'scatter3d', mode = 'markers', x = scores$PC1, y = scores$PC2, z = scores$PC3,
            marker = list(color = 'gray', size = 5), name = 'Observations')

# Add vectors for the loadings
for (i in 1:nrow(loadings)) {
  pca_3d_plot <- pca_3d_plot %>%
    add_trace(type = 'scatter3d', mode = 'lines+text', x = c(0, loadings$PC1[i]),
              y = c(0, loadings$PC2[i]), z = c(0, loadings$PC3[i]),
              line = list(color = color_palette[loadings$Label[i]], width = 2),
              text = loadings$Number[i], textposition = 'top center', showlegend = TRUE,
              name = loadings$Label[i])
}

# Layout customization for 3D plot
pca_3d_plot <- pca_3d_plot %>%
  layout(scene = list(xaxis = list(title = "PC1"), yaxis = list(title = "PC2"), zaxis = list(title = "PC3")),
         title = "3D PCA Biplot with Colorful Vectors")

# Display the 3D PCA plot
pca_3d_plot

# 17. Analyze relationships between PCA components
# Function to calculate relationships (angles) between PCA components based on loadings
# Input: `loadings` (PCA loadings matrix), `pc_columns` (columns to analyze), `label` (label for the relationship)
# Output: Dataframe showing relationships (angles and similarity) between components
identify_pca_relationships <- function(loadings, pc_columns, label) {
  # Normalize the loading vectors for specified PCs
  normalized_loadings <- loadings %>%
    rowwise() %>%
    mutate(
      Norm_Vector = list(across(all_of(pc_columns), ~ . / sqrt(sum(across(all_of(pc_columns))^2))))
    ) %>%
    unnest_wider(Norm_Vector, names_sep = "_Norm_")
  
  # Calculate angles between each pair of vectors without repetition
  relationships <- expand.grid(Variable1 = normalized_loadings$Variable, Variable2 = normalized_loadings$Variable) %>%
    filter(as.character(Variable1) < as.character(Variable2)) %>%
    rowwise() %>%
    mutate(
      Angle = angle_between_vectors(
        as.numeric(normalized_loadings %>% filter(Variable == Variable1) %>% select(starts_with("Norm"))),
        as.numeric(normalized_loadings %>% filter(Variable == Variable2) %>% select(starts_with("Norm")))
      ),
      Relationship = case_when(
        Angle <= 30 ~ "Similar Gradients",
        Angle >= 150 ~ "Opposite Gradients",
        TRUE ~ "No Significant Relationship"
      )
    ) %>%
    select(Variable1, Variable2, Angle, Relationship) %>%
    rename(!!paste0("Relationship_", label) := Relationship,
           !!paste0("Angle_", label) := Angle)
  
  return(relationships)
}

# Function to calculate the angle between two vectors
# Input: Two vectors `v1` and `v2`
# Output: The angle (in degrees) between the vectors
angle_between_vectors <- function(v1, v2) {
  cos_theta <- sum(v1 * v2) / (sqrt(sum(v1^2)) * sqrt(sum(v2^2)))
  theta <- acos(cos_theta) * (180 / pi)  # Convert to degrees
  return(theta)
}

# Calculate relationships for 2D (PC1, PC2), 3D (PC1, PC2, PC3), and all PCs
# Input: `loadings` (PCA loadings matrix)
# Output: Dataframes showing relationships between the principal components
relationships_2d <- identify_pca_relationships(loadings, c("PC1", "PC2"), "2D")
relationships_3d <- identify_pca_relationships(loadings, c("PC1", "PC2", "PC3"), "3D")
relationships_all <- identify_pca_relationships(loadings, colnames(loadings)[grepl("^PC", colnames(loadings))], "All")

# Merge the results for each unique pair of components
# Input: `relationships_2d`, `relationships_3d`, `relationships_all`
# Output: Merged dataframe of relationships
all_relationships <- relationships_2d %>%
  full_join(relationships_3d, by = c("Variable1", "Variable2")) %>%
  full_join(relationships_all, by = c("Variable1", "Variable2"))

# Filter out rows where all three relationships show "No Significant Relationship"
# Input: `all_relationships`
# Output: Filtered dataframe with significant relationships
all_relationships <- all_relationships %>%
  filter(!(Relationship_2D == "No Significant Relationship" &
             Relationship_3D == "No Significant Relationship" &
             Relationship_All == "No Significant Relationship"))

# Calculate the proximity to 0 or 180 for sorting (find the most significant relationships)
# Input: `all_relationships`
# Output: Sorted dataframe with the top 20 strongest relationships
all_relationships <- all_relationships %>%
  rowwise() %>%
  mutate(
    Closest_Angle_Distance = min(abs(Angle_All - 0), abs(Angle_All - 180), na.rm = TRUE)
  ) %>%
  ungroup() %>%
  arrange(Closest_Angle_Distance) %>%
  slice(1:20)  # Keep only the top 20 closest relationships

# Display the top 20 relationships
print(all_relationships, width = Inf, n = Inf)

# 18. Create a correlation plot between the top relationships
# Input: `all_relationships`
# Output: Plot showing the top relationships between PCA components
ggplot(all_relationships, aes(x = Angle_2D, y = Angle_3D, color = Relationship_All)) +
  geom_point(size = 3) +
  labs(title = "Top 20 Strongest PCA Relationships",
       x = "Angle between PC1 and PC2",
       y = "Angle between PC1 and PC3",
       color = "Relationship") +
  theme_minimal()

# 19. Advanced Outlier Detection and Handling
# Loop through each top relationship and detect outliers using studentized residuals
# Input: `all_relationships`, `data_pivot_numeric`
# Output: Dataframes showing outliers and cleaned data without them
plot_data_list <- list()
outliers_list <- list()

for (i in 1:nrow(all_relationships)) {
  var1 <- all_relationships$Variable1[i]
  var2 <- all_relationships$Variable2[i]
  
  # Select relevant columns from `data_pivot_numeric` and include `Place`
  # Input: `data_pivot_numeric`, columns corresponding to `var1` and `var2`
  # Output: Dataframe for the current pair of variables (X and Y)
  data_pair <- data_pivot_numeric %>%
    select(!!sym(var1), !!sym(var2), Place) %>%
    rename(X = !!sym(var1), Y = !!sym(var2))
  
  # Fit a linear model and calculate studentized residuals
  # Input: `data_pair` (current pair of variables)
  # Output: `studentized_residuals` (model diagnostics)
  model <- lm(Y ~ X, data = data_pair)
  studentized_residuals <- rstudent(model)
  
  # Add residuals to the data and separate outliers
  # Output: `data_pair` with added residuals and outliers detected
  data_pair <- data_pair %>%
    mutate(Studentized_Residuals = studentized_residuals, Variable1 = var1, Variable2 = var2, Index = i)
  
  # Identify outliers (abs(Studentized_Residuals) > 2.5)
  # Output: List of outliers for each pair
  outliers <- data_pair %>% filter(abs(Studentized_Residuals) > 2.5)
  outliers_list[[i]] <- outliers
  
  # Keep only points within ±3 standard deviations for plotting
  # Output: Cleaned data for plotting without outliers
  plot_data_list[[i]] <- data_pair %>% filter(abs(Studentized_Residuals) <= 3)
}

# Combine outliers and plot data into single data frames
# Input: `plot_data_list`, `outliers_list`
# Output: Dataframes `outliers_data` and `plot_data`
outliers_data <- bind_rows(outliers_list)
plot_data <- bind_rows(plot_data_list)

# Print details of removed outliers, sorted by Place
# Output: Print outliers with the corresponding details
if (nrow(outliers_data) > 0) {
  outliers_data %>%
    select(Place, Variable1, Variable2, Studentized_Residuals) %>%
    arrange(Place, desc(abs(Studentized_Residuals))) %>%
    print(n = Inf)
} else {
  cat("No outliers were removed.\n")
}

# 20. Plot cleaned data with simplified titles and variable names
# Input: `plot_data`, `variable_mapping`
# Output: Plot showing relationships between variables, cleaned and labeled
plot_data <- plot_data %>%
  mutate(
    Variable1 = recode(Variable1, !!!variable_mapping),
    Variable2 = recode(Variable2, !!!variable_mapping),
    Title = paste(Variable1, "vs", Variable2)
  )

ggplot(plot_data, aes(x = X, y = Y, color = Place)) +
  geom_point(alpha = 0.7) +
  geom_smooth(method = "lm", se = FALSE, color = alpha("grey", alpha = 0.6)) +
  facet_wrap(~ Title, scales = "free") +
  labs(
    x = "Variable 1",
    y = "Variable 2",
    color = "Place",
    title = "Top 20 Strongest PCA Relationships Colored by Place (Outliers Removed)"
  ) +
  theme_minimal()

# 21. Data Standardization and Analysis with Socioeconomic Variables
# Input: `data_pivot_numeric`, `soceco_factors`
# Output: Data for socioeconomic factors and clean data
soceco_factors <- c("Unemployment Rate Ages 16+", "Percent Foreign Born", "Percent 18+ High School Graduates", "Median Household Income (Dollars)")
soceco_data <- data_pivot_numeric %>%
  select(Place, all_of(soceco_factors)) 

# Standardize the socioeconomic variables and other predictors
# Input: `x_vars`, `y_vars` (socioeconomic and other data)
# Output: Standardized `x_vars` and `y_vars`
x_vars <- city_data %>%
  left_join(soceco_data, by = "Place") %>%
  select(-c(Place, County, Congressional_District, State)) %>%
  mutate_all(as.numeric) %>%
  mutate_all(~ replace(., is.na(.), 0))

y_vars <- data_pivot_numeric %>%
  select(-Place, -all_of(soceco_factors)) %>%
  mutate_all(as.numeric) %>%
  mutate_all(~ replace(., is.na(.), 0))

# Perform PCA on the socioeconomic data
# Input: `x_vars`
# Output: PCA results (`pca_x`), scores and loadings
pca_x <- prcomp(x_vars, scale. = TRUE)

# Extract PCA scores and loadings
# Input: `pca_x` (PCA results)
# Output: Dataframes `pca_scores` and `pca_loadings`
pca_scores <- as_tibble(pca_x$x) %>%
  mutate(id = row_number())

pca_loadings <- as_tibble(pca_x$rotation) %>%
  mutate(variable = rownames(pca_x$rotation))

# 22. Plot PCA biplot for socioeconomic data
# Input: `pca_scores`, `pca_loadings`
# Output: PCA biplot visualization
ggplot() +
  geom_point(data = pca_scores, aes(x = PC1, y = PC2), color = "blue", alpha = 0.5) +
  geom_segment(data = pca_loadings,
               aes(x = 0, y = 0, xend = PC1 * max(abs(pca_scores$PC1)), yend = PC2 * max(abs(pca_scores$PC2))),
               arrow = arrow(length = unit(0.2, "cm")), color = "red", alpha = 0.7) +
  geom_text(data = pca_loadings, aes(x = PC1 * max(abs(pca_scores$PC1)) * 1.1, 
                                     y = PC2 * max(abs(pca_scores$PC2)) * 1.1, label = variable),
            color = "red", hjust = 0.5) +
  ggtitle("PCA Biplot of Socioeconomic Variables") +
  xlab("PC1") + ylab("PC2") +
  theme_minimal()

# 23. Stepwise Regression with Socioeconomic Variables
# Perform stepwise regression on the socioeconomic variables to identify the best predictors for each outcome
# Input: `x_vars` (socioeconomic data), `y_vars` (dependent variables)
# Output: List of model summaries for each dependent variable
model_summaries <- list()

for (y_var_name in names(y_vars)) {
  
  # Apply string transformations to make model_name easier to read
  transformed_name <- y_var_name %>%
    str_replace("\\s*\\(.*?\\)", "") %>%  # Remove text within parentheses
    str_replace("Rate of Laboratory Confirmed Infections Caused by", "(RLC) Infections")
  
  # Set up the data for the current y_var
  y_var <- y_vars[[y_var_name]]
  data <- x_vars %>% mutate(y = y_var)  # Include y variable
  
  # Stepwise regression with a stricter penalty to find the optimal model
  full_model <- lm(y ~ ., data = data %>% select(-Place))
  stepwise_model <- step(full_model, direction = "backward", trace = 0, k = 3)  # Set k = 3 for stricter model selection
  
  # Store model summary, adjusted R^2, and model name
  model_summary <- summary(stepwise_model)
  adjusted_r_squared <- model_summary$adj.r.squared
  
  # Calculate VIFs for each predictor in the final model
  vif_values <- calculate_vif(stepwise_model)
  
  # Diagnostics: check residuals and remove outliers with |studentized residuals| > 3
  residuals <- rstudent(stepwise_model)
  non_outliers <- abs(residuals) <= 3
  data_no_outliers <- data[non_outliers, ]
  
  model_summaries[[transformed_name]] <- list(
    model_name = transformed_name,
    model = stepwise_model,
    summary = model_summary,
    adjusted_r_squared = adjusted_r_squared,
    vif_values = vif_values,
    data_no_outliers = data_no_outliers
  )
}

# 24. Extract the top 7 models based on adjusted R-squared
# Input: `model_summaries` (list of regression models)
# Output: Names of the top 7 models sorted by adjusted R-squared
top_7_models <- model_summaries %>%
  purrr::map_dfr(~ tibble(
    model_name = .x$model_name,
    adjusted_r_squared = .x$adjusted_r_squared
  )) %>%
  arrange(desc(adjusted_r_squared)) %>%
  slice(1:7) %>%
  pull(model_name)

# 25. Generate Regression Plots for the Top 7 Models
# For each of the top models, plot actual vs predicted values and display model diagnostics
for (model_name in top_7_models) {
  model_info <- model_summaries[[model_name]]
  stepwise_model <- model_info$model
  data_no_outliers <- model_info$data_no_outliers
  
  # Generate predicted values for data with outliers removed
  data_no_outliers <- data_no_outliers %>%
    mutate(predicted_y = predict(stepwise_model, newdata = data_no_outliers),
           actual_y = y)
  
  # Extract coefficients, t-values, and adjusted R-squared for the OLS box
  coefs <- coef(summary(stepwise_model))
  intercept <- round(coefs[1, "Estimate"], 2)
  adjusted_r_squared <- round(model_info$adjusted_r_squared, 3)
  
  # Create a formatted string for the selected variables and their t-values
  coef_text <- paste0("Intercept: ", intercept, "\nAdjusted R²: ", adjusted_r_squared, "\n\nVariables:\n")
  for (i in 2:nrow(coefs)) {
    coef_name <- rownames(coefs)[i]
    coef_est <- round(coefs[i, "Estimate"], 2)
    coef_tval <- round(coefs[i, "t value"], 2)
    coef_text <- paste0(coef_text, coef_name, ": ", coef_est, " (t = ", coef_tval, ")\n")
  }
  
  # Create the plot with OLS regression line, points, and OLS summary box
  plot <- ggplot(data_no_outliers, aes(x = predicted_y, y = actual_y, color = "grey")) +
    geom_abline(intercept = intercept, slope = coefs[2, "Estimate"], color = "blue", linetype = "dashed") + # Optimal OLS line
    geom_smooth(method = "lm", color = "red") + 
    geom_point(alpha = 0.6) +
    labs(title = paste("Actual vs Predicted for Model:", model_name),
         x = "Predicted Values", y = "Actual Values") +
    theme_minimal() +
    theme(legend.position = "none") +
    annotate("text", x = Inf, y = Inf, label = coef_text, 
             hjust = 1.1, vjust = 1.5, size = 3.5, color = "black",
             fontface = "italic", parse = FALSE, 
             label.size = 0.5, label.padding = unit(0.4, "lines"), 
             label.r = unit(0.15, "lines"), 
             label.background = element_rect(fill = "white", color = "black"))
  
  # Explicitly print the plot
  print(plot)
  
  readline(prompt = "Press [Enter] to view the next plot...")
}

# 26. Visualizing Multivariate Relationships
# Plot multivariate relationships for the top 7 models using pairwise scatter plots
# Input: `x_vars` (predictor data), `y_vars` (dependent variables)
# Output: Pairwise scatter plots for selected variables
for (model_name in top_7_models) {
  model_info <- model_summaries[[model_name]]
  stepwise_model <- model_info$model
  data_no_outliers <- model_info$data_no_outliers
  
  # Get the variables used in the model
  model_vars <- names(coef(stepwise_model))[-1]  # Exclude intercept
  
  # Create pairwise scatter plots for predictor variables
  plot_data <- data_no_outliers %>%
    select(all_of(model_vars))
  
  ggplot(plot_data, aes(x = plot_data[, 1], y = plot_data[, 2])) +
    geom_point(color = "blue", alpha = 0.5) +
    labs(title = paste("Pairwise Scatter Plot for Model:", model_name),
         x = model_vars[1], y = model_vars[2]) +
    theme_minimal()
}

# 27. Model Evaluation Metrics: RMSE, MAE, and R-squared
# Evaluate each model using RMSE (Root Mean Squared Error), MAE (Mean Absolute Error), and Adjusted R-squared
# Input: `model_summaries`
# Output: Evaluation metrics for each model
model_metrics <- purrr::map_dfr(model_summaries, function(model_info) {
  stepwise_model <- model_info$model
  data_no_outliers <- model_info$data_no_outliers
  
  # Calculate predicted values and residuals
  predictions <- predict(stepwise_model, newdata = data_no_outliers)
  residuals <- data_no_outliers$y - predictions
  
  # Calculate RMSE, MAE, and Adjusted R-squared
  rmse <- sqrt(mean(residuals^2))
  mae <- mean(abs(residuals))
  adj_r2 <- model_info$adjusted_r_squared
  
  tibble(
    Model = model_info$model_name,
    RMSE = rmse,
    MAE = mae,
    Adjusted_R2 = adj_r2
  )
})

# Display the model evaluation metrics
# Output: Table with evaluation metrics for each model
print(model_metrics)

