# Load necessary libraries
library(dplyr)

# Load the dataset
data <- read.csv('cdc_diabetes_health_indicators_train2.csv')

# Remove MentHlth and PhysHlth columns
data <- data %>%
  select(-MentHlth, -PhysHlth)

# Convert binary variables to ordered factors
# For binary health indicators, we'll use "No" and "Yes" as labels
binary_vars <- c("HighBP", "HighChol", "Smoker", "Stroke", "HeartDiseaseorAttack", 
                 "PhysActivity", "HvyAlcoholConsump", "AnyHealthcare", "NoDocbcCost", 
                 "DiffWalk")

data <- data %>%
  mutate(across(all_of(binary_vars), 
                ~factor(.x, levels = c(0, 1), 
                        labels = c("No", "Yes"), 
                        ordered = TRUE)))


data$Diabetes <- factor(data$Diabetes, 
                        levels = c(0, 1), 
                        labels = c("No", "Yes"))

data$BMI <- as.numeric(data$BMI)

data$Age <- factor(data$Age, 
                   levels = 1:13,
                   labels = c("18-24", "25-29", "30-34", "35-39", "40-44", "45-49",
                              "50-54", "55-59", "60-64", "65-69", "70-74", "75-79", "80+"),
                   ordered = TRUE)

data$Education <- factor(data$Education, 
                         levels = 1:6,
                         labels = c("Never attended", "Elementary", "Some high school", 
                                    "High school graduate", "Some college", "College graduate"),
                         ordered = TRUE)

data$Income <- factor(data$Income, 
                      levels = 1:8,
                      labels = c("<$10k", "$10-15k", "$15-20k", "$20-25k", "$25-35k",
                                 "$35-50k", "$50-75k", ">$75k"),
                      ordered = TRUE)

# Convert Sex to a factor (not ordered since it's nominal)
data$Sex <- factor(data$Sex, 
                   levels = c(0, 1), 
                   labels = c("Female", "Male"))

# Convert HealthyEating to an ordered factor
data$HealthyEating <- factor(data$HealthyEating, 
                             levels = 0:2, 
                             labels = c("Low", "Medium", "High"), 
                             ordered = TRUE)

# Save the preprocessed data as an RDS file
saveRDS(data, "processed_data_train2.rds")

# View structure of the preprocessed data
str(data)
