# Load necessary libraries
library(dplyr)

# Load the dataset
data <- read.csv('cdc_diabetes_health_indicators.csv')

# Remove MentHlth and PhysHlth columns
data <- data %>%
  select(-MentHlth, -PhysHlth)

# Convert binary variables to factors
data <- data %>%
  mutate(across(c(HighBP, HighChol, Smoker, Stroke, HeartDiseaseorAttack, PhysActivity,
                  HvyAlcoholConsump, AnyHealthcare, NoDocbcCost, Diabetes_binary, DiffWalk),
                as.factor))

# Discretize BMI into categories and convert it to an ordered factor
data$BMI <- cut(data$BMI, breaks = c(0, 18.5, 24.9, 29.9, 40), 
                labels = c("Underweight", "Normal", "Overweight", "Obese"), 
                ordered_result = TRUE)

data$GenHlth <- factor(data$GenHlth, levels = 1:5, ordered = TRUE)
data$Age <- factor(data$Age, levels = 1:13, ordered = TRUE)
data$Education <- factor(data$Education, levels = 1:6, ordered = TRUE)
data$Income <- factor(data$Income, levels = 1:8, ordered = TRUE)

# Convert Sex to a factor (0 = female, 1 = male)
data$Sex <- factor(data$Sex, levels = c(0, 1), labels = c("Female", "Male"))

# Convert HealthyEating to an ordered factor if it represents ordinal data
data$HealthyEating <- factor(data$HealthyEating, levels = 0:2, labels = c("Low", "Medium", "High"), ordered = TRUE)

# Save the preprocessed data as an RDS file
saveRDS(data, "processed_data.rds")

# View structure of the preprocessed data
str(data)

