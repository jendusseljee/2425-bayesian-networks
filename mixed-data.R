df <- read.csv("cdc_diabetes_health_indicators_test.csv")

# Pre-process binary variables
binary_vars <- c("Smoker", "PhysActivity", "HvyAlcoholConsump", "AnyHealthcare", "Sex", "Diabetes")
df[binary_vars] <- lapply(df[binary_vars], as.numeric)

# Pre-process BMI
df$BMI <- as.numeric(df$BMI)

# Process remaining variables as ordered
df$Age <- ordered(df$Age, levels=1:13)
df$Education <- ordered(df$Education, levels=1:6)
df$Income <- ordered(df$Income, levels=1:8)
df$HealthyEating <- ordered(df$HealthyEating, levels=1:3)
saveRDS(df, "processed_data_test.rds")