# Load necessary library
library(bnlearn)
library(dagitty)

# Load the preprocessed dataset
data <- readRDS("processed_data.rds")

# Rename Diabetes_binary to Diabetes
data <- data %>% rename(Diabetes = Diabetes_binary)

# Define your DAGitty graph without positions
dag <- dagitty("dag {
  Age
  AnyHealthcare
  BMI
  Diabetes
  Education
  HealthyEating
  HighBP
  HighChol
  HvyAlcoholConsump
  Income
  PhysActivity
  Sex
  Smoker
  Age -> AnyHealthcare
  Age -> BMI
  Age -> Diabetes
  Age -> Education
  Age -> HighBP
  Age -> HighChol
  Age -> Income
  AnyHealthcare -> Diabetes
  AnyHealthcare -> HvyAlcoholConsump
  AnyHealthcare -> Smoker
  BMI -> Diabetes
  BMI -> HighBP
  BMI -> HighChol
  Education -> AnyHealthcare
  Education -> HealthyEating
  Education -> HvyAlcoholConsump
  Education -> Income
  Education -> PhysActivity
  Education -> Smoker
  HealthyEating -> BMI
  HealthyEating -> Diabetes
  HealthyEating -> HighBP
  HealthyEating -> HighChol
  HighBP -> Diabetes
  HighChol -> Diabetes
  HighChol -> HighBP
  HvyAlcoholConsump -> BMI
  HvyAlcoholConsump -> HighBP
  HvyAlcoholConsump -> HighChol
  Income -> AnyHealthcare
  Income -> HealthyEating
  Income -> HvyAlcoholConsump
  Income -> Smoker
  PhysActivity -> BMI
  PhysActivity -> Diabetes
  PhysActivity -> HighBP
  PhysActivity -> HighChol
  PhysActivity -> Income
  Sex -> AnyHealthcare
  Sex -> BMI
  Sex -> Diabetes
  Sex -> Education
  Sex -> HealthyEating
  Sex -> HighChol
  Sex -> HvyAlcoholConsump
  Sex -> Income
  Sex -> PhysActivity
  Sex -> Smoker
  Smoker -> Diabetes
  Smoker -> HighBP
  Smoker -> HighChol
}")

dag_variables <- c("Age", "AnyHealthcare", "BMI", "Diabetes", "Education", 
                   "HealthyEating", "HighBP", "HighChol", "HvyAlcoholConsump", 
                   "Income", "PhysActivity", "Sex", "Smoker")
# Remove extra variables not in the DAG
data <- data %>%
  select(all_of(dag_variables))


# Convert DAGitty graph to bnlearn-compatible model string
dag_bnlearn_string <- toString(dag, "bnlearn")

# Convert to bnlearn network
bn_network <- model2network(dag_bnlearn_string)

# Fit the network with MLE using your preprocessed data
fit <- bn.fit(bn_network, data = data)

# Inspect the fitted network
print(fit)



# Example usages:



# Convert the CPT for Diabetes to a data frame
diabetes_cpt <- as.data.frame(fit$Diabetes$prob)
print(diabetes_cpt)

# Print CPTs for each node 
for (node in names(fit)) {
  cat("\nConditional Probability Table for", node, ":\n")
  print(fit[[node]])
}


