# Load necessary library
library(bnlearn)
library(dagitty)

# Load the preprocessed dataset
data <- readRDS("processed_data.rds")

# Rename Diabetes_binary to Diabetes
# data <- data %>% rename(Diabetes = Diabetes_binary)

# Define your DAGitty graph without positions
dag <- dagitty('dag {
bb="-3.515,-2.393,3.029,2.184"
Age [pos="-1.281,0.323"]
AnyHealthcare [pos="1.005,-0.399"]
BMI [pos="-1.068,-1.389"]
Diabetes [pos="-0.377,1.684"]
Education [pos="0.594,1.358"]
HealthyEating [pos="-2.969,-0.185"]
HvyAlcoholConsump [pos="1.620,-0.969"]
Income [pos="-0.267,-1.012"]
PhysActivity [pos="-2.558,1.594"]
Sex [pos="0.101,0.038"]
Smoker [pos="2.209,1.143"]
Age -> AnyHealthcare
Age -> BMI
Age -> Diabetes
Age -> Education
Age -> HealthyEating
Age -> Income
Age -> PhysActivity
Age -> Smoker
AnyHealthcare -> Diabetes
AnyHealthcare -> HvyAlcoholConsump
AnyHealthcare -> Smoker
BMI -> Diabetes
Education -> AnyHealthcare
Education -> BMI
Education -> Diabetes
Education -> HealthyEating
Education -> HvyAlcoholConsump
Education -> Income
Education -> PhysActivity
Education -> Smoker
HealthyEating -> BMI
HealthyEating -> Diabetes
HvyAlcoholConsump -> BMI
HvyAlcoholConsump -> Smoker
Income -> AnyHealthcare
Income -> BMI
Income -> Diabetes
Income -> HealthyEating
Income -> HvyAlcoholConsump
Income -> Smoker
PhysActivity -> BMI
PhysActivity -> Diabetes
PhysActivity -> HealthyEating
PhysActivity -> Income
Sex -> AnyHealthcare
Sex -> BMI
Sex -> Diabetes
Sex -> Education
Sex -> HealthyEating
Sex -> HvyAlcoholConsump
Sex -> Income
Sex -> PhysActivity
Sex -> Smoker
Smoker -> Diabetes
}
')

dag_variables <- c("Age", "AnyHealthcare", "BMI", "Diabetes", "Education", 
                   "HealthyEating", "HvyAlcoholConsump", 
                   "Income", "PhysActivity", "Sex", "Smoker")
# Remove extra variables not in the DAG
data <- data %>%
  select(all_of(dag_variables))


# Convert DAGitty graph to bnlearn-compatible model string
dag_bnlearn_string <- toString(dag, "bnlearn")

# Convert to bnlearn network
bn_network <- model2network(dag_bnlearn_string)

# Fit the network with MLE using your preprocessed data
fit <- bn.fit(bn_network, data = data, method="mle")

# Inspect the fitted network
print(fit)

d <- read.csv('cdc_diabetes_health_indicators_train2.csv')

d_scaled <- as.data.frame(scale(d))

model = sem(toString(dag, "lavaan"), data = d)

library(semPlot)
library(lavaan)

semPaths(model, 
        what = "std",  # Show standardized coefficients
        edge.label.cex = 0.5,
        node.label.cex = 0.8,
        residuals = FALSE,
        layout = "circle")



# Example usages:



# Convert the CPT for Diabetes to a data frame
diabetes_cpt <- as.data.frame(fit$Diabetes$prob)
print(diabetes_cpt)

# Print CPTs for each node 
for (node in names(fit)) {
  cat("\nConditional Probability Table for", node, ":\n")
  print(fit[[node]])
}

d <- read.csv('cdc_diabetes_health_indicators_train2.csv')
# 1. First, make sure all variables are properly scaled

d_scaled <- as.data.frame(scale(d))

model <- sem(toString(dag, "lavaan"), d_scaled)

semPaths(model,
         what = "std",
         #edge.width = abs(coefs$est.std) * 5,  # Width proportional to effect size
         residuals = FALSE,
         layout = "circle",
         standardized = TRUE)

summary(model, standardized=TRUE)








effects <- parameterEstimates(model, standardized = TRUE)

# Filter and show direct effects for Diabetes
direct_effects <- effects[effects$lhs == "Diabetes" & effects$op == "~", ]
print("Direct Effects on Diabetes:")
print(direct_effects)


# Filter and show indirect effects through mediators
indirect_effects <- effects[effects$lhs %in% c("BMI", "HealthyEating", "PhysActivity") &
                              effects$op == "~" & 
                              effects$rhs %in% c("Income", "Education"), ]
print("Indirect Effects via Mediators:")
print(indirect_effects)


# Summarize total effects (direct + indirect) for each predictor
total_effects <- merge(
  direct_effects[, c("rhs", "est")],
  aggregate(est ~ rhs, data = indirect_effects, sum),
  by = "rhs", all = TRUE
)
colnames(total_effects) <- c("Predictor", "Direct_Effect", "Indirect_Effect")
total_effects$Total_Effect <- rowSums(total_effects[, c("Direct_Effect", "Indirect_Effect")], na.rm = TRUE)
print("Total Effects on Diabetes:")
print(total_effects)
