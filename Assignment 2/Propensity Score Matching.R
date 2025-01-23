library(dagitty)
library(lavaan)
library(semPlot)

g <- dagitty('dag {
bb="-3.394,-2.543,2.588,3.329"
Age [pos="-1.421,-1.738"]
BMI [pos="-0.496,1.200"]
Diabetes [pos="-3.023,0.786"]
Education [pos="-1.748,-0.584"]
HealthyEating [pos="0.734,0.103"]
Income [pos="-0.457,-0.574"]
PhysActivity [pos="-1.555,0.560"]
Sex [pos="-0.131,-1.694"]
Age -> BMI
Age -> Diabetes
Age -> Education
Age -> HealthyEating
Age -> Income
Age -> PhysActivity
BMI -> Diabetes
Education -> BMI
Education -> HealthyEating
Education -> Income
Education -> PhysActivity
HealthyEating -> BMI
Income -> BMI
Income -> Diabetes
Income -> HealthyEating
Income -> PhysActivity
PhysActivity -> BMI
PhysActivity -> Diabetes
PhysActivity -> HealthyEating
Sex -> BMI
Sex -> HealthyEating
Sex -> Income
}
')

learned_g <- dagitty('dag {
Age
AnyHealthcare
BMI
Diabetes
Education
HealthyEating
HvyAlcoholConsump
Income
PhysActivity
Sex
Smoker
Age -> AnyHealthcare
Age -> BMI
Age -> Diabetes
Age -> Education
Age -> HvyAlcoholConsump
Age -> Income
Age -> PhysActivity
Age -> Smoker
BMI -> Diabetes
BMI -> PhysActivity
BMI -> Smoker
Diabetes -> PhysActivity
Diabetes -> Smoker
Education -> AnyHealthcare
Education -> Diabetes
Education -> Income
Education -> PhysActivity
Education -> Smoker
HealthyEating -> PhysActivity
HvyAlcoholConsump -> BMI
HvyAlcoholConsump -> Diabetes
HvyAlcoholConsump -> Income
HvyAlcoholConsump -> Smoker
Income -> AnyHealthcare
Income -> BMI
Income -> Diabetes
Income -> PhysActivity
Income -> Smoker
PhysActivity -> Smoker
Sex -> BMI
Sex -> Diabetes
Sex -> Education
Sex -> HealthyEating
Sex -> Income
Sex -> PhysActivity
Sex -> Smoker
}')

plot(g)
plot(learned_g)

run_analysis <- function(dag, data) {
  # Use the DAG to identify the minimal adjustment set
  adjustment_set <- adjustmentSets(dag, exposure = "Income", outcome = "Diabetes")
  print("Minimal adjustment set for the DAG:")
  print(adjustment_set)
  
  # Collapse Income into two levels
  data$IncomeBinary <- ifelse(as.numeric(data$Income) <= 4, "Low", "High")
  data$IncomeBinary <- as.factor(data$IncomeBinary)
  
  # Perform propensity score matching using the adjustment set
  matched_data <- matchit(
    IncomeBinary ~ Age + Sex + Education + PhysActivity + Smoker + AnyHealthcare,
    data = data, method = "nearest"
  )
  
  # Check balance after matching
  print("Balance Summary:")
  print(summary(matched_data))
  bal.tab(matched_data)
  
  # Extract matched data for further analysis
  matched_data <- match.data(matched_data)
  
  # Analyze the outcome (Diabetes) on the matched data
  outcome_analysis <- glm(Diabetes ~ IncomeBinary, data = matched_data, family = binomial())
  print("Outcome Analysis Summary:")
  print(summary(outcome_analysis))
}

# Load libraries
library(MatchIt)
library(cobalt)
# Load dataset
data <- readRDS("processed_data_train1.rds")

# Run the analysis for DAG `g`
cat("\n--- Analysis for DAG g ---\n")
run_analysis(dag = g, data = data)

# Run the analysis for DAG `learned_g`
cat("\n--- Analysis for Learned DAG ---\n")
run_analysis(dag = learned_g, data = data)



