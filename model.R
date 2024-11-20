d_train <- read.csv('cdc_diabetes_health_indicators_train1.csv')
d_test <- read.csv('cdc_diabetes_health_indicators_test.csv')

d_scaled <- as.data.frame(scale(d))

library(dagitty)
library(bnlearn)
#library(gRain)

g <- dagitty('
dag {
bb="-2.621,-3.082,3.664,3.551"
Age [pos="-1.494,-1.252"]
AnyHealthcare [pos="-2.097,0.895"]
BMI [pos="0.870,-1.662"]
Diabetes [pos="2.630,0.800"]
Education [pos="0.662,0.063"]
HealthyEating [pos="1.430,-0.725"]
HvyAlcoholConsump [pos="-0.777,1.771"]
Income [pos="-0.616,0.002"]
PhysActivity [pos="3.140,-0.758"]
Sex [pos="0.859,1.183"]
Smoker [pos="2.376,1.715"]
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
Income -> PhysActivity
Income -> Smoker
PhysActivity -> BMI
PhysActivity -> Diabetes
PhysActivity -> HealthyEating
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
plot(g)
bn_dag <- convert(g, "bnlearn")

d_train <- d_train[, !(names(d_train) %in% c("HighBP", "HighChol","Stroke", "HeartDiseaseorAttack", "NoDocbcCost", "GenHlth", "DiffWalk",  "MentHlth", "PhysHlth"))]
d_test <- d_test[, !(names(d_test) %in% c("HighBP", "HighChol","Stroke", "HeartDiseaseorAttack", "NoDocbcCost", "GenHlth", "DiffWalk",  "MentHlth", "PhysHlth"))]

d_train$BMI <- as.factor(d_train$BMI)
d_train$Smoker <- as.factor(d_train$Smoker)
d_train$PhysActivity <- as.factor(d_train$PhysActivity)
d_train$HvyAlcoholConsump <- as.factor(d_train$HvyAlcoholConsump)
d_train$AnyHealthcare <- as.factor(d_train$AnyHealthcare)
d_train$Sex <- as.factor(d_train$Sex)
d_train$Age <- as.factor(d_train$Age)
d_train$Education <- as.factor(d_train$Education)
d_train$Income <- as.factor(d_train$Education)
d_train$Diabetes <- as.factor(d_train$Diabetes)
d_train$HealthyEating <- as.factor(d_train$HealthyEating)

d_test$BMI <- as.factor(d_test$BMI)
d_test$Smoker <- as.factor(d_test$Smoker)
d_test$PhysActivity <- as.factor(d_test$PhysActivity)
d_test$HvyAlcoholConsump <- as.factor(d_test$HvyAlcoholConsump)
d_test$AnyHealthcare <- as.factor(d_test$AnyHealthcare)
d_test$Sex <- as.factor(d_test$Sex)
d_test$Age <- as.factor(d_test$Age)
d_test$Education <- as.factor(d_test$Education)
d_test$Income <- as.factor(d_test$Education)
d_test$Diabetes <- as.factor(d_test$Diabetes)
d_test$HealthyEating <- as.factor(d_test$HealthyEating)

bn <- bnlearn::model2network(bn_dag)
fitted_bn <- bnlearn::bn.fit(bn, d_train)

library(gRbase)
library(gRain)

grain_bn <- as.grain(fitted_bn)

query <- querygrain(grain_bn, nodes = c("Diabetes"), type = "marginal")

all_variables <- names(d_test)

# Function to set evidence for all variables in a test row
predict_diabetes <- function(i) {
  # Set the evidence for each variable in the test row
  evidence <- setEvidence(grain_bn, nodes = all_variables, states = as.character(d_test[i, all_variables]))
  
  # Query the model for the probability of "Diabetes"
  query_with_evidence <- querygrain(evidence, nodes = c("Diabetes"), type = "marginal")
  
  # Return the predicted probability of Diabetes
  return(query_with_evidence$Diabetes)
}

d_test_10 <- head(d_test, 10)

# Combine predictions with test data (optional)
predictions <- sapply(d_test_10, predict_diabetes)

# Combine predictions with test data (optional)
d_test$PredictedDiabetes <- predictions

# View the predictions
head(d_test)


#library(lavaan)

#model <- sem(toString(g, "lavaan"), d)

#summary(model, standardized = TRUE, fit.measures = TRUE)
