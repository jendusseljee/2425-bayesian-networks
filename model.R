d_train <- read.csv('cdc_diabetes_health_indicators_train1.csv')
d_test <- read.csv('cdc_diabetes_health_indicators_test.csv')

d_scaled <- as.data.frame(scale(d))

library(dagitty)
library(bnlearn)
#library(gRain)

g <-dagitty('dag {
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
