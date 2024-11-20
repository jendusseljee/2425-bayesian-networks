d <- read.csv('cdc_diabetes_health_indicators_train1.csv')

d_scaled <- as.data.frame(scale(d))

library(dagitty)
library(bnlearn)
library(gRain)

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
# plot(g)



plotLocalTestResults( localTests( g, d_scaled, type="cis" ))
plotLocalTestResults( localTests( g, d, type="cis.chisq" ))


library(lavaan)

model <- sem(toString(g, "lavaan"), d)

summary(model, standardized = TRUE, fit.measures = TRUE)

library(semPlot)

coefs <- standardizedSolution(model)

# Color-coded paths
semPaths(model,
         what = "est",
         edge.label.cex = 0.5,
         node.label.cex = 0.8,
         edge.color = c("blue", "red")[1 + (coefs$est.std < 0)],  # Blue positive, red negative
         #edge.width = abs(coefs$est.std) * 5,  # Width proportional to effect size
         residuals = FALSE,
         #layout = "tree",
         standardized = TRUE)




d <- read.csv('cdc_diabetes_health_indicators_train2.csv')
# 1. First, make sure all variables are properly scaled

d_scaled <- as.data.frame(scale(d))

model <- sem(toString(g, "lavaan"), d_scaled)

semPaths(model,
         what = "std",
         #edge.width = abs(coefs$est.std) * 5,  # Width proportional to effect size
         residuals = FALSE,
         layout = "circle",
         standardized = TRUE)
