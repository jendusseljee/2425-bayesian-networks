library(dagitty)

g <- dagitty('dag {
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

data <- readRDS('pre_processed/processed_data_train1.rds')

# Calculate polychoric correlation matrix using lavaan
library(lavaan)
M <- lavCor(data)

# Plot and print the local test results
library(dagitty)
plotLocalTestResults( localTests( g, sample.cov=M, sample.nobs = nrow(data), type="cis" ))
localTests( g, sample.cov=M, sample.nobs = nrow(data), type="cis" )


library(semPlot)
fit <- sem( toString(g,"lavaan"), sample.cov=M, sample.nobs=nrow(data) )

semPaths(fit, 
         what = "std",  # Show standardized coefficients
         edge.label.cex = 0.5,
         node.label.cex = 0.8,
         residuals = FALSE,
         layout = "circle")

summary(fit)
