library(dagitty)

g <- dagitty('dag {
bb="-2.621,-3.082,3.664,3.551"
Age [pos="-1.494,-1.252"]
AnyHealthcare [pos="-2.097,0.895"]
BMI [pos="0.870,-1.662"]
Diabetes [pos="2.630,0.800"]
Education [pos="1.446,-0.043"]
HealthyEating [pos="1.238,-0.719"]
HvyAlcoholConsump [pos="-0.777,1.771"]
Income [pos="-0.232,-0.364"]
PhysActivity [pos="3.140,-0.758"]
Sex [pos="0.859,1.183"]
Smoker [pos="2.376,1.715"]
Age -> AnyHealthcare
Age -> BMI
Age -> Diabetes
Age -> Education
Age -> Income
AnyHealthcare -> Diabetes
AnyHealthcare -> HvyAlcoholConsump
AnyHealthcare -> Smoker
BMI -> Diabetes
Education -> AnyHealthcare
Education -> HealthyEating
Education -> HvyAlcoholConsump
Education -> Income
Education -> PhysActivity
Education -> Smoker
HealthyEating -> BMI
HealthyEating -> Diabetes
HvyAlcoholConsump -> BMI
Income -> AnyHealthcare
Income -> HealthyEating
Income -> HvyAlcoholConsump
Income -> PhysActivity
Income -> Smoker
PhysActivity -> BMI
PhysActivity -> Diabetes
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
}')

data <- readRDS('processed_data_train1.rds')

# Calculate polychoric correlation matrix using lavaan
library(lavaan)
M <- lavCor(data)

# Plot and print the local test results
library(dagitty)
plotLocalTestResults( localTests( g, sample.cov=M, sample.nobs = nrow(data), type="cis" ))
localTests( g, sample.cov=M, sample.nobs = nrow(data), type="cis" )


# See the adjusted dag and possibly prune edges
updated_g = dagitty('dag {
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

library(semPlot)
fit <- sem( toString(updated_g,"lavaan"), sample.cov=M, sample.nobs=nrow(data) )

semPaths(fit, 
         what = "std",  # Show standardized coefficients
         edge.label.cex = 0.5,
         node.label.cex = 0.8,
         residuals = FALSE,
         layout = "circle")

summary(fit)


pruned_g = dagitty('dag {
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
BMI -> Diabetes
Education -> AnyHealthcare
Education -> BMI
Education -> Diabetes
Education -> HealthyEating
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
Sex -> Income
Sex -> PhysActivity
Sex -> Smoker
Smoker -> Diabetes
}
')

pruned_g_lavaan = paste(toString(pruned_g, "lavaan"), "\nSex ~~ 0*Age")

plotLocalTestResults( localTests( pruned_g, sample.cov=M, sample.nobs = nrow(data), type="cis" ))

fit <- sem( toString(pruned_g, "lavaan"), sample.cov=M, sample.nobs=nrow(data) )

semPaths(fit, 
         what = "std",  # Show standardized coefficients
         edge.label.cex = 0.5,
         node.label.cex = 0.8,
         residuals = FALSE,
         layout = "spring")

summary(fit)



heavily_pruned_g = dagitty('dag {
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

heavily_pruned_g_lavaan = paste(toString(heavily_pruned_g, "lavaan"), "\nSex ~~ 0*Age")

plotLocalTestResults( localTests( heavily_pruned_g, sample.cov=M, sample.nobs = nrow(data), type="cis" ))

fit <- sem( heavily_pruned_g_lavaan, sample.cov=M, sample.nobs=nrow(data) )

semPaths(fit, 
         what = "std",  # Show standardized coefficients
         edge.label.cex = 0.5,
         node.label.cex = 0.8,
         residuals = FALSE,
         layout="spring")

summary(fit)
