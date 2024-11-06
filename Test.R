d <- read.csv('cdc_diabetes_health_indicators.csv')

library(dagitty)

g <- dagitty('dag {
bb="0,0,1,1"
Age [pos="0.115,0.645"]
AnyHealthcare [pos="0.116,0.921"]
BMI [pos="0.608,0.507"]
Diabetes [pos="0.693,0.677"]
EatsHealthy [pos="0.412,0.513"]
Education [pos="0.797,0.927"]
HighBP [pos="0.130,0.367"]
HighChol [pos="0.310,0.450"]
HvyAlcoholConsump [pos="0.178,0.213"]
Income [pos="0.082,0.770"]
NoDocbcCost [pos="0.350,0.750"]
PhysActivity [pos="0.743,0.242"]
Sex [pos="0.064,0.563"]
Smoker [pos="0.592,0.364"]
Age -> Diabetes
Age -> HighBP
Age -> HighChol
BMI -> Diabetes
BMI -> HighBP
EatsHealthy -> BMI
Education -> Income
Education -> PhysActivity
HighChol -> HighBP
HvyAlcoholConsump -> HighBP
Income -> AnyHealthcare
Income -> NoDocbcCost
PhysActivity -> BMI
PhysActivity -> HighBP
Sex -> Income
Smoker -> HighBP
Smoker -> HighChol
}')
plot(g)

library(lavaan)

model <- sem(toString(g, "lavaan"), d)

plotLocalTestResults( localTests( g, d, type="cis.chisq" ) )
