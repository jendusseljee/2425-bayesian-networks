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

lrn_g <- dagitty('dag {
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
plot(lrn_g)

train_data <- readRDS("processed_data_train1.rds")

M <- lavCor(train_data)

fit <- sem( toString(g,"lavaan"), sample.cov=M, sample.nobs=nrow(train_data) )

semPaths(fit, 
         what = "std", 
         edge.label.cex = 0.5,
         node.label.cex = 0.8,
         residuals = FALSE,
         layout = "circle")

summary(fit)

adjustment_sets <- adjustmentSets(g, exposure = "Income", outcome = "Diabetes")

print(adjustment_sets)

n <- glm(Income ~ Diabetes, train_data, family="binomial")
coef(n)[1:2]

m <- glm(Income ~ Diabetes + Age + Sex + Education, train_data, family="binomial")
coef(m)[1:2]

fit_lrn <- sem( toString(lrn_g, "lavaan"), sample.cov=M, sample.nobs=nrow(train_data))

semPaths(fit_lrn, 
         what = "std", 
         edge.label.cex = 0.5,
         node.label.cex = 0.8,
         residuals = FALSE,
         layout = "circle")

summary(fit_lrn)

adjustment_sets_lrn <- adjustmentSets(lrn_g, exposure = "Income", outcome = "Diabetes")

print(adjustment_sets_lrn)

n_lrn <- glm(Income ~ Diabetes, train_data, family="binomial")
coef(n_lrn)[1:2]

m_lrn <- glm(Income ~ Diabetes + Age + Sex + Education + HvyAlcoholConsump, train_data, family="binomial")
coef(m_lrn)[1:2]
