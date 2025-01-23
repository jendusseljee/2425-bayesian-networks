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

adj_g <- dagitty('dag {
bb="-2.774,-2.794,2.771,2.514"
Age [adjusted,pos="-1.637,-1.495"]
BMI [pos="-0.467,-0.197"]
Diabetes [outcome,pos="0.485,-2.294"]
Education [adjusted,pos="-2.274,0.721"]
HealthyEating [pos="1.215,-0.748"]
Income [exposure,pos="-0.435,1.410"]
PhysActivity [pos="0.868,0.613"]
Sex [adjusted,pos="2.271,2.014"]
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

plot(g)
plot(adj_g)

train_data <- readRDS("processed_data_train1.rds")
test_data <- readRDS("processed_data_test.rds")

print(train_data)

M <- lavCor(train_data)

fit <- sem( toString(adj_g,"lavaan"), sample.cov=M, sample.nobs=nrow(data) )

semPaths(fit, 
         what = "std", 
         edge.label.cex = 0.5,
         node.label.cex = 0.8,
         residuals = FALSE,
         layout = "circle")

summary(fit)

adjustment_sets <- adjustmentSets(g, exposure = "Income", outcome = "Diabetes")

n <- glm(Income ~ Diabetes, train_data, family="binomial")
coef(n)[1:2]

m <- glm(Income ~ Diabetes + Age + Sex + Education, train_data, family="binomial")
coef(m)[1:2]

print(m)
