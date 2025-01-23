# Load required libraries
library(bnlearn)
library(Rgraphviz)

# Read the preprocessed data
data <- readRDS("processed_data_train1.rds")

blacklist = matrix(c(
  "Income", "Sex",
  "Diabetes", "Sex",
  "HealthyEating", "Sex",
  "Education", "Sex",
  "Smoker", "Sex",
  "BMI", "Sex",
  "PhysActivity", "Sex",
  "BMI", "Sex",
  "Age", "Sex",
  "Sex", "Age",
  "Diabetes", "Age",
  "Education", "Age",
  "PhysActivity", "Age",
  "Income", "Age",
  "Smoker", "Age",
  "AnyHealthcare", "Age",
  "BMI", "Age",
  "HvyAlcoholConsump", "Age",
  "Income", "Education"
  ), ncol = 2, byrow = TRUE)

whitelist = matrix(c(
  "Income", "Diabetes",
  "Income", "AnyHealthcare",
  "BMI", "Diabetes"
), ncol = 2, byrow = TRUE)


# Learn DAG structure using PC algorithm with less conservative parameters
pc.dag <- pc.stable(data, alpha = 0.05, 
                    # undirected = FALSE,  # Force direction learning
                    max.sx = 2, blacklist=blacklist, whitelist=whitelist)          # Allow for larger conditioning sets

# Print the learned structure
print("Network structure:")
print(pc.dag)


# Plot the DAG with customized appearance
graphviz.plot(pc.dag, 
              shape = "ellipse",
              main = "Diabetes Health Indicators DAG",
              highlight = list(nodes = "Diabetes", col = "red"))



library(dagitty)

dagitty_graph <- as.dagitty(pc.dag)

print(dagitty_graph)
