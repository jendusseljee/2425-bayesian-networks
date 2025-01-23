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
  "Income", "AnyHealthcare",
  "BMI", "Diabetes"
), ncol = 2, byrow = TRUE)


# Learn DAG structure using PC algorithm with less conservative parameters
pc.dag <- pc.stable(data, alpha = 0.5, 
                    # undirected = FALSE,  # Force direction learning
                    max.sx = NULL, test = "disCItest", blacklist=blacklist, whitelist=whitelist)          # Allow for larger conditioning sets

# Print the learned structure
print("Network structure:")
print(pc.dag)


# Plot the DAG with customized appearance
graphviz.plot(pc.dag, 
              shape = "ellipse",
              main = "Diabetes Health Indicators DAG",
              highlight = list(nodes = "Diabetes", col = "red"))





# Calculate and print node degrees to see connectivity
node_degrees <- degree(pc.dag)
print("\nNode connectivity (degrees):")
print(node_degrees)


# Calculate edge strengths using multiple criteria
mi_strengths <- arc.strength(pc.dag, data, criterion = "mi")
print("\nEdge strengths (mutual information):")
print(mi_strengths[order(-mi_strengths$strength), ])

# Perform bootstrap analysis with modified parameters
boot <- boot.strength(data, 
                      algorithm = "pc.stable",
                      R = 1000,          # Increased number of bootstrap samples
                      alpha = 0.05,      # Less conservative significance level
                      algorithm.args = list(max.sx = 5))

# Print strongest connections from bootstrap
print("\nMost reliable edges from bootstrap analysis (strength > 0.5):")
print(boot[boot$strength > 0.5, ])

# Plot network with bootstrap support
# Reduced threshold to show more connections
averaged.network <- averaged.network(boot, threshold = 0.5)
graphviz.plot(averaged.network, 
              shape = "ellipse",
              main = "Bootstrap-averaged DAG (threshold = 0.5)")

# Save plots
pdf("original_dag.pdf")
graphviz.plot(pc.dag, 
              shape = "ellipse",
              main = "Original DAG Structure")
dev.off()

pdf("bootstrap_dag.pdf")
graphviz.plot(averaged.network, 
              shape = "ellipse",
              main = "Bootstrap-averaged DAG (threshold = 0.5)")
dev.off()

# Network statistics
print("\nNetwork Statistics:")
print(paste("Number of nodes:", length(nodes(pc.dag))))
print(paste("Number of edges:", length(arcs(pc.dag))))

# Analyze Markov blanket for Diabetes
mb_diabetes <- mb(pc.dag, "Diabetes")
print("\nMarkov blanket for Diabetes:")
print(mb_diabetes)

# Additional network analysis
# Calculate local structure around Diabetes
children <- children(pc.dag, "Diabetes")
parents <- parents(pc.dag, "Diabetes")

print("\nLocal structure around Diabetes:")
print("Parents:")
print(parents)
print("Children:")
print(children)

# Calculate and print all paths to/from Diabetes
paths_to_diabetes <- incoming.paths(pc.dag, "Diabetes")
print("\nPaths to Diabetes:")
print(paths_to_diabetes)