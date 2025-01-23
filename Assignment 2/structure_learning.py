from pgmpy.estimators import PC
from pgmpy.independencies import Independencies, IndependenceAssertion
import pandas as pd
import numpy as np
import networkx as nx
import matplotlib.pyplot as plt

def run_pc_algorithm(data, whitelist=None, blacklist=None, alpha=0.05, max_cond_vars=3):
    """
    Added max_cond_vars parameter to limit the number of conditioning variables
    """
    est = PC(data=data)
    
    # Create independencies from blacklist
    indeps = Independencies()
    if blacklist:
        for (i, j) in blacklist:
            indeps.add_assertions(IndependenceAssertion(i, j))
    
    # Estimate with modified parameters
    estimated_model = est.estimate(
        variant="stable",
        ci_test="pearsonr",
        significance_level=alpha,
        max_cond_vars=max_cond_vars,  # Limit conditioning set size
        known_independencies=indeps if blacklist else None
    )
    
    return estimated_model

def visualize_graph(model):
    """
    Safely visualize the graph with better layout and formatting
    """
    plt.figure(figsize=(15, 10))
    
    # Create a new directed graph
    G = nx.DiGraph()
    G.add_edges_from(model.edges())
    
    # Use a better layout algorithm
    pos = nx.spring_layout(G, k=1, iterations=50)
    
    # Draw nodes
    nx.draw_networkx_nodes(G, pos, node_color='lightblue', 
                          node_size=2000, alpha=0.7)
    
    # Draw edges
    nx.draw_networkx_edges(G, pos, edge_color='gray', 
                          arrows=True, arrowsize=20)
    
    # Draw labels
    nx.draw_networkx_labels(G, pos, font_size=10)
    
    plt.title("Causal Graph for Diabetes Indicators")
    plt.axis('off')
    plt.tight_layout()
    
    # Save and show
    plt.savefig("causal_graph.png", dpi=300, bbox_inches='tight')
    plt.show()

if __name__ == "__main__":
    # Read the data
    data = pd.read_csv('cdc_diabetes_health_indicators_train1.csv')
    
    print("Columns in dataset:", list(data.columns))
    
    # Define blacklist - let's test with some clear relationships
    blacklist = [
        ('Income', 'Education'),
        ('Diabetes', 'Age'),      # Diabetes shouldn't cause Age
        ('Diabetes', 'Sex'),      # Diabetes shouldn't cause Sex
        ('BMI', 'Age'),          # BMI shouldn't cause Age
        ('BMI', 'Sex'),           # BMI shouldn't cause Sex
        ('Age', 'Sex'),
        ('Sex', 'Age')
    ]
    
    # Define whitelist if needed
    whitelist = [
        ('Age', 'Diabetes'),     # Age can influence Diabetes
        ('BMI', 'Diabetes')      # BMI can influence Diabetes
    ]
    
    # Run PC algorithm
    model = run_pc_algorithm(
        data=data,
        whitelist=whitelist,
        blacklist=blacklist,
        alpha=0.05
    )
    
    # Print edges and verify blacklist enforcement
    print("\nDiscovered edges:")
    edges = list(model.edges())
    for edge in edges:
        print(f"{edge[0]} -> {edge[1]}")
    
    # Verify blacklist enforcement
    print("\nVerifying blacklist enforcement:")
    for (i, j) in blacklist:
        forward = model.has_edge(i, j)
        backward = model.has_edge(j, i)
        print(f"Blacklisted edge {i} -> {j}: {'Present!' if forward else 'Not present'}")
        print(f"Blacklisted edge {j} -> {i}: {'Present!' if backward else 'Not present'}")

    # Visualize the graph
    visualize_graph(model)