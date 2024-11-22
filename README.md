# Bayesian Networks Project 1: Diabetes Risk Factors
This repository contains the code we used for the first project of the course Bayesian Networks and Causal Inference.

We have the following files:
- images/: Images generated for the report.
- pre-process.ipynb: Used to download the data and perform some pre-processing.
- cdc_diabetes_health_indicators*.csv: The training and test dataset files.
- mixed-data.R: Loads the data into rds files in the right formats for polychoric covariation calculation.
- test_and_prune_structure.R: Contains the code to fit the model, perform correlation tests on implied independencies and read the causal effects from the model.
- processed_data*.rds: R dataframe files containing the processed training data.