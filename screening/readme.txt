requirement: python 3 and anndata
datafile: "train_study.h5ad", "valid_study.h5ad"


Brief summary of csv files
Y (1, -1): label
train (T, F): choosing the training data set that was generated from "set.seed(seed=2023)" and "sample".
Other columns: records of continuous gene expression levels in a numeric format.
