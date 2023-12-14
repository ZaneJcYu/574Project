## Exploring single-cell perturbation marker genes using classification methods for high-dimensional data

### Introduction
We are going to inspect the performance of classification methods using high-dimensional features.
We consider aspects of prediction accuarcy and model interpretability based on a particular domain knowledge to
evaluate the performance for each method in this report.

#### Data
To investigate the performance of our methods, we apply them to a published single-cell transcriptomics data
from (Kang et al., 2018). The data of Kang et al. (2018) consists of 14,983 samples in a train data file and 2,000
samples in a test data file with information of seven different cell types and the expression level for 7000 genes.
For our project, we choose 1115 samples including 541 stimulated and 574 control cells for one cell type, CD8+ T
cells or cytotoxic T cells (TC), from the files.
|Cell|Stimulated|Control|Total|
|---------|---------|---------|---------|
|Whole data|541|574|1115|
|Train data|437|455|892|
|Test data|104|119|223|

#### Methods
Here we choose five different classification methods `MARS`,`NSC`,`Linear SVM`, `Sparse Linear SVM` and `Linear Regression Model`.
The performance of each method on test dataset is in the below:
|Model|Selected Genes|Test Error|Accuracy|Sensitivity|Specificity|
|---------|---------|---------|---------|---------|---------|
|MARS|16|0.0359|0.9641|0.9327|0.9916|
|NSC|174|0.0314|0.9686|0.9423|0.9916|
|Linear SVM|53|0.0179|0.9821|0.9712|0.9916|
|Sparse Linear SVM with Elastic net|31|0.0179|0.9821|0.9712|0.9916|
|Linear Regression Model|13|0.0359|0.9641|0.9327|0.9916|
