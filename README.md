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
|CD8+ T cells|541|574|1115|

#### Methods
Here we choose five different classification methods `MARS`,`NSC`,`Linear SVM`, `Sparse SVM` and `Linear Regression Model`.
The performance of each method on test dataset is in the below:
|Model|Selected Genes|Test Error|Accuracy|Sensitivity|Specificity|
|---------|---------|---------|---------|---------|---------|
|MARS|16|0.03587|0.9641|0.9327|0.9916|
|NSC|174|0.03139|0.9686|0.9423|0.9916|
|Sparse Linear SVM with elastic net|356|0.06278|0.9372|0.9038|0.9664|
|Linear SVM|53|0.03587|0.9641|0.9327|0.9916|
|Linear Regression with Forward Selection |15|0.02691|0.9731|0.9916|0.9519|
