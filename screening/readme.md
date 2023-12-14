## Basic information

Requirement: `Python 3` and `anndata`

Rawdata: `data/train_study.h5ad`, `data/valid_study.h5ad`


## Summary of csv files

`Y`: label `=1` (stimulated; counts:541), `=-1` (control; counts:574)

`train`: choosing the training data set that was generated from `set.seed(seed=2023)` and `sample`. `=TRUE` (Train data; counts:892), `=FALSE` (Test data; counts:223)

Other columns: records of continuous gene expression levels in a numeric format.
