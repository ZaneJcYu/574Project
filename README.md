# README

## Description

## Data exploration

#### Data source

[scGen predicts single-cell perturbation responses]: https://www.nature.com/articles/s41592-019-0494-8

#### Preprocessing

* Only take data from the file `train_study.h5ad`

* 14983 sample in total, the expression matrix is stored in a sparse matrix, code below is used to convert the sparse matrix to dense matrix.

  ```python
  import anndata
  import pandas as pd
  from scipy.sparse import csr_matrix
  
  
  info = anndata.read('train_study.h5ad')
  dense_matrix = info.X.toarray()dense_matrix.shape
  # (14893,7000)
  ```

* Then get the observation information below:

  * We can get `sample index` , `cell_type`, `condition`, `mt_frac`, `n_counts`, `n_genes`, `percent_mito`, `study`.
  * From the the `cell type`, after `set()` function, we can see these cell types:`CD8 T cells`,`CD4 T cells`,`B cells`,`FCGR3A+ Monocytes`,`Dendritic cells`,`CD14+ Monocytes`,`NK cells`
