import numpy as np
import anndata
import pandas as pd


info = anndata.read('train_study.h5ad')
dense_matrix = info.X.toarray()

control_index = np.where(info.obs['condition']=='control')
control_matrix = info.X[control_index]

## control_index start from 7772
genes = info.var_names.values
controls = info.obs[7772:]
tc_index = np.where(controls['cell_type']=='CD8 T cells') #517*7000
tc_cells = control_matrix[tc_index]
tc_obs = controls[controls['cell_type']=='CD8 T cells']
tc_dense_matrix = tc_cells.toarray()

nk_index = np.where(controls['cell_type']=='NK cells') #471*7000
nk_cells = control_matrix[nk_index]
nk_obs = controls[controls['cell_type']=='NK cells']
nk_dense_matrix = nk_cells.toarray()

total_matrix = np.concatenate((tc_dense_matrix,nk_dense_matrix))

non_zero_cols = np.where(~np.all(total_matrix == 0, axis=0))[0]
non_zero_genes = genes[non_zero_cols].tolist()
total_matrix = total_matrix[:,non_zero_cols]
index = ['TC']*517 + ['NK']*471
dataset = pd.DataFrame(total_matrix,columns=non_zero_genes,index=index)
print(dataset)
dataset.to_csv('dataset.csv')

