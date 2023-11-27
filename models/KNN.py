import anndata
import pandas as pd
from sklearn.model_selection import train_test_split
from sklearn.neighbors import KNeighborsClassifier
from sklearn.metrics import zero_one_loss
# define dataset
info = anndata.read_h5ad('train_study.h5ad')
dense_matrix = info.X.toarray()
dense_matrix.shape
num_samples = dense_matrix.shape[0]

X = dense_matrix
y = pd.read_csv('obs.csv').cell_type.values

X_train, X_test, y_train, y_test = train_test_split(X, y, random_state=45)

#searched between 1 and 100, k = 11 was the best
knn = KNeighborsClassifier(n_neighbors = 11)
knn.fit(X_train, y_train)
y_pred = knn.predict(X_test)
test_error = zero_one_loss(y_test, y_pred)
print(test_error)
