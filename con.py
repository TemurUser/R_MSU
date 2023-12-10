import pandas as pd
import numpy as np
import datetime
import timeit
import pyarrow.parquet as pq
from scipy.sparse import csr_matrix
from sklearn.metrics.pairwise import cosine_similarity
from sklearn.metrics import pairwise_distances
from sklearn.preprocessing import normalize
from ml_metrics import mapk
import gc
import pyspark

PATH_DATA = '/Users/TemurMdov/Downloads/'

def get_date_k_weeks_before(cur_date: str, k: int = 2) -> str:
    cur_date = datetime.datetime.strptime(cur_date, "%Y-%m-%d")
    return (cur_date - datetime.timedelta(weeks=k)).strftime("%Y-%m-%d")

def remove_duplicates_order(x: list) -> list:
    return list(dict.fromkeys(x))

def create_user_product_matrix(df: pd.DataFrame) -> csr_matrix:
    users = pd.Series(df['user_id'].unique())
    products = pd.Series(df['product_id'].unique())

    user_mapping = dict(enumerate(users))
    reverse_user_mapping = {v: k for k, v in user_mapping.items()}

    product_mapping = dict(enumerate(products))
    reverse_product_mapping = {v: k for k, v in product_mapping.items()}

    data = np.ones(len(df))
    row_indices = df['user_id'].map(reverse_user_mapping)
    col_indices = df['product_id'].map(reverse_product_mapping)

    user_product_matrix = csr_matrix((data, (row_indices, col_indices)), shape=(len(users), len(products)))

    return user_product_matrix

def get_validation_split(df: pd.DataFrame, timestamp_to_split: str) -> tuple[pd.DataFrame, pd.DataFrame, pd.DataFrame, csr_matrix]:
    data_train = df[df["timestamp"] < timestamp_to_split]
    val = df[df["timestamp"] >= timestamp_to_split]

    gt_val = val.groupby("user_id")[["product_id"]].agg(list).reset_index()
    gt_val["product_id"] = gt_val["product_id"].apply(remove_duplicates_order)

    timestamp_to_val_split = get_date_k_weeks_before(timestamp_to_split, k=2)
    data_val = data_train[data_train["timestamp"] >= timestamp_to_val_split]
    data_train = data_train[data_train["timestamp"] < timestamp_to_val_split].reset_index(drop=True)

    gt_val = gt_val[gt_val["user_id"].isin(set(data_val["user_id"]))].reset_index(drop=True)
    data_val = data_val[data_val["user_id"].isin(set(gt_val["user_id"]))].reset_index(drop=True)

    # Создаем бинарную матрицу
    user_product_matrix = create_user_product_matrix(data_train)

    return data_train, data_val, gt_val, user_product_matrix

# Измерение времени выполнения
start_time = timeit.default_timer()

# Чтение данных блоками с использованием pyarrow
df_train_reader = pq.ParquetFile(PATH_DATA + "train.parquet")
df_test = pq.ParquetFile(PATH_DATA + "test.parquet")

# Разбиваем чтение на блоки
chunk_size = 10000
user_product_matrix_list = []

for i in range(0, df_train_reader.num_row_groups, chunk_size):
    row_groups = list(range(i, min(i + chunk_size, df_train_reader.num_row_groups)))
    df_chunk = pd.concat([df_train_reader.read_row_group(j).to_pandas() for j in row_groups])
    user_product_matrix_list.append(create_user_product_matrix(df_chunk))

# Объединение матриц из блоков
user_product_matrix = user_product_matrix_list[0] if len(user_product_matrix_list) == 1 else csr_matrix(np.vstack([mat.toarray() for mat in user_product_matrix_list]))

# Очистка памяти
del user_product_matrix_list
gc.collect()

# Создание разреженной матрицы похожести пользователей
user_similarity = cosine_similarity(user_product_matrix, user_product_matrix)

# Очистка памяти
del user_product_matrix
gc.collect()

# Нормализация матрицы похожести
user_similarity_normalized = normalize(user_similarity, axis=1)

# Очистка памяти
del user_similarity
gc.collect()

# Для каждого пользователя предскажем топ-N товаров на основе схожести пользователей
top_n = 3  # Можно изменить
user_predictions = []

for i in range(user_similarity_normalized.shape[0]):
    similar_users = np.argsort(user_similarity_normalized[i, :])[::-1][1:top_n + 1]
    predicted_products = np.unique(np.concatenate([user_product_matrix_list[j][similar_users].toarray().flatten() for j in range(len(user_product_matrix_list))]))
    user_predictions.append(predicted_products)

# Очистка памяти
del user_similarity_normalized
gc.collect()

test_user_product_matrix = create_user_product_matrix(df_test.to_pandas())
test_user_similarity = cosine_similarity(test_user_product_matrix, user_product_matrix)

test_user_similarity_normalized = normalize(test_user_similarity, axis=1)

test_user_predictions = []

for i in range(test_user_similarity_normalized.shape[0]):
    similar_users = np.argsort(test_user_similarity_normalized[i, :])[::-1][1:top_n + 1]
    predicted_products = np.unique(np.concatenate([user_product_matrix_list[j][similar_users].toarray().flatten() for j in range(len(user_product_matrix_list))]))
    test_user_predictions.append(predicted_products)

df_submission = pd.DataFrame(df_test['Id'], test_user_predictions)
df_submission.to_csv('submission1.csv', index=False)
