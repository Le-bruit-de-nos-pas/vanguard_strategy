from keras.models import Sequential
from keras.layers import Dense
from keras.wrappers.scikit_learn import KerasClassifier
from keras.utils import np_utils

from sklearn.model_selection import cross_val_score
from sklearn.model_selection import KFold
from sklearn.preprocessing import LabelEncoder
from sklearn.pipeline import Pipeline
from sklearn.preprocessing import MinMaxScaler
from sklearn.model_selection import train_test_split

from joblib import dump, load

import pandas as pd
import numpy as np
df = pd.read_csv('HF_stages.txt', sep=",")
df = df.dropna()
df.head()
for col in df.columns:
    print(col)
#  df = df.iloc[:, 0:160]
for col in df.columns:
    print(col)
df['AStages'] = df['AStages'].round(0)
    
df['AStages'].unique()
df['AStages'] = df['AStages'].astype(object)
def prepareY(df):
  Y = df["AStages"]
  yencoder = LabelEncoder()
  yencoder.fit(Y)
  dump(yencoder,"yencoder.joblib")
  return yencoder.transform(Y)
y = prepareY(df)
df = df.drop(["AStages"], axis=1)
pd.DataFrame(y).head()
round(
    pd.DataFrame(y).value_counts()/(2042+1085+826+223)*100
    )
df = df.drop(['Apatient'], axis=1)
# from sklearn.compose import ColumnTransformer
# from sklearn.preprocessing import OrdinalEncoder
# from sklearn.preprocessing import MinMaxScaler

# numerical_ix = df.select_dtypes(include=['int64', 'float64']).columns
# categorical_ix = df.select_dtypes(include=['object', 'bool']).columns

# column_trans = ColumnTransformer(
#    [('cat', OrdinalEncoder(),categorical_ix),
#     ('num', MinMaxScaler(feature_range=(-1, 1)), numerical_ix)],
#     remainder='drop')
# column_trans.fit(df)
# dump(column_trans,"column_trans.joblib")
# X = column_trans.transform(df)
# pd.DataFrame(X).head()
X_train, X_test, y_train, y_test = train_test_split(df, y, test_size=0.3, random_state=42)
yhot = np_utils.to_categorical(y)
yhot_train = np_utils.to_categorical(y_train)
yhot_test = np_utils.to_categorical(y_test)
# define baseline model
def baseline_model():
  # create model
  model = Sequential()
  
  # Rectified Linear Unit Activation Function
  model.add(Dense(2050, input_dim=1025, activation='relu'))
  model.add(Dense(2050, activation = 'relu'))
  # Softmax for multi-class classification
  model.add(Dense(4, activation='softmax'))
  # Compile model
  model.compile(loss='categorical_crossentropy', optimizer='adam', metrics=['accuracy'])
  return model
cmodel = KerasClassifier(build_fn=baseline_model, epochs=200, batch_size=100, verbose=0)
kfold = KFold(n_splits=10, shuffle=True)
# result = cross_val_score(cmodel, df, yhot, cv=kfold)

# print("Result: %.2f%% (%.2f%%)" % (result.mean()*100, result.std()*100))
model = baseline_model()
model.compile(loss='categorical_crossentropy', optimizer='adam', metrics=['accuracy'])
history = model.fit(X_train, yhot_train, validation_split=0.33, epochs=200, batch_size=100, verbose=0)
import matplotlib.pyplot as plt

# list all data in history
print(history.history.keys())

# summarize history for accuracy
plt.plot(history.history['accuracy'])
plt.plot(history.history['val_accuracy'])
plt.title('model accuracy')
plt.ylabel('accuracy')
plt.xlabel('epoch')
plt.legend(['train', 'test'], loc='upper left')
plt.show()

# summarize history for loss
plt.plot(history.history['loss'])
plt.plot(history.history['val_loss'])
plt.title('model loss')
plt.ylabel('loss')
plt.xlabel('epoch')
plt.legend(['train', 'test'], loc='upper left')
plt.show()
# evaluate the keras model
_, accuracy = model.evaluate(X_test, yhot_test)
print('Accuracy from evaluate: %.2f' % (accuracy*100))
predict_x = model.predict(X_test)
pred = np.argmax(predict_x, axis=1)
print(f'Prediction Accuracy: {(pred == y_test).mean() * 100:f}')
model_json = model.to_json()

with open("customermodel.json", "w") as json_file:
  json_file.write(model_json)

# serialize weights to HDF5
model.save_weights("model.h5")
print("Saved model to disk")
from keras.models import model_from_json

# load json and create model
json_file = open('customermodel.json', 'r')
loaded_model_json = json_file.read()
json_file.close()
loaded_model = model_from_json(loaded_model_json)

# load weights into new model
loaded_model.load_weights("model.h5")
print("Loaded model from disk")

# evaluate loaded model on test data
loaded_model.compile(loss='categorical_crossentropy', optimizer='adam', metrics=['accuracy'])
loaded_model.summary()
predict_x = loaded_model.predict(df)
pred = np.argmax(predict_x, axis=1)
print(f'Prediction Accuracy: {(pred == y).mean() * 100:f}')
pd.DataFrame(predict_x)
