#------------------------------------
#--------ML for rshiny---------------
#------------------------------------

# import iris dataset from sklearn
from sklearn import datasets
df = datasets.load_iris()
X = df.data
y = df.target

# splitting the dataset into the training set and test set
from sklearn.model_selection import train_test_split
X_train, X_test, y_train, y_test = train_test_split(X, y , test_size = 0.25, random_state = 0)

# feature scaling
from sklearn.preprocessing import StandardScaler
sc = StandardScaler()
X_train = sc.fit_transform(X_train)
X_test = sc.transform(X_test)

# training the logistic regression model on the training set
from sklearn.linear_model import LogisticRegression
classifier = LogisticRegression(random_state = 0)
classifier.fit(X_train, y_train)

# predicting the test set results
y_pred = classifier.predict(X_test)

# making the confusion matrix
from sklearn.metrics import confusion_matrix, accuracy_score
cm = confusion_matrix(y_test, y_pred)
#print(cm)
accuracy_score(y_test, y_pred)

# show the whole data
def show_dataframe():
    import pandas as pd
    df_dataframe = pd.DataFrame(df.data)
    df_dataframe.columns = df.feature_names
    df_dataframe["Target_variable"] = df.target
    return df_dataframe

# show accuracy
def show_accuracy():
    acc_score = accuracy_score(y_test, y_pred)
    return acc_score

# print(show_accuracy())

# to show the scatter we have to take X and Y
def return_x():
    x = X[:,1]
    return x
def return_y():
    x = X[:,2]
    return x








