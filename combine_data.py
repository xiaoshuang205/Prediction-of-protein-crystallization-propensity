from sklearn import feature_selection
import numpy as np
import pandas as pd
from sklearn.linear_model import LogisticRegression
from sklearn.model_selection import train_test_split
from sklearn.metrics import accuracy_score
from sklearn.feature_selection import SelectPercentile
from sklearn.feature_selection import chi2
from sklearn.preprocessing import StandardScaler
from sklearn.preprocessing import MinMaxScaler
from xgboost.sklearn import XGBClassifier
from sklearn.decomposition import PCA
filepath=r"D:\EBAC\Semester2\final_project\code\pssm\pssm\CF_TR\CFTR_PSSM\DP_PSSM.csv"
# check missing values
data=pd.read_csv(filepath)
#data.dropna(inplace=True)
data.fillna(0,inplace=True)
print(data.isnull().any().any())
#False no missing value

#Remove features with low variance
X=data.iloc[:,2:]
colNames = X.columns
column=X.shape[1]
percentage=30
actualcolumn=int((percentage/100)*column)
tansfer=StandardScaler()
transfer=MinMaxScaler()
X=tansfer.fit_transform(X)
X=transfer.fit_transform(X)
print(X.shape)
y=data.iloc[:,1]

#logistic regression to predict use raw data
# split X and y
X_train, X_test, y_train, y_test = train_test_split(X, y, test_size=0.2, random_state=2)
logreg = LogisticRegression()
logreg.fit(X_train, y_train.values.reshape(-1))
y_pred = logreg.predict(X_test)
# print('在测试数据集上面的预测准确率: {:.2f}'.format(logreg.score(X_test, y_test)))
print("raw data accuracy is %2.3f" % accuracy_score(y_test, y_pred))
#raw data 结果为0.732(0.2,2)

sel = feature_selection.VarianceThreshold()
train_variance = sel.fit_transform(X)
print(train_variance.shape)
#Remove highly correlated features
corr_matrix = data.corr().abs()
#print(corr_matrix['label added'].sort_values(ascending=False).head(10))
#the feature CHAM820101.lag27 has the highest correlation to the target,with a correlation value of only 0.0875
#it is only weakly correlated

#feature selection of chi2
X_new = SelectPercentile(chi2, percentage).fit_transform(X, y)
New_Xy =pd.concat((pd.DataFrame(X_new),y),axis=1)

X_chi2=New_Xy.iloc[:,0:-1]
X_chi2.fillna(0,inplace=True)
print(X_chi2.isnull().any().any())
print("chi2 shape")
print(X_chi2.shape)
y_chi2=New_Xy.iloc[:,-1]
#print(y_chi2)

X_train, X_test, y_train, y_test = train_test_split(X_chi2, y_chi2, test_size=0.2, random_state=2)
logreg = LogisticRegression()
logreg.fit(X_train, y_train.values.reshape(-1))
y_pred = logreg.predict(X_test)
# print('在测试数据集上面的预测准确率: {:.2f}'.format(logreg.score(X_test, y_test)))
print("chi2 accuracy is %2.3f" % accuracy_score(y_test, y_pred))
#0.751

#recursive feature elimination（RFE）----SVC
# from sklearn.svm import SVC
# from sklearn.feature_selection import RFE
# svc=SVC(kernel="linear",C=1)
# rfe=RFE(estimator=svc,n_features_to_select=actualcolumn).fit_transform(X,y)
# RFE_selection =pd.concat((pd.DataFrame(rfe),y),axis=1)
#
# X_rfe=RFE_selection.iloc[:,0:-1]
# print("rfe shape")
# print(X_rfe.shape)
# y_rfe=RFE_selection.iloc[:,-1]
# #print(y_chi2)
#
# XR_train, XR_test, yR_train, yR_test = train_test_split(X_rfe, y_rfe, test_size=0.2, random_state=2)
# logreg = LogisticRegression()
# logreg.fit(XR_train, yR_train.values.reshape(-1))
# yR_pred = logreg.predict(XR_test)
# # print('在测试数据集上面的预测准确率: {:.2f}'.format(logreg.score(XR_test, yR_test)))
# print("RFE accuracy is %2.3f" % accuracy_score(yR_test, yR_pred))

#xgboost
from numpy import loadtxt
from numpy import sort
from xgboost import XGBClassifier
from xgboost import XGBRegressor
from sklearn.model_selection import train_test_split
from sklearn.metrics import accuracy_score
from sklearn.feature_selection import SelectFromModel

# split data into train and test sets

# fit model on all training data
# instantiate the model
model = XGBClassifier(max_depth=3)
model.fit(X, y)
selection = SelectFromModel(model).fit_transform(X,y)
#selection=selection.transform(X,y)
#print(selection.shape)
New_selection =pd.concat((pd.DataFrame(selection),y),axis=1)

# (200, 3)
X_xgboost=New_selection.iloc[:,0:-1]
print("xgboost shape")
print(X_xgboost.shape)
y_xgboost=New_selection.iloc[:,-1]
#print(y_chi2)

XG_train, XG_test, yG_train, yG_test = train_test_split(X_xgboost, y_xgboost, test_size=0.2, random_state=2)
logreg = LogisticRegression()
logreg.fit(XG_train, yG_train.values.reshape(-1))
yG_pred = logreg.predict(XG_test)
# print('在测试数据集上面的预测准确率: {:.2f}'.format(logreg.score(XG_test, yG_test)))
print("xgboost accuracy is %2.3f" % accuracy_score(yG_test, yG_pred))


#Lasso
from sklearn.linear_model import LassoCV
# LASSO method
X=pd.DataFrame(X)
X.columns = colNames
#alphas = np.logspace(-3, 1, 50)
#print(alphas)
#model_lassoCV = LassoCV(alphas=alphas, cv=10, max_iter=100000).fit(X, y)  # cv, cross-validation
model_lassoCV = LassoCV( cv=10, max_iter=100000).fit(X, y)  # cv, cross-validation
coef = pd.Series(model_lassoCV.coef_,index = X.columns) #new knowledge
# print(coef)
print("Lasso picked " + str(sum(coef != 0)) + " variables and eliminated the other " + str(sum(coef == 0)))
index = coef[coef != 0].index
K=index.tolist()
K.append('label added')
Lassodata=data.loc[:,K]
#X = X[index]
#print(Lassodata)
#K=pd.DataFrame(coef[coef != 0])
X_Lasso=Lassodata.iloc[:,0:-1]
print(X_Lasso.shape)
y_Lasso=Lassodata.iloc[:,-1]
#print(y_Lasso)
XLA_train, XLA_test, yLA_train, yLA_test = train_test_split(X_Lasso, y_Lasso, test_size=0.2, random_state=2)
logreg = LogisticRegression()
logreg.fit(XLA_train, yLA_train.values.reshape(-1))
yLA_pred = logreg.predict(XLA_test)
# print('在测试数据集上面的预测准确率: {:.2f}'.format(logreg.score(XLA_test, yLA_test)))
print("Lasso accuracy is %2.3f" % accuracy_score(yLA_test, yLA_pred))

from sklearn.datasets import load_digits
from sklearn.model_selection import train_test_split
from sklearn.metrics import accuracy_score
import pandas as pd
from deepforest import CascadeForestClassifier
from sklearn.decomposition import PCA
X, y = load_digits(return_X_y=True)#load data
from sklearn.feature_selection import SelectPercentile
from sklearn.feature_selection import chi2
from sklearn.preprocessing import StandardScaler
from sklearn.preprocessing import MinMaxScaler
from xgboost.sklearn import XGBClassifier
from sklearn.decomposition import PCA
#Chi2
filepath1=r"D:\EBAC\Semester2\final_project\code\feature extraction\feature extraction\CF_TR_Feature\cf_Moran .csv"
# check missing values
data1=pd.read_csv(filepath1)
#print(data.isnull().any().any())
#False no missing value
#Remove features with low variance
X1=data1.iloc[:,2:]
tansfer=StandardScaler()
transfer=MinMaxScaler()
X1=tansfer.fit_transform(X1)
X1=transfer.fit_transform(X1)
#print(X.shape)
y1=data1.iloc[:,1]
#feature selection data
X1_new = SelectPercentile(chi2, 5).fit_transform(X1, y1)
New_Xy1 =pd.concat((y1,pd.DataFrame(X1_new)),axis=1)#feature selection data
#print(New_Xy1.shape)
#X,y
# X_chi2=New_Xy.iloc[:,0:-1]
# y_chi2=New_Xy.iloc[:,-1]
# print("chi2 shape")
# print(X_chi2.shape)
filepath2=r"D:\EBAC\Semester2\final_project\code\feature extraction\feature extraction\CF_TR_Feature\CF_TR_GAAC_Ilearn.csv"
# check missing values
data2=pd.read_csv(filepath2)
X2=data2.iloc[:,3:]
y2=data2.iloc[:,1]
X2=tansfer.fit_transform(X2)
X2=transfer.fit_transform(X2)
X2_new = SelectPercentile(chi2, 50).fit_transform(X2, y2)
New_Xy2 =pd.concat((New_Xy1,pd.DataFrame(X2_new)),axis=1)#feature selection data
# print("New_Xy2.shape")
# print(New_Xy2.shape)
filepath3=r"D:\EBAC\Semester2\final_project\code\feature extraction\feature extraction\CF_TR_Feature\CF_TR_CTraid_Ilearn.csv"
# check missing values
data3=pd.read_csv(filepath3)
X3=data3.iloc[:,3:]
y3=data3.iloc[:,1]
X3=tansfer.fit_transform(X3)
X3=transfer.fit_transform(X3)
X3_new = SelectPercentile(chi2, 5).fit_transform(X3, y3)
New_Xy3 =pd.concat((New_Xy2,pd.DataFrame(X3_new)),axis=1)#feature selection data
# print("New_Xy3.shape")
# print(New_Xy3.shape)
filepath4=r"D:\EBAC\Semester2\final_project\code\feature extraction\feature extraction\CF_TR_Feature\CF_TR_QSOrder_Ilearn.csv"
# check missing values
data4=pd.read_csv(filepath4)
X4=data4.iloc[:,3:]
y4=data4.iloc[:,1]
X4=tansfer.fit_transform(X4)
X4=transfer.fit_transform(X4)
X4_new = SelectPercentile(chi2, 50).fit_transform(X4, y4)
New_Xy4 =pd.concat((New_Xy3,pd.DataFrame(X4_new)),axis=1)#feature selection data
print("New_Xy4.shape")
print(New_Xy4.shape)
filepath5=r"D:\EBAC\Semester2\final_project\code\feature extraction\feature extraction\CF_TR_Feature\CF_TR_QSOrder_Ilearn.csv"
# check missing values
data5=pd.read_csv(filepath5)
X5=data5.iloc[:,3:]
y5=data5.iloc[:,1]
X5=tansfer.fit_transform(X5)
X5=transfer.fit_transform(X5)
X5_new = SelectPercentile(chi2, 30).fit_transform(X5, y5)
New_Xy5 =pd.concat((New_Xy4,pd.DataFrame(X5_new)),axis=1)#feature selection data
# print("New_Xy5.shape")
# print(New_Xy5.shape)

filepath6=r"D:\EBAC\Semester2\final_project\code\feature extraction\feature extraction\CF_TR_Feature\CF_TR_APAAC_Ilearn.csv"
# check missing values
data6=pd.read_csv(filepath6)
#print("data6.shape")
#print(data6.shape)
#print(data6)
X6=data6.iloc[:,3:]
y6=data6.iloc[:,1]
print("y6.shape")
print(y6)
X6=tansfer.fit_transform(X6)
X6=transfer.fit_transform(X6)
X6_new = SelectPercentile(chi2, 50).fit_transform(X6, y6)
New_Xy6 =pd.concat((New_Xy5,pd.DataFrame(X6_new)),axis=1)#feature selection data
print("New_Xy6.shape")
print(New_Xy6.shape)
#print(New_Xy6.type)#DataFrame

recursive feature elimination（RFE）----SVC
from sklearn.svm import SVC
from sklearn.feature_selection import RFE
svc=SVC(kernel="linear",C=1)
filepath7=r"D:\EBAC\Semester2\final_project\code\feature extraction\feature extraction\CF_TR_Feature\CF_TR_KSCTraid_Ilearn.csv"
data7=pd.read_csv(filepath7)
X7=data7.iloc[:,2:]
y7=data7.iloc[:,1]
X7=tansfer.fit_transform(X7)
X7=transfer.fit_transform(X7)
#colNames = X7.columns
column=X7.shape[1]
percentage=10
actualcolumn=int((percentage/100)*column)#RFE use
rfe7=RFE(estimator=svc,n_features_to_select=actualcolumn).fit_transform(X7,y7)
New_Xy7 =pd.concat((New_Xy4,pd.DataFrame(rfe7)),axis=1)

filepath8=r"D:\EBAC\Semester2\final_project\code\feature extraction\feature extraction\CF_TR_Feature\CF_TR_CKSAAP_Ilearn.csv"
data8=pd.read_csv(filepath8)
X8=data8.iloc[:,2:]
y8=data8.iloc[:,1]
X8=tansfer.fit_transform(X8)
X8=transfer.fit_transform(X8)
column=X8.shape[1]
percentage=10
actualcolumn=int((percentage/100)*column)#RFE use
rfe8=RFE(estimator=svc,n_features_to_select=actualcolumn).fit_transform(X8,y8)
New_Xy8 =pd.concat((New_Xy7,pd.DataFrame(rfe8)),axis=1)

filepath9=r"D:\EBAC\Semester2\final_project\code\feature extraction\feature extraction\CF_TR_Feature\CF_TR_DDE_Ilearn.csv"
data9=pd.read_csv(filepath9)
X9=data9.iloc[:,2:]
y9=data9.iloc[:,1]
X9=tansfer.fit_transform(X9)
X9=transfer.fit_transform(X9)
column=X9.shape[1]
percentage=20
actualcolumn=int((percentage/100)*column)#RFE use
rfe9=RFE(estimator=svc,n_features_to_select=actualcolumn).fit_transform(X9,y9)
New_Xy9 =pd.concat((New_Xy8,pd.DataFrame(rfe9)),axis=1)

filepath10=r"D:\EBAC\Semester2\final_project\code\feature extraction\feature extraction\CF_TR_Feature\CF_TR_DPC_Ilearn.csv"
data10=pd.read_csv(filepath10)
X10=data10.iloc[:,2:]
y10=data10.iloc[:,1]
X10=tansfer.fit_transform(X10)
X10=transfer.fit_transform(X10)
column=X10.shape[1]
percentage=20
actualcolumn=int((percentage/100)*column)#RFE use
rfe10=RFE(estimator=svc,n_features_to_select=actualcolumn).fit_transform(X10,y10)
New_Xy10 =pd.concat((New_Xy9,pd.DataFrame(rfe10)),axis=1)
New_Xy10.to_csv('aftercleandata.csv')
print("New_Xy10.shape")
print(New_Xy10.shape)
y1=y1.value
print(y1.type)#y1Series
beforedimensionX=New_Xy4.iloc[:,1:]
beforedimensiony=New_Xy4.iloc[:,0]
# print("beforedimensiony")
# print(beforedimensiony)
#PCA
pca1 = PCA(n_components=0.9)
#pca = PCA(n_components=0.9)# 保证降维后的数据保持90%的信息
reduced_data1 = pca1.fit_transform(beforedimensionX)

print("reduced_data1")
print(reduced_data1)
print(reduced_data1.type)#numpy.ndarray
T-SNE(need less than 4)
from sklearn.datasets import load_digits
from sklearn.model_selection import train_test_split
from sklearn.metrics import accuracy_score
import pandas as pd
from deepforest import CascadeForestClassifier
from sklearn.decomposition import PCA
X, y = load_digits(return_X_y=True)#load data
from sklearn.feature_selection import SelectPercentile
from sklearn.feature_selection import chi2
from sklearn.preprocessing import StandardScaler
from sklearn.preprocessing import MinMaxScaler
from xgboost.sklearn import XGBClassifier
from sklearn.decomposition import PCA
#Chi2
filepath1=r"D:\EBAC\Semester2\final_project\数据\DCFCrystal_Dataset\BD_CRYS\CF_DS\combinecleandata.csv"
# check missing values
data1=pd.read_csv(filepath1)
#print(data.isnull().any().any())
#False no missing value
#Remove features with low variance
X1=data1.iloc[:,2:]
colNames = X1.columns
# tansfer=StandardScaler()
# transfer=MinMaxScaler()
# X1=tansfer.fit_transform(X1)
# X1=transfer.fit_transform(X1)
#print(X.shape)
y1=data1.iloc[:,1]
#feature selection data
#chi2
# X1_new = SelectPercentile(chi2, 30).fit_transform(X1, y1)
# New_Xy1 =pd.concat((y1,pd.DataFrame(X1_new)),axis=1)#feature selection data

# print("New_Xy10.shape")
# print(New_Xy10.shape)
#y1=y1.value
#print(y1.type)#y1Series
# beforedimensionX=New_Xy1.iloc[:,1:]
# beforedimensiony=New_Xy1.iloc[:,0]
# print("beforedimensiony")
# print(beforedimensiony)
#PCA
# pca1 = PCA(n_components=0.9)
# #pca = PCA(n_components=0.9)# 保证降维后的数据保持90%的信息
# reduced_data1 = pca1.fit_transform(X1)

#T-SNE(need less than 4)
# from sklearn.manifold import TSNE
# from pandas.core.frame import DataFrame
# import numpy as np
# pca_tsne = TSNE(n_components=3)
# reduced_data1 = pca_tsne.fit_transform(X1)
#data1 = DataFrame(newMat)
# print("newMat")
# print(newMat)
# print(newMat.type)
#X，y all numpy.ndarray

# #LDA
from sklearn.discriminant_analysis import LinearDiscriminantAnalysis
LDA=LinearDiscriminantAnalysis(n_components=1)
LDA.fit(X1,y1)
reduced_data1=LDA.transform(X1)
# print("ldadata")
# print(ldadata)
# print(ldadata.type)
#n_components cannot be larger than min(n_features, n_classes - 1).



#DCF
from deepforest import CascadeForestClassifier
X_train, X_test, y_train, y_test = train_test_split(reduced_data1, y1, random_state=1)
model = CascadeForestClassifier(random_state=1)
model.fit(X_train, y_train)
y_pred = model.predict(X_test)
acc = accuracy_score(y_test, y_pred) * 100
print("\nTesting Accuracy: {:.3f} %".format(acc))
#PCA:Testing Accuracy: 73.077 %
#T-SNE:Testing Accuracy: 70.696 %
#LDA:Testing Accuracy: 70.696 %




