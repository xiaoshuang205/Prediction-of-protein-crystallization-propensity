from sklearn.datasets import load_digits
from sklearn.model_selection import train_test_split
from sklearn.feature_selection import VarianceThreshold
from sklearn.metrics import accuracy_score
import pandas as pd
from sklearn import svm
import numpy as np
from sklearn import metrics
from sklearn.model_selection import cross_val_score
from  sklearn.metrics import roc_auc_score
from sklearn.metrics import matthews_corrcoef
from sklearn.discriminant_analysis import LinearDiscriminantAnalysis
from deepforest import CascadeForestClassifier
from sklearn.decomposition import PCA
# X, y = load_digits(return_X_y=True)#load data
from sklearn.feature_selection import SelectPercentile
from sklearn.feature_selection import chi2
from sklearn.preprocessing import StandardScaler
from sklearn.preprocessing import MinMaxScaler
from xgboost.sklearn import XGBClassifier
from sklearn.decomposition import PCA
from sklearn.metrics import confusion_matrix


#Chi2


#filepath1 = r"D:\EBAC\Semester2\final_project\数据\DCFCrystal_Dataset\BD_CRYS\CF_DS\trainfeaturedata.csv"
# check missing values
#data1 = pd.read_csv(filepath1)
#filepath2 = r"D:\EBAC\Semester2\final_project\finalmodel数据整理\CF2182\无PSSM\handlecombinedata.csv"
filepath2 = r"D:\EBAC\Semester2\final_project\数据\DCFCrystal_Dataset提过\BD_CRYS\带入文件汇总\pssmpfdata7388.csv"
alldata = pd.read_csv(filepath2)
#print(alldata)
#print(alldata.isnull().any().any())
#False no missing value
#alldata = alldata.replace(np.nan, 0)
#testX1 =alldata.dropna()
#print(alldata)
# print(np.isnan(testX1).any())
#X1 = data1.iloc[:, 2:]

# colNames = X1.columns
# y1 = data1.iloc[:, 1]
# print(y1)
#testX1 = testdata.iloc[:, 2:]

# # colNames = X1.columns
allX1=alldata.iloc[:,2:]
#allX1=VarianceThreshold(threshold=0.8).fit_transform(allX1)
# transfer=MinMaxScaler()
# allX1=transfer.fit_transform(allX1)
ally1=alldata.iloc[:,1]

# allX1 = SelectPercentile(chi2, 10).fit_transform(allX1, ally1)
ally1 = pd.DataFrame(ally1)
#trainy1 = ally1.iloc[:2182, :]
#testy1 = ally1.iloc[2182:, :]
trainy1 = ally1.iloc[:7388, :]
testy1 = ally1.iloc[7388:, :]
# print(trainy1)
# print(testy1)

# PCA

# pca1 = PCA(n_components=0.9)
# reduced_traindata = pca1.fit_transform(allX1)


#T-SNE(need less than 4)
# from sklearn.manifold import TSNE
# from pandas.core.frame import DataFrame
# import numpy as np
# pca_tsne = TSNE(n_components=3)
# reduced_traindata = pca_tsne.fit_transform(allX1)


# LDA
LDA = LinearDiscriminantAnalysis(n_components=1)
LDA.fit(allX1, ally1)
reduced_traindata = LDA.transform(allX1)
# n_components cannot be larger than min(n_features, n_classes - 1).
# print(ally1.shape[0])



# DCF
reduceddata = pd.DataFrame(reduced_traindata)
train = reduceddata.iloc[:7388, :]
reduced_traindata1 = train.values
test = reduceddata.iloc[7388:, :]
reduced_testdata1 = test.values
print("reducetrainshape")
print(reduced_traindata1.shape)
print("trainy1")
print(trainy1.shape)
from deepforest import CascadeForestClassifier
DCF_model = CascadeForestClassifier(random_state=1)
DCF_score = cross_val_score(DCF_model, reduced_traindata1, trainy1)
print("the cross validation accuracy of deep forest is :" + str(DCF_score.mean()))
DCF_model.fit(reduced_traindata1, trainy1)
# y_pred = model.predict(X_test)
dcf_pre = DCF_model.predict(reduced_testdata1)
tn1, fp1, fn1, tp1 = confusion_matrix(testy1, dcf_pre).ravel()
specificity1 = tn1 / (tn1+fp1)
sensitive1=tp1/ (tp1+ fn1)
print("dcf prediction specificity", specificity1)
print("dcf prediction sensitive", sensitive1)
print("dcf prediction Accuracy", accuracy_score(testy1, dcf_pre))
print("dcf auc:",roc_auc_score(testy1, dcf_pre))
print('DCF Mean Absolute Error:', metrics.mean_absolute_error(testy1, dcf_pre))
print('DCF Mean Squared Error:', metrics.mean_squared_error(testy1, dcf_pre))
print('DCF Root Mean Squared Error:', np.sqrt(metrics.mean_squared_error(testy1, dcf_pre)))
print('DCF Root Matthews correlation coefficient:', matthews_corrcoef(testy1, dcf_pre))
#
# # acc = accuracy_score(y_test, y_pred) * 100
# # print("\nTesting Accuracy: {:.3f} %".format(acc))
# # #PCA:Testing Accuracy: 73.077 %
# # #T-SNE:Testing Accuracy: 70.696 %
# # #LDA:Testing Accuracy: 70.696 %
#
# # 训练随机森林解决回归问题
from sklearn.ensemble import RandomForestClassifier

RF = RandomForestClassifier()
RF1 = RandomForestClassifier(n_estimators=50, random_state=0)
RF2 = RandomForestClassifier(n_estimators=150, random_state=0)
RF3 = RandomForestClassifier(n_estimators=300, random_state=0)
RF_score = cross_val_score(RF, reduced_traindata1, trainy1)
RF1_score = cross_val_score(RF1, reduced_traindata1, trainy1)
RF2_score = cross_val_score(RF2, reduced_traindata1, trainy1)
RF3_score = cross_val_score(RF3, reduced_traindata1, trainy1)
RF.fit(reduced_traindata1, trainy1)
RF1.fit(reduced_traindata1, trainy1)
RF2.fit(reduced_traindata1, trainy1)
RF3.fit(reduced_traindata1, trainy1)
print("the cross validation accuracy of Random forest is :" + str(RF_score.mean()))
print("the cross validation accuracy of Random forest1 is :" + str(RF1_score.mean()))
print("the cross validation accuracy of Random forest2 is :" + str(RF2_score.mean()))
print("the cross validation accuracy of Random forest3 is :" + str(RF3_score.mean()))
y_pre = RF.predict(reduced_testdata1)
y_pre1 = RF1.predict(reduced_testdata1)
y_pre2 = RF2.predict(reduced_testdata1)
y_pre3 = RF3.predict(reduced_testdata1)
rftn, rffp, rffn, rftp = confusion_matrix(testy1, y_pre).ravel()
rfspecificity = rftn / (rftn+rffp)
rfsensitive=rftp/ (rftp+ rffn)
print("rf prediction specificity", rfspecificity)
print("rf prediction sensitive", rfsensitive)
rftn1, rffp1, rffn1, rftp1 = confusion_matrix(testy1, y_pre1).ravel()
rfspecificity1 = rftn1 / (rftn1+rffp1)
rfsensitive1=rftp1/ (rftp1+ rffn1)
print("rf1 prediction specificity", rfspecificity1)
print("rf1 prediction sensitive", rfsensitive1)
rftn2, rffp2, rffn2, rftp2 = confusion_matrix(testy1, y_pre2).ravel()
rfspecificity2 = rftn2 / (rftn2+rffp2)
rfsensitive2=rftp2/ (rftp2+ rffn2)
print("rf2 prediction specificity", rfspecificity2)
print("rf2 prediction sensitive", rfsensitive2)
rftn3, rffp3, rffn3, rftp3 = confusion_matrix(testy1, y_pre3).ravel()
rfspecificity3 = rftn3 / (rftn3+rffp3)
rfsensitive3=rftp3/ (rftp3+ rffn3)
print("rf3 prediction specificity", rfspecificity3)
print("rf3 prediction sensitive", rfsensitive3)
print("rf prediction Accuracy", accuracy_score(testy1, y_pre))
print("rf1 prediction Accuracy", accuracy_score(testy1, y_pre1))
print("rf2 prediction Accuracy", accuracy_score(testy1, y_pre2))
print("rf3 prediction Accuracy", accuracy_score(testy1, y_pre3))
print("rf auc:",roc_auc_score(testy1, y_pre))
print("rf1 auc:",roc_auc_score(testy1, y_pre1))
print("rf2 auc:",roc_auc_score(testy1, y_pre2))
print("rf3 auc:",roc_auc_score(testy1, y_pre3))
print('rf Root Matthews correlation coefficient:', matthews_corrcoef(testy1, y_pre))
print('rf1 Root Matthews correlation coefficient:', matthews_corrcoef(testy1, y_pre1))
print('rf2 Root Matthews correlation coefficient:', matthews_corrcoef(testy1, y_pre2))
print('rf3 Root Matthews correlation coefficient:', matthews_corrcoef(testy1, y_pre3))
# 评估回归性能
from sklearn import metrics

print('RF Mean Absolute Error:', metrics.mean_absolute_error(testy1, y_pre))
print('RF1 Mean Absolute Error:', metrics.mean_absolute_error(testy1, y_pre1))
print('RF2 Mean Absolute Error:', metrics.mean_absolute_error(testy1, y_pre2))
print('RF3 Mean Absolute Error:', metrics.mean_absolute_error(testy1, y_pre3))
print('RF Mean Squared Error:', metrics.mean_squared_error(testy1, y_pre))
print('RF1 Mean Squared Error:', metrics.mean_squared_error(testy1, y_pre1))
print('RF2 Mean Squared Error:', metrics.mean_squared_error(testy1, y_pre2))
print('RF3 Mean Squared Error:', metrics.mean_squared_error(testy1, y_pre3))
print('RF Root Mean Squared Error:', np.sqrt(metrics.mean_squared_error(testy1, y_pre)))
print('RF1 Root Mean Squared Error:', np.sqrt(metrics.mean_squared_error(testy1, y_pre1)))
print('RF2 Root Mean Squared Error:', np.sqrt(metrics.mean_squared_error(testy1, y_pre2)))
print('RF3 Root Mean Squared Error:', np.sqrt(metrics.mean_squared_error(testy1, y_pre3)))

# SVM
from sklearn.svm import SVC
def select_c_function(i):
    svm_model = SVC(kernel='rbf',C=i)

    recall_score = cross_val_score(svm_model, reduced_traindata1, trainy1.values.ravel(), scoring='recall', cv=10)
    return recall_score.mean()
c_range = [3,5,7,10,15,18,20,30,40,60]
for i in c_range:
    avg_score = select_c_function(i)
    print('When C is{}，the mean recall score of K-fold cross validation{}'.format(i, avg_score))
def select_c_function2(i):
    svm_model = SVC(kernel='rbf',C=i)
    recall_score = cross_val_score(svm_model, reduced_traindata1, trainy1)
    return recall_score.mean()

c_range = [3,5,7,10,15,18,20,30,40,60]
for i in c_range:
    avg_score = select_c_function2(i)
    print('When C is{}，the mean accurracy score of K-fold cross validation{}'.format(i, avg_score))

SVM_model = svm.SVC(kernel='rbf',C=7)
SVM_score = cross_val_score(SVM_model, reduced_traindata1, trainy1)
print("the cross validation accuracy of SVM is :" + str(SVM_score.mean()))
SVM_model.fit(reduced_traindata1, trainy1)
# y_pred = model.predict(X_test)
SVM_pre = SVM_model.predict(reduced_testdata1)
svmtn, svmfp, svmfn, svmtp = confusion_matrix(testy1, SVM_pre).ravel()
svmspecificity = svmtn / (svmtn+svmfp)
svmsensitive=svmtp/ (svmtp+ svmfn)
print("svm prediction specificity", svmspecificity)
print("svm prediction sensitive", svmsensitive)
print("svm prediction Accuracy", accuracy_score(testy1, SVM_pre))
print("svm auc:",roc_auc_score(testy1, SVM_pre))
print('SVM Mean Absolute Error:', metrics.mean_absolute_error(testy1, SVM_pre))
print('SVM Mean Squared Error:', metrics.mean_squared_error(testy1, SVM_pre))
print('SVM Root Mean Squared Error:', np.sqrt(metrics.mean_squared_error(testy1, SVM_pre)))
print('SVM Root Matthews correlation coefficient:', matthews_corrcoef(testy1, SVM_pre))

