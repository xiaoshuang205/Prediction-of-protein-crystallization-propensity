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
import numpy as np
from xgboost.sklearn import XGBClassifier
from sklearn.decomposition import PCA
#Chi2
filepath1=r"D:\EBAC\Semester2\final_project\code\feature extraction\feature extraction\CF_TR_Feature\CF_TR_Feature\cf_Moran .csv"
# check missing values
data1=pd.read_csv(filepath1)
corr_matrix1=data1.corr().abs()
upper1=corr_matrix1.where(np.triu(np.ones(corr_matrix1.shape),k=1).astype(np.bool))
to_drop1=[column for column in upper1.columns if any(upper1[column]>0.85)]
data1.drop(to_drop1,axis=1,inplace=True)
#print(data.isnull().any().any())
#False no missing value
#Remove features with low variance

X1=data1.iloc[:,2:]
all_name1 = X1.columns.values.tolist()
tansfer=StandardScaler()
transfer=MinMaxScaler()
X1=tansfer.fit_transform(X1)
X1=transfer.fit_transform(X1)
#print(X.shape)
y1=data1.iloc[:,1]
#feature selection data
SP1=SelectPercentile(chi2, 5)
X1_new = SP1.fit_transform(X1, y1)
New_Xy1 =pd.concat((y1,pd.DataFrame(X1_new)),axis=1)#feature selection data
#new-fortest
select_name_index1 = SP1.get_support(indices=True)
select_name1 = []
for i in select_name_index1:
    select_name1.append(all_name1[i])
# print(test1[[select_name]])
testpath1=r"D:\EBAC\Semester2\final_project\code\feature extraction\feature extraction\CF_TE_Feature\t_cf_Moran .csv"
test1=pd.read_csv(testpath1)
T1=test1.iloc[:,2:]
Ty1=test1.iloc[:,1]
testdata1=T1[select_name1]
Testdata1 =pd.concat((Ty1,pd.DataFrame(testdata1)),axis=1)

trainname=data1.iloc[:,0]
testname=test1.iloc[:,0]
New_Xy1 =pd.concat((trainname,New_Xy1),axis=1)
Testdata1 =pd.concat((testname,Testdata1),axis=1)
#type of select_name is list
# print(New_Xy1.shape)
# print(Testdata1.shape)

#Lasso
from sklearn.linear_model import LassoCV
filepath2=r"D:\EBAC\Semester2\final_project\code\feature extraction\feature extraction\CF_TR_Feature\CF_TR_Feature\CF_TR_GAAC_Ilearn.csv"
# check missing values
data2=pd.read_csv(filepath2)
corr_matrix2=data2.corr().abs()
upper2=corr_matrix2.where(np.triu(np.ones(corr_matrix2.shape),k=1).astype(np.bool))
to_drop2=[column for column in upper2.columns if any(upper2[column]>0.85)]
data2.drop(to_drop2,axis=1,inplace=True)
X2=data2.iloc[:,2:]
colNames = X2.columns
all_name2 = X2.columns.values.tolist()
y2=data2.iloc[:,1]
X2=tansfer.fit_transform(X2)
X2=transfer.fit_transform(X2)
X2=pd.DataFrame(X2)
X2.columns = colNames
model_lassoCV = LassoCV( cv=10, max_iter=100000).fit(X2, y2)  # cv, cross-validation
coef = pd.Series(model_lassoCV.coef_,index = X2.columns) #new knowledge
# print(coef)
print("Lasso picked " + str(sum(coef != 0)) + " variables and eliminated the other " + str(sum(coef == 0)))
index = coef[coef != 0].index
K=index.tolist()
testpath2=r"D:\EBAC\Semester2\final_project\code\feature extraction\feature extraction\CF_TE_Feature\CF_TE_GAAC_Ilearn.csv"
test2=pd.read_csv(testpath2)
T2=test2.iloc[:,2:]
testdata2=T2[K]
K.append('label added')
Lassodata=data2.loc[:,K]
X_Lasso=Lassodata.iloc[:,0:-1]
print(X_Lasso.shape)
y_Lasso=Lassodata.iloc[:,-1]
New_Xy2 =pd.concat((New_Xy1,pd.DataFrame(X_Lasso)),axis=1)
Testdata2 =pd.concat((Testdata1,pd.DataFrame(testdata2)),axis=1)
New_Xy2.to_csv("trainLASS0data.csv")
Testdata2.to_csv('testLASSOdata.csv')






















#X,y
# X_chi2=New_Xy.iloc[:,0:-1]
# y_chi2=New_Xy.iloc[:,-1]
filepath2=r"D:\EBAC\Semester2\final_project\code\feature extraction\feature extraction\CF_TR_Feature\CF_TR_GAAC_Ilearn.csv"
# check missing values
data2=pd.read_csv(filepath2)
corr_matrix2=data2.corr().abs()
upper2=corr_matrix2.where(np.triu(np.ones(corr_matrix2.shape),k=1).astype(np.bool))
to_drop2=[column for column in upper2.columns if any(upper2[column]>0.85)]
data2.drop(to_drop2,axis=1,inplace=True)
X2=data2.iloc[:,2:]
all_name2 = X2.columns.values.tolist()
y2=data2.iloc[:,1]
X2=tansfer.fit_transform(X2)
X2=transfer.fit_transform(X2)
SP2=SelectPercentile(chi2, 5)
X2_new = SP2.fit_transform(X2, y2)
New_Xy2 =pd.concat((New_Xy1,pd.DataFrame(X2_new)),axis=1)#feature selection data
select_name_index2 = SP2.get_support(indices=True)
select_name2 = []
for i in select_name_index2:
    select_name2.append(all_name2[i])
# print(test1[[select_name]])
testpath2=r"D:\EBAC\Semester2\final_project\code\feature extraction\feature extraction\CF_TE_Feature\CF_TE_GAAC_Ilearn.csv"
test2=pd.read_csv(testpath2)
T2=test2.iloc[:,2:]
testdata2=T2[select_name2]
Testdata2 =pd.concat((Testdata1,pd.DataFrame(testdata2)),axis=1)
print("traintest2 shape")
print(New_Xy2.shape)
print(Testdata2.shape)



filepath3=r"D:\EBAC\Semester2\final_project\code\feature extraction\feature extraction\CF_TR_Feature\CF_TR_CTraid_Ilearn.csv"
# check missing values
data3=pd.read_csv(filepath3)
corr_matrix3=data3.corr().abs()
upper3=corr_matrix3.where(np.triu(np.ones(corr_matrix3.shape),k=1).astype(np.bool))
to_drop3=[column for column in upper3.columns if any(upper3[column]>0.85)]
data3.drop(to_drop3,axis=1,inplace=True)
X3=data3.iloc[:,2:]
all_name3 = X3.columns.values.tolist()
y3=data3.iloc[:,1]
X3=tansfer.fit_transform(X3)
X3=transfer.fit_transform(X3)
SP3=SelectPercentile(chi2, 5)
X3_new = SP3.fit_transform(X3, y3)
New_Xy3 =pd.concat((New_Xy2,pd.DataFrame(X3_new)),axis=1)#feature selection data
select_name_index3 = SP3.get_support(indices=True)
select_name3 = []
for i in select_name_index3:
    select_name3.append(all_name3[i])
# print(test1[[select_name]])
testpath3=r"D:\EBAC\Semester2\final_project\code\feature extraction\feature extraction\CF_TE_Feature\CF_TE_CTriad_Ilearn.csv"
test3=pd.read_csv(testpath3)
T3=test3.iloc[:,2:]
testdata3=T3[select_name3]
Testdata3 =pd.concat((Testdata2,pd.DataFrame(testdata3)),axis=1)
print("traintest3 shape")
print(New_Xy3.shape)
print(Testdata3.shape)


filepath4=r"D:\EBAC\Semester2\final_project\code\feature extraction\feature extraction\CF_TR_Feature\CF_TR_QSOrder_Ilearn.csv"
# check missing values
data4=pd.read_csv(filepath4)
corr_matrix4=data4.corr().abs()
upper4=corr_matrix4.where(np.triu(np.ones(corr_matrix4.shape),k=1).astype(np.bool))
to_drop4=[column for column in upper4.columns if any(upper4[column]>0.85)]
data4.drop(to_drop4,axis=1,inplace=True)
X4=data4.iloc[:,2:]
all_name4 = X4.columns.values.tolist()
y4=data4.iloc[:,1]
X4=tansfer.fit_transform(X4)
X4=transfer.fit_transform(X4)
SP4=SelectPercentile(chi2, 50)
X4_new =SP4.fit_transform(X4, y4)
New_Xy4 =pd.concat((New_Xy3,pd.DataFrame(X4_new)),axis=1)#feature selection data
select_name_index4 = SP4.get_support(indices=True)
select_name4 = []
for i in select_name_index4:
    select_name4.append(all_name4[i])
# print(test1[[select_name]])
testpath4=r"D:\EBAC\Semester2\final_project\code\feature extraction\feature extraction\CF_TE_Feature\CF_TE_QSOrder_Ilearn.csv"
test4=pd.read_csv(testpath4)
T4=test4.iloc[:,2:]
testdata4=T4[select_name4]
Testdata4 =pd.concat((Testdata3,pd.DataFrame(testdata4)),axis=1)
print("traintest4 shape")
print(New_Xy4.shape)
print(Testdata4.shape)


filepath5=r"D:\EBAC\Semester2\final_project\code\feature extraction\feature extraction\CF_TR_Feature\CF_TR_QSOrder_Ilearn.csv"
# check missing values
data5=pd.read_csv(filepath5)
corr_matrix5=data5.corr().abs()
upper5=corr_matrix5.where(np.triu(np.ones(corr_matrix5.shape),k=1).astype(np.bool))
to_drop5=[column for column in upper5.columns if any(upper5[column]>0.85)]
data5.drop(to_drop5,axis=1,inplace=True)
X5=data5.iloc[:,2:]
all_name5 = X5.columns.values.tolist()
y5=data5.iloc[:,1]
X5=tansfer.fit_transform(X5)
X5=transfer.fit_transform(X5)
SP5=SelectPercentile(chi2, 30)
X5_new = SP5.fit_transform(X5, y5)
New_Xy5 =pd.concat((New_Xy4,pd.DataFrame(X5_new)),axis=1)#feature selection data
select_name_index5 = SP5.get_support(indices=True)
select_name5 = []
for i in select_name_index5:
    select_name5.append(all_name5[i])
# print(test1[[select_name]])
testpath5=r"D:\EBAC\Semester2\final_project\code\feature extraction\feature extraction\CF_TE_Feature\CF_TE_QSOrder_Ilearn.csv"
test5=pd.read_csv(testpath5)
T5=test5.iloc[:,2:]
testdata5=T5[select_name5]
Testdata5 =pd.concat((Testdata4,pd.DataFrame(testdata5)),axis=1)
print("traintest5 shape")
print(New_Xy5.shape)
print(Testdata5.shape)





filepath6=r"D:\EBAC\Semester2\final_project\code\feature extraction\feature extraction\CF_TR_Feature\CF_TR_APAAC_Ilearn.csv"
# check missing values
data6=pd.read_csv(filepath6)
corr_matrix6=data6.corr().abs()
upper6=corr_matrix6.where(np.triu(np.ones(corr_matrix6.shape),k=1).astype(np.bool))
to_drop6=[column for column in upper6.columns if any(upper6[column]>0.85)]
data6.drop(to_drop6,axis=1,inplace=True)
X6=data6.iloc[:,2:]
all_name6 = X6.columns.values.tolist()
y6=data6.iloc[:,1]
X6=tansfer.fit_transform(X6)
X6=transfer.fit_transform(X6)
SP6=SelectPercentile(chi2, 50)
X6_new = SP6.fit_transform(X6, y6)
New_Xy6 =pd.concat((New_Xy5,pd.DataFrame(X6_new)),axis=1)#feature selection data
select_name_index6 = SP6.get_support(indices=True)
select_name6 = []
for i in select_name_index6:
    select_name6.append(all_name6[i])
# print(test1[[select_name]])
testpath6=r"D:\EBAC\Semester2\final_project\code\feature extraction\feature extraction\CF_TE_Feature\CF_TE_APAAC_Ilearn.csv"
test6=pd.read_csv(testpath6)
T6=test6.iloc[:,2:]
testdata6=T6[select_name6]
Testdata6 =pd.concat((Testdata5,pd.DataFrame(testdata6)),axis=1)
print("traintest6 shape")
print(New_Xy6.shape)
print(Testdata6.shape)





#recursive feature elimination（RFE）----SVC
from sklearn.svm import SVC
from sklearn.feature_selection import RFE
svc=SVC(kernel="linear",C=1)
filepath7=r"D:\EBAC\Semester2\final_project\code\feature extraction\feature extraction\CF_TR_Feature\CF_TR_KSCTraid_Ilearn.csv"
data7=pd.read_csv(filepath7)
corr_matrix7=data7.corr().abs()
upper7=corr_matrix7.where(np.triu(np.ones(corr_matrix7.shape),k=1).astype(np.bool))
to_drop7=[column for column in upper7.columns if any(upper7[column]>0.85)]
data7.drop(to_drop7,axis=1,inplace=True)
X7=data7.iloc[:,2:]
all_name7 = X7.columns.values.tolist()
y7=data7.iloc[:,1]
X7=tansfer.fit_transform(X7)
X7=transfer.fit_transform(X7)
#colNames = X7.columns
column=X7.shape[1]
percentage=10
actualcolumn=int((percentage/100)*column)#RFE use
RFE7=RFE(estimator=svc,n_features_to_select=actualcolumn)
rfe7=RFE7.fit_transform(X7,y7)
New_Xy7 =pd.concat((New_Xy6,pd.DataFrame(rfe7)),axis=1)
select_name_index7 = RFE7.get_support(indices=True)
select_name7 = []
for i in select_name_index7:
    select_name7.append(all_name7[i])
# print(test1[[select_name]])
testpath7=r"D:\EBAC\Semester2\final_project\code\feature extraction\feature extraction\CF_TE_Feature\CF_TE_KSCTriad_Ilearn.csv"
test7=pd.read_csv(testpath7)
T7=test7.iloc[:,2:]
testdata7=T7[select_name7]
Testdata7 =pd.concat((Testdata6,pd.DataFrame(testdata7)),axis=1)
print("traintest7 shape")
print(New_Xy7.shape)
print(Testdata7.shape)



filepath8=r"D:\EBAC\Semester2\final_project\code\feature extraction\feature extraction\CF_TR_Feature\CF_TR_CKSAAP_Ilearn.csv"
data8=pd.read_csv(filepath8)
corr_matrix8=data8.corr().abs()
upper8=corr_matrix8.where(np.triu(np.ones(corr_matrix8.shape),k=1).astype(np.bool))
to_drop8=[column for column in upper8.columns if any(upper8[column]>0.85)]
data8.drop(to_drop8,axis=1,inplace=True)
X8=data8.iloc[:,2:]
all_name8 = X8.columns.values.tolist()
y8=data8.iloc[:,1]
X8=tansfer.fit_transform(X8)
X8=transfer.fit_transform(X8)
column=X8.shape[1]
percentage=10
actualcolumn=int((percentage/100)*column)#RFE use
RFE8=RFE(estimator=svc,n_features_to_select=actualcolumn)
rfe8=RFE8.fit_transform(X8,y8)
New_Xy8 =pd.concat((New_Xy7,pd.DataFrame(rfe8)),axis=1)
select_name_index8 = RFE8.get_support(indices=True)
select_name8 = []
for i in select_name_index8:
    select_name8.append(all_name8[i])
# print(test1[[select_name]])
testpath8=r"D:\EBAC\Semester2\final_project\code\feature extraction\feature extraction\CF_TE_Feature\CF_TE_KSCTriad_Ilearn.csv"
test8=pd.read_csv(testpath8)
T8=test8.iloc[:,2:]
testdata8=T8[select_name8]
Testdata8 =pd.concat((Testdata7,pd.DataFrame(testdata8)),axis=1)
print("traintest8 shape")
print(New_Xy8.shape)
print(Testdata8.shape)




filepath9=r"D:\EBAC\Semester2\final_project\code\feature extraction\feature extraction\CF_TR_Feature\CF_TR_DDE_Ilearn.csv"
data9=pd.read_csv(filepath9)
corr_matrix9=data9.corr().abs()
upper9=corr_matrix9.where(np.triu(np.ones(corr_matrix9.shape),k=1).astype(np.bool))
to_drop9=[column for column in upper9.columns if any(upper9[column]>0.85)]
data9.drop(to_drop9,axis=1,inplace=True)
X9=data9.iloc[:,2:]
all_name9 = X9.columns.values.tolist()
y9=data9.iloc[:,1]
X9=tansfer.fit_transform(X9)
X9=transfer.fit_transform(X9)
column=X9.shape[1]
percentage=20
actualcolumn=int((percentage/100)*column)#RFE use
RFE9=RFE(estimator=svc,n_features_to_select=actualcolumn)
rfe9=RFE9.fit_transform(X9,y9)
New_Xy9 =pd.concat((New_Xy8,pd.DataFrame(rfe9)),axis=1)
select_name_index9 = RFE9.get_support(indices=True)
select_name9 = []
for i in select_name_index9:
    select_name9.append(all_name9[i])
# print(test1[[select_name]])
testpath9=r"D:\EBAC\Semester2\final_project\code\feature extraction\feature extraction\CF_TE_Feature\CF_TE_KSCTriad_Ilearn.csv"
test9=pd.read_csv(testpath9)
T9=test9.iloc[:,2:]
testdata9=T9[select_name9]
Testdata9 =pd.concat((Testdata8,pd.DataFrame(testdata9)),axis=1)
print("traintest7 shape")
print(New_Xy9.shape)
print(Testdata9.shape)

filepath10=r"D:\EBAC\Semester2\final_project\code\feature extraction\feature extraction\CF_TR_Feature\CF_TR_DPC_Ilearn.csv"
data10=pd.read_csv(filepath10)
corr_matrix10=data10.corr().abs()
upper10=corr_matrix10.where(np.triu(np.ones(corr_matrix10.shape),k=1).astype(np.bool))
to_drop10=[column for column in upper10.columns if any(upper10[column]>0.85)]
data10.drop(to_drop10,axis=1,inplace=True)
X10=data10.iloc[:,2:]
all_name10 = X10.columns.values.tolist()
y10=data10.iloc[:,1]
X10=tansfer.fit_transform(X10)
X10=transfer.fit_transform(X10)
column=X10.shape[1]
percentage=20
actualcolumn=int((percentage/100)*column)#RFE use
RFE10=RFE(estimator=svc,n_features_to_select=actualcolumn)
rfe10=RFE10.fit_transform(X10,y10)
New_Xy10 =pd.concat((New_Xy9,pd.DataFrame(rfe10)),axis=1)
select_name_index10 = RFE10.get_support(indices=True)
select_name10 = []
for i in select_name_index10:
    select_name10.append(all_name10[i])
# print(test1[[select_name]])
testpath10=r"D:\EBAC\Semester2\final_project\code\feature extraction\feature extraction\CF_TE_Feature\CF_TE_KSCTriad_Ilearn.csv"
test10=pd.read_csv(testpath10)
T10=test10.iloc[:,2:]
testdata10=T10[select_name10]
Testdata10=pd.concat((Testdata9,pd.DataFrame(testdata10)),axis=1)
print("traintest10 shape")
print(New_Xy10.shape)
print(Testdata10.shape)
New_Xy10.to_csv('trainfeaturedata.csv')
Testdata10.to_csv('testfeaturedata.csv')