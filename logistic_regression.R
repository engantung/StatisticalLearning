#author : Edmond Ngantung
# 27/05/2020

#Set the initials, error tolerance and the learning rates
e = 2.781
B0 = 0.2
B1 = 0.3
tolerance = 0.01 
alpha <- c(0.01, 0.02, 0.03, 0.04, 0.05, 0.06, 0.07, 0.08, 0.09, 0.1, 0.11)

#Call functions To Receive Sample Datas and Solve the Project
#source("C:\\Users\\LENOVO\\Desktop\\Summer Project 2020\\MP02-Madjid Alili\\Project\\call_functions_logistic_regression.R",echo=TRUE)
source("C:\\Users\\LENOVO\\Documents\\Desktop Files\\myBU Document\\2020 - 4th Semester (Summer)\\MP02-Madjid Alili\\Project\\call_functions_logistic_regression.R",echo=TRUE)

#Receive Data Sample : X_sample and Y_sample
#data<-read.csv(file="C:\\Users\\LENOVO\\Desktop\\Summer Project 2020\\MP02-Madjid Alili\\Project\\SAheart.data",head=TRUE,sep=",")
data<-read.csv(file="C:\\Users\\LENOVO\\Documents\\Desktop Files\\myBU Document\\2020 - 4th Semester (Summer)\\MP02-Madjid Alili\\Project\\SAheart.data",head=TRUE,sep=",")
X_sample = Show_Data_Sample(data$ldl)
Y_sample = Show_Data_Sample(data$chd)
#data<-read.csv(file="C:\\Users\\LENOVO\\Desktop\\Summer Project 2020\\MP02-Madjid Alili\\Project\\sampledata.csv",head=TRUE,sep=",")
#X_sample = Show_Data_Sample(data$X)
#Y_sample = Show_Data_Sample(data$Y)

#Normalized Sample Data 
X_normal = Show_Data_Normalized(X_sample)
Y_normal = Show_Data_Normalized(Y_sample)
Show_Colapsed_Data(X_normal,Y_normal)

#Calculate Value of Objective function
Objective_Value = Show_Likelihood(X_normal,Y_normal,B0,B1)
log_likelihood = Objective_Value$result_likelihood
cat("Value_of_Objective_Function = ",log_likelihood,"\n")

#Apply Training and Perform the Best Parameters 
Error_Rates = Show_Parameters_as_Updated_Error_Rates(X_normal,Y_normal,alpha,B0,B1,tolerance)
the_best = Show_Best_Parameters(Error_Rates,alpha)
cat("The Best learning rates = ",alpha[the_best],"\n")
Training_Result = Show_Training_Process(X_normal,Y_normal,alpha[the_best],B0,B1,tolerance)
B1_update = Training_Result$Bi
cat("B1 = ",B1_update,"\n")

#Apply Testing based on the best equation below
cat("Y = ",B0, " + ",B1_update,"X","\n","\n")
Testing_Result = Show_Testing(X_normal,Y_normal,B0,B1_update)
