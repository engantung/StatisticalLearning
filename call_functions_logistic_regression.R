#author : Edmond Ngantung
# 27/05/2020

# Define a function

Show_Data_Length_Actual<-function(Sample_Data)
{	
	# Count Total Data
	return(length(Sample_Data))
}

Show_Colapsed_Data<-function(Sample_Data01,Sample_Data02)
{	
	# Count Total Data
	for(i in 1:length(Sample_Data01))
	{
		cat("[",i,"]",Sample_Data01[i],",",Sample_Data02[i],"\n")
	}
}

Show_Colapsed_Data_Quadraple<-function(Starting,Finishing,Sample_Data01,Sample_Data02,Sample_Data03,Sample_Data04)
{	
	# Count Total Data
	for(i in Starting:Finishing)
	{
		cat("[",i,"]",Sample_Data01[i],",",Sample_Data02[i],",",Sample_Data03[i],",",Sample_Data04[i],"\n")
	}
}

Show_Data_Length_Limit_To_100<-function(Sample_Data)
{	
	# Control Total Data Limited to 100
	if(length(Sample_Data) >= 100)
	{
		return(100)
	}
	else
	{
		return(length(Sample_Data))
	}
}

Show_StartingData_For_Testing<-function(Sample_Data)
{	
	# Starting_Data_For_Testing
	if(length(Sample_Data) >= 100)
	{
		return(101)
	}
	else
	{
		return(1)
	}
}

Show_Data_Sample<-function(Sample_Data)
{	
	# Print Data
	return(Sample_Data)
}

Show_Data_Normalized<-function(Sample_Data)
{
	# Compute normalized data
	Sample_Data_sd = sd(Sample_Data,na.rm = TRUE) 
	Sample_Data_mean = mean(Sample_Data,na.rm = TRUE)
	Sample_Data_norm = (Sample_Data - Sample_Data_mean)/Sample_Data_sd
	return(Sample_Data_norm)
}

Show_Likelihood<-function(X_Normal,Y_Normal,Bo,Bi)
{
	# reset the likelihood
	  log_likelihood = 0.000
	# Compute the likelihood
	for(i in 1:Show_Data_Length_Actual(X_Normal))
	{
		log_likelihood_old = log_likelihood
		log_likelihood = log_likelihood + Y_Normal[i]*(Bo+Bi*X_Normal[i])-log(1+e^(Bo+Bi*X_Normal[i]))
		log_likelihood_new = log_likelihood
		cat("Data[", i,"], OLD_Likelihood: ", log_likelihood_old,", NEW_Likelihood: ", log_likelihood_new, ", DIFF_Likelihood = ", abs(log_likelihood_new - log_likelihood_old),"\n")
		trend_log_likelihood = list("data_likelihood" = i, "old_likelihood" = log_likelihood_old, "new_likelihood" = log_likelihood_new,"delta_likelihood" = abs(log_likelihood_new - log_likelihood_old),"result_likelihood" = log_likelihood)
	}
	return(trend_log_likelihood)
}

Show_Training_Process<-function(X_Normal,Y_Normal,learning_rate,Bo,Bi,error_tolerance)
{
	# reset the parameters
	gradient = 0.000
	counter = 0
	Bi_old = 0
	Bi_new = 1
	
	#Apply Iteration 
	while(abs(Bi_new - Bi_old) > error_tolerance)
	{
		Bi_old = Bi
		probability = e^(Bo+Bi*X_Normal)/(1+e^(Bo+Bi*X_Normal))
		
		#For Training, only using to First 100 data 
		for(i in 1:Show_Data_Length_Limit_To_100(X_Normal))
		{
			gradient = gradient + (Y_Normal[i]-probability[i])*X_Normal[i]
		}
		Bi = Bi + learning_rate * gradient
		Bi_new = Bi
		counter = counter + 1
		cat("ITERATION[",counter,"],Learning_rate = ", learning_rate,",Error_lowest = ", abs(Bi_new - Bi_old),",Bi_final = ", Bi, ",Gradient_final = ", gradient,"\n")
		if(abs(Bi_new - Bi_old) < error_tolerance)
		{
			cat("Learning_rate = ", learning_rate,"with ",counter,"times ITERATION,Error_lowest = ", abs(Bi_new - Bi_old),",Bi_final = ", Bi, ",Gradient_final = ", gradient,"\n")
			parameters_trained = list("alpha" = learning_rate, "epoch" = counter, "update_error" = abs(Bi_new - Bi_old),"Bi" = Bi,"grad" = gradient)
			return(parameters_trained) 		
		}
		
	}
}

Show_Training<-function(X_Normal,Y_Normal,learning_rate,Bo,Bi,error_tolerance)
{
	# reset the parameters
	gradient = 0.000
	counter = 0
	Bi_old = 0
	Bi_new = 1
	
	#Apply Iteration 
	while(abs(Bi_new - Bi_old) > error_tolerance)
	{
		Bi_old = Bi
		probability = e^(Bo+Bi*X_Normal)/(1+e^(Bo+Bi*X_Normal))
		
		#For Training, only using to First 100 data 
		for(i in 1:Show_Data_Length_Limit_To_100(X_Normal))
		{
			gradient = gradient + (Y_Normal[i]-probability[i])*X_Normal[i]
		}
		Bi = Bi + learning_rate * gradient
		Bi_new = Bi
		counter = counter + 1
		
		#cat("ITERATION[",counter,"],Learning_rate = ", learning_rate,",Error_lowest = ", abs(Bi_new - Bi_old),",Bi_final = ", Bi, ",Gradient_final = ", gradient,"\n")
		
		if(abs(Bi_new - Bi_old) < error_tolerance)
		{
			cat("Learning_rate = ", learning_rate,"with ",counter,"times ITERATION,Error_lowest = ", abs(Bi_new - Bi_old),",Bi_final = ", Bi, ",Gradient_final = ", gradient,"\n")
			parameters_trained = list("alpha" = learning_rate, "epoch" = counter, "update_error" = abs(Bi_new - Bi_old),"Bi" = Bi,"grad" = gradient)
			return(parameters_trained) 		
		}
	}
}

Show_Parameters_as_Updated_Error_Rates<-function(X_Normal,Y_Normal,learning_rate,Bo,Bi,error_tolerance)
{
	#Generate set of array to store the error rates produced from training

	error_initial = runif(Show_Data_Length_Actual(learning_rate)) 
		
	for(i in 1:Show_Data_Length_Actual(learning_rate))
	{
		error_initial[i] <- Show_Training(X_Normal,Y_Normal,learning_rate[i],Bo,Bi,error_tolerance)$update_error
	}

	return(error_initial)
}

Show_Best_Parameters<-function(Sample_Data01,Sample_Data02)
{
	#Generate set of array to store the error rates produced from training
	Sample_Data01 = runif(Show_Data_Length_Actual(Sample_Data02)) 
	# Determine the learning rate which produces the lowest error rate
	
	for(i in 1:Show_Data_Length_Actual(Sample_Data02))
	{
		if(Sample_Data01[i] == min(Sample_Data01))
		{
			min_location = i
		}
	}
	return(min_location)
}

Show_Testing<-function(X_Normal,Y_Normal,Bo,Bi)
{
	options(scipen=999) #eliminate the scientific number
	
	#Set the prediction
	Y_pred = Bo + Bi*X_Normal
	counter = 0
	
	#Data to Test from Data 101th to Data 462th or end of the Data
	Start_Testing = Show_StartingData_For_Testing(X_Normal)
	Finish_Testing = Show_Data_Length_Actual(X_Normal)
	total_data_for_testing = Finish_Testing-Start_Testing+1
	
	#Calculate the error percentage to compare the difference between Y_Pred and Y_Normal
	error_percentage = abs((Y_pred-Y_Normal)/Y_pred)*100
	acceptance = runif(total_data_for_testing)
	
	for(i in Start_Testing:Finish_Testing)
	{
		if(error_percentage[i] > 50)
		{
			acceptance[i] = 0
		}
		else
		{
			acceptance[i] = 1
			counter = counter + 1
		}
	}
	Show_Colapsed_Data_Quadraple(Start_Testing,Finish_Testing,Y_Normal,Y_pred,error_percentage,acceptance)
	cat("There are only = ",(counter/total_data_for_testing)*100,"% data accepted, which is only ",counter ,"out of", total_data_for_testing ,"data has lower than 50% accuracy\n")
}