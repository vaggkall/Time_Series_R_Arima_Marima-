dev.off()  # But only if there IS a plot

# Clear console
cat("\014")  # ctrl+L

## This empties the work space.
rm(list=ls())


# -------------- Importing data ----------------

#remove.packages(c("flexmix", "mltools","data.table"))

#install.packages("flexmix",dependencies = TRUE)
#install.packages("mltools",dependencies = TRUE)
#install.packages("data.table",dependencies = TRUE)


library(ggplot2)
library(rstudioapi)
library(forecast)
library(flexmix)
library(lmtest)
library(mltools)
library(data.table)

pathA=dirname(rstudioapi::getSourceEditorContext()$path)
raw_data  <- read.csv(paste(pathA, '/DataAssignment3.csv', sep=''), header=TRUE)

# Transformation of data
Y <- raw_data[,2]
mu <- log(Y)[1]
#Y_new <- raw_data[33:length(Y),2]
#mu_new <- log(Y_new)[1]

##################
# THIS PART BELOW WAS USED ONLY TO TRY DIFFERENT COMBINATIONS OF DATA INPUT AND
# OUTPUT, TO FIX OPTIMIZER BREAKING WHILE ESTIMATING ARIMA
##################

# Different data input to try
  # Log transformation of all data                          (1) - 102 measurements
  Y_transformed <- log(Y) - mu
  # Log transformation excl. first 32 datapoints            (2) - 70 measurements
  #Y_new_transformed <- log(Y_new) - mu_new
  # Differentiated log data                                 (3) - 101 measurements
  #Y_diff_transformed <- diff(Y_transformed, 1)
  # Diffetentiated log data excl. first 32 datapoints       (4) - 69 measurements
  #Y_new_diff_transformed <- diff(Y_new_transformed, 1)
  # Differentiated original data                            (5) - 101 measurements
  #Y_diff <- diff(Y, 1)
  # Differentiated original data excl. first 32 datapoints  (6) - 69 measurements
  #Y_new_diff <- diff(Y_new, 1)
  
X <- data.frame(raw_data[3])
#X_sh <- data.frame(raw_data[33:102,3])
#colnames(X_sh) <- c("Government")
X$Government <- as.factor(X$Government)
#X_sh$Government <- as.factor(X_sh$Government)
# Encoding political parties 
  # Whole dataset                                           (A) - 102 rows
  #X_encoded <- one_hot(as.data.table(X$Government))
  #X_encoded <- sapply(X_encoded, as.numeric)
  # Whole dataset 1 party as all zeros                      (B) - 102 rows
  X_encoded_drop <- one_hot(as.data.table(X$Government))
  X_encoded_drop <- sapply(X_encoded_drop, as.numeric)
  X_encoded_drop <- subset(X_encoded_drop, select = -c(V1_Independent))
  # 70 measurements                                         (C) - 70 rows
  #X_encoded_sh <- one_hot(as.data.table(X_sh$Government))
  #X_encoded_sh <- sapply(X_encoded_sh, as.numeric)
  # 70 measurements 1 party as all zeros                    (D) - 70 rows
  #X_encoded_sh_drop <- one_hot(as.data.table(X_sh$Government))
  #X_encoded_sh_drop <- sapply(X_encoded_sh_drop, as.numeric)
  #X_encoded_sh_drop <- subset(X_encoded_sh_drop, select = -c(V1_Venstre))

  
# ---------- Proper part -----------
  
####################################
# ONE COLUMN WAS DROPPED FROM ORIGINAL ONE-HOT ENCODING MATRIX, BECAUSE THE DROPPED ROW 
# PARTY CAN BE INPUT TO OUR MODEL BY GIVING ALL ZERO ENTRIES, AND THAT WAY WE PREVENTED 
# MULTICOLINEARITY WHICH APPARENTLY WAS BRAKING OPTIMIZER
####################################
  
Ys <- Y_transformed
Xs <- X_encoded_drop

# model estimation
model <- arima(Ys, order=c(1,1,1), xreg=Xs)
#model <- arima(Ys, order=c(1,1,1), xreg=Xs, method = 'CSS-ML')
#model <- arima(Ys, order=c(1,1,1), xreg=Xs, optim.method = "BFGS")

# printing coefficients and performing statistical test
model
coeftest(model)

# sample prediction for 1 time step and 2 time steps for Conservative party  

tests_1 <- data.frame(t(c(1, 0, 0, 0)))
tests_2 <- data.frame(t(cbind(c(1, 0, 0, 0), c(1, 0, 0, 0))))
output_1 <- predict(model, n.ahead=1, newxreg = tests_1)
output_2 <- predict(model, n.ahead=2, newxreg = tests_2)
output_1$pred
output_2$pred   # predictions, but logarithmic
output_1$se     # to confidence interval estimation (in code for parts 1-4 and Spyros code)
output_2$se

# fixing dates 
Dates <- raw_data[1]
Dates <- sapply(Dates,as.numeric)
new_dates <- 2021:2030
Dates <- append(Dates, new_dates)



#predictions for conservative Party
Conservative <- matrix(rep(t(cbind(1,0,0,0)),10),ncol=4,byrow=TRUE)
Conservtive_model_prediction <- predict(model, n.ahead=10, newxreg = Conservative)

conf = cbind(Conservtive_model_prediction$pred + qnorm(0.025)*Conservtive_model_prediction$se,Conservtive_model_prediction$pred - qnorm(0.025)*Conservtive_model_prediction$se)

Y_pred = exp(Conservtive_model_prediction$pred+mu)
conf_pred = exp(conf+mu)

Conservative_pred <- Y_pred


Y_new2 <- sapply(Y,as.numeric)

Y_final <- append(Y_new2,Y_pred)
Y_final <- cbind(Dates, Y_final)

Y_finaldf <- data.frame(Y_final)
Y_finaldf['Col'] <- 'Original data'
Y_finaldf[103:112,'Col']='Predictions'
Y_finaldf[103:112,'lower']=conf_pred[,1]
Y_finaldf[103:112,'upper']=conf_pred[,2]

ggplot(Y_finaldf, aes(x=Dates)) + 
  geom_point(aes(y=Y_final, color=Col), size=2) +
  geom_line(aes(y=lower, color='Prediction interval'), linetype = "dashed", size=1) + 
  geom_line(aes(y=upper, color='Prediction interval'), linetype = "dashed", size=1) +
  geom_ribbon(data=subset(Y_finaldf, 2021 <= Dates & Dates <= 2030), aes(ymin=lower,ymax=upper), fill="green", alpha=0.25) +
  theme(text = element_text(size = 16)) + 
  theme(panel.border = element_rect(color = "black",
                                    fill = NA,
                                    size = 1)) +
  labs(y='Number of Words') +
  labs(x='Year') +
  labs(color='Legend') 


# predictions for Social liberals
Social_Liberals <- matrix(rep(t(cbind(0,1,0,0)),10),ncol=4,byrow=TRUE)
Social_Liberals_prediction <- predict(model, n.ahead=10, newxreg = Social_Liberals)

conf = cbind(Social_Liberals_prediction$pred + qnorm(0.025)*Social_Liberals_prediction$se,Social_Liberals_prediction$pred - qnorm(0.025)*Social_Liberals_prediction$se)

Y_pred = exp(Conservtive_model_prediction$pred+mu)
conf_pred = exp(conf+mu)

Social_Liberals_pred <- Y_pred

Y_new2 <- sapply(Y,as.numeric)

Y_final <- append(Y_new2,Y_pred)
Y_final <- cbind(Dates, Y_final)

Y_finaldf <- data.frame(Y_final)
Y_finaldf['Col'] <- 'Original data'
Y_finaldf[103:112,'Col']='Predictions'
Y_finaldf[103:112,'lower']=conf_pred[,1]
Y_finaldf[103:112,'upper']=conf_pred[,2]

ggplot(Y_finaldf, aes(x=Dates)) + 
  geom_point(aes(y=Y_final, color=Col), size=2) +
  geom_line(aes(y=lower, color='Prediction interval'), linetype = "dashed", size=1) + 
  geom_line(aes(y=upper, color='Prediction interval'), linetype = "dashed", size=1) +
  geom_ribbon(data=subset(Y_finaldf, 2021 <= Dates & Dates <= 2030), aes(ymin=lower,ymax=upper), fill="green", alpha=0.25) +
  theme(text = element_text(size = 16)) + 
  theme(panel.border = element_rect(color = "black",
                                    fill = NA,
                                    size = 1)) +
  labs(y='Number of Words') +
  labs(x='Year') +
  labs(color='Legend') 


# Social Democrats predictions
Social_Democrats <- matrix(rep(t(cbind(0,0,1,0)),10),ncol=4,byrow=TRUE)
Social_Democrats_prediction <- predict(model, n.ahead=10, newxreg = Social_Democrats)

conf = cbind(Social_Democrats_prediction$pred + qnorm(0.025)*Social_Democrats_prediction$se,Social_Democrats_prediction$pred - qnorm(0.025)*Social_Democrats_prediction$se)

Y_pred = exp(Conservtive_model_prediction$pred+mu)
conf_pred = exp(conf+mu)

Social_Democrats_pred <- Y_pred

Y_new2 <- sapply(Y,as.numeric)

Y_final <- append(Y_new2,Y_pred)
Y_final <- cbind(Dates, Y_final)

Y_finaldf <- data.frame(Y_final)
Y_finaldf['Col'] <- 'Original data'
Y_finaldf[103:112,'Col']='Predictions'
Y_finaldf[103:112,'lower']=conf_pred[,1]
Y_finaldf[103:112,'upper']=conf_pred[,2]

ggplot(Y_finaldf, aes(x=Dates)) + 
  geom_point(aes(y=Y_final, color=Col), size=2) +
  geom_line(aes(y=lower, color='Prediction interval'), linetype = "dashed", size=1) + 
  geom_line(aes(y=upper, color='Prediction interval'), linetype = "dashed", size=1) +
  geom_ribbon(data=subset(Y_finaldf, 2021 <= Dates & Dates <= 2030), aes(ymin=lower,ymax=upper), fill="green", alpha=0.25) +
  theme(text = element_text(size = 16)) + 
  theme(panel.border = element_rect(color = "black",
                                    fill = NA,
                                    size = 1)) +
  labs(y='Number of Words') +
  labs(x='Year') +
  labs(color='Legend') 

# Venstre predictions
Venstre = matrix(rep(t(cbind(0,0,0,1)),10),ncol=4,byrow=TRUE)
Venstre_prediction <- predict(model, n.ahead=10, newxreg = Venstre)

conf = cbind(Venstre_prediction$pred + qnorm(0.025)*Venstre_prediction$se,Venstre_prediction$pred - qnorm(0.025)*Venstre_prediction$se)

Y_pred = exp(Conservtive_model_prediction$pred+mu)
conf_pred = exp(conf+mu)

Venstre_pred <- Y_pred

Y_new2 <- sapply(Y,as.numeric)

Y_final <- append(Y_new2,Y_pred)
Y_final <- cbind(Dates, Y_final)

Y_finaldf <- data.frame(Y_final)
Y_finaldf['Col'] <- 'Original data'
Y_finaldf[103:112,'Col']='Predictions'
Y_finaldf[103:112,'lower']=conf_pred[,1]
Y_finaldf[103:112,'upper']=conf_pred[,2]

Y_finaldf

ggplot(Y_finaldf, aes(x=Dates)) + 
  geom_point(aes(y=Y_final, color=Col), size=2) +
  geom_line(aes(y=lower, color='Prediction interval'), linetype = "dashed", size=1) + 
  geom_line(aes(y=upper, color='Prediction interval'), linetype = "dashed", size=1) +
  geom_ribbon(data=subset(Y_finaldf, 2021 <= Dates & Dates <= 2030), aes(ymin=lower,ymax=upper), fill="green", alpha=0.25) +
  theme(text = element_text(size = 16)) + 
  theme(panel.border = element_rect(color = "black",
                                    fill = NA,
                                    size = 1)) +
  labs(y='Number of Words') +
  labs(x='Year') +
  labs(color='Legend') 



Y_new2 <- sapply(Y,as.numeric)
typeof(Y_new2)
Y_new2 <- ts(Y_new2, start =1)
ts(Dates,start =1)

Y_Data <- data.frame(time = seq (1919,2020,length(102)),M=as.numeric(Y_new2),isin="data")
Venstre <- data.frame(time = seq (2021,2030,length(10)),M=as.numeric(Venstre_pred),isin="Venstre")
Social_Democrats <- data.frame(time = seq (2021,2030,length(10)),M=as.numeric(Social_Democrats_pred),isin="Social Democrats")
Social_Liberals <- data.frame(time = seq (2021,2030,length(10)),M=as.numeric(Social_Liberals_pred),isin="Social Liberals")
Conservative <- data.frame(time = seq (2021,2030,length(10)),M=as.numeric(Conservative_pred),isin="Conservative")

Y_finaldf <- cbind (Dates,Y_new2,Venstre_pred,Social_Democrats_pred,Social_Liberals_pred,Conservative_pred)
Y_finaldf


ggplot(Y_finaldf, aes(x =Dates)) +
  geom_point(aes(y= Y_new2, color = "green"), size=2)+
  geom_point(aes(y= Venstre, color = "blue"), size=2)+
  geom_point(aes(y= Social_Democrats, color = "red"), size=2)+
  geom_point(aes(y= Social_Liberals, color = "green"), size=2)+
  geom_point(aes(y= Conservative, color = "Yellow"), size=2)+
  theme(text = element_text(size = 16)) + 
  labs(y='Number of Words') +
  labs(x='Year') +
  labs(color='Legend') 


Y_finaldf <- rbind (Y_Data,Venstre,Social_Democrats,Social_Liberals,Conservative)
Y_finaldf

ggplot(Y_finaldf, aes(x = time, y = M, color = isin)) +
  geom_point()+
  theme(text = element_text(size = 16)) + 
  labs(y='Number of Words') +
  labs(x='Year') +
  labs(color='Legend') 
