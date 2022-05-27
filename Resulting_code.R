dev.off()  # But only if there IS a plot

# Clear console
cat("\014")  # ctrl+L

## This empties the work space.
rm(list=ls())

# -------------- Settings --------------

training_years <- c(1850, 2013)
test_years <- c(2014, 2018)
lambda <- 0.8

# -------------- Imports --------------

library(ggplot2)
library(rstudioapi)
pathA=dirname(rstudioapi::getSourceEditorContext()$path)
raw_data <- read.csv(paste(pathA, '/A1_annual.txt', sep=''), header=TRUE, sep='\t')
raw_data <- raw_data[c('year', 'nh')]
training_set <- raw_data[which(raw_data$year==training_years[1]):which(raw_data$year==training_years[2]),]
test_set <- raw_data[which(raw_data$year==test_years[1]):which(raw_data$year==test_years[2]),]
raw_data['Purpose'] <- vector()
raw_data[which(raw_data$year==training_years[1]):which(raw_data$year==training_years[2]),'Purpose']<-'training'
raw_data[which(raw_data$year==test_years[1]):which(raw_data$year==test_years[2]),'Purpose']<-'test'

# -------------- Question 1.1 --------------

ggplot(raw_data, aes(x=year, y=nh, color=Purpose)) + geom_point()

# -------------- Question 1.2 calculations --------------

# Defining linear function and L matrix
f <- function(j) rbind(1, j)
L=matrix(c(1,1,0,1), ncol=2, nrow=2)
LInv=solve(L)

# Vector for Time Series estimated values, confidence interval and errors
yhat <- matrix(NA, nrow=nrow(raw_data))
errors <- matrix(NA, nrow=nrow(raw_data))
theta.all <- matrix(NA,ncol=2, nrow=nrow(raw_data))
pred_int <- matrix(NA, ncol=2, nrow=nrow(raw_data))

# Creating x and Y matrix for first time series iteration
x <- matrix(c(1, 1, 1, -2, -1, 0), ncol=2, nrow=3)
Y <- training_set[1:3, 2]

# F and h matrices for first iteration
F <- t(x) %*% x
h <- t(x) %*% Y

# First solution
theta <- solve(F, h)
theta.all[3,] <- theta
yhat[4] <- t(f(1)) %*% theta      
errors[4] <- yhat[4]-raw_data[4,2]
epsilon = Y - x %*% theta
sigma2 = (t(epsilon)%*%epsilon)/(4-2)
variance <- sigma2*(1+(t(f(1)) %*% solve(F) %*% f(1)))
#variance <- (t(Y - x %*% theta) %*% (Y - x %*% theta)/(4 - 2))*(1+(t(f(1)) %*% solve(F) %*% f(1)))
pred_int[4,] <- c(yhat[4]-qt(p=0.95,df=4-2)*sqrt(variance),yhat[4]+qt(p=0.95,df=4-2)*sqrt(variance))


# Loop for getting all solutions for training set
for (i in 4:nrow(training_set)){
  Y <- training_set[1:i,2]
  x <- rbind(c(1,-i+1), x)
  F <- F + f(-(i-1)) %*% t(f(-(i-1)))
  h <- LInv %*% h + f(0)*training_set[i,2]
  theta <- solve(F, h)
  theta.all[i,] <- theta
    if (i<nrow(training_set)){
      yhat[i+1] <- t(f(1)) %*% theta
      errors[i+1] <- yhat[i+1]-raw_data[i+1,2]
      epsilon = Y - x %*% theta
      sigma2 = (t(epsilon)%*%epsilon)/(i-2)
      variance <- sigma2*(1+(t(f(1)) %*% solve(F) %*% f(1)))
      pred_int[i+1,] <- c(yhat[i+1]-qt(p=0.95,df=i-2)*sqrt(variance),yhat[i+1]+qt(p=0.95,df=i-2)*sqrt(variance))
    }
}
  
# Calculating values for test data
init <- which(raw_data$year==test_years[1])
for (i in 1:nrow(test_set)){
  yhat[init-1+i] <- t(f(i)) %*% theta
  errors[init-1+i] <- yhat[init-1+i]-test_set[i,2]
  epsilon = raw_data[1:which(raw_data$year==training_years[2])+i,2] - x %*% theta
  sigma2 = (t(epsilon) %*%epsilon)/(nrow(training_set) - 2)
  variance <- sigma2*(1+(t(f(i)) %*% solve(F) %*% f(i)))
  pred_int[init-1+i,] <- c(yhat[init+i-1]-qt(p=0.975,df=nrow(training_set)-2)*sqrt(variance),yhat[init+i-1]+qt(p=0.975,df=nrow(training_set)-2)*sqrt(variance)) 
}

# Joining all data into 1 dataframe for plots
raw_data$yhat <- yhat
raw_data$errors <- errors
raw_data <- cbind(raw_data, pred_int)
names(raw_data)[6:7]<-c('lower', 'upper')

# -------------- Question 1.2 plots -------------- 

train_data <- raw_data[1:which(raw_data$year==training_years[2]),]
colors <- c('Prediction interval'='black', 'Estimated values'='darkred', 'Original data'='steelblue','Test data'='green')
ggplot(train_data, aes(x=year,y=nh)) + 
  geom_line(aes(y=yhat, color='Estimated values'), size=2) + 
  geom_line(aes(y=lower, color='Prediction interval'), linetype = "dashed") + 
  geom_line(aes(y=upper, color='Prediction interval'), linetype = "dashed") + 
  #geom_line(aes(y=errors, color='Errors')) + 
  geom_point(aes(y=nh, color='Original data')) +
  labs(x = "Year",
       y = "nh",
       color = "") +
  scale_color_manual(values = colors)

ggplot(train_data,aes(x=year)) +
  geom_point(aes(y=errors), colour="green", size = 3) +
  labs(x = "Year",
     y = "",
     color = "")

test_data <- raw_data[which(raw_data$year==test_years[1]):which(raw_data$year==test_years[2]),]
ggplot(test_data, aes(x=year,y=nh)) + 
  geom_point(aes(y=yhat, color='Estimated values'), size=2) + 
  geom_line(aes(y=lower, color='Prediction interval'), linetype = "dashed") + 
  geom_line(aes(y=upper, color='Prediction interval'), linetype = "dashed") + 
  geom_point(aes(y=nh, color='Original data')) +
  labs(x = "Year",
       y = "nh",
       color = "Legend") +
  scale_color_manual(values = colors)

ggplot(test_data,aes(x=year)) +
  geom_point(aes(y=errors), colour="green", size = 6)+
  labs(x = "Year",
       y = "",
       color = "")

Whole_data <- rbind(train_data,test_data)
Whole_data

ggplot(Whole_data, aes(x=year,y=nh)) + 
  geom_point(aes(y=yhat, color=ifelse(Purpose== 'test','Test data','Estimated values'))) + 
  geom_line(aes(y=lower, color=ifelse(Purpose== 'test','Test data','Estimated values')), linetype = "dashed") + 
  geom_line(aes(y=upper, color=ifelse(Purpose== 'test','Test data','Estimated values')), linetype = "dashed") + 
  geom_point(aes(y=nh, color='Original data')) +
  labs(x = "Year",
       y = "nh",
       color = "Legend") +
  scale_color_manual(values = colors)

# -------------- Question 1.3 calculations --------------

# Vector for Time Series estimated values, confidence interval and errors
yhat_loc <- matrix(NA, nrow=nrow(raw_data))
errors_loc <- matrix(NA, nrow=nrow(raw_data))
pred_int_loc <- matrix(NA, ncol=2, nrow=nrow(raw_data))
pred_int_loc_new <- matrix(NA, ncol=2, nrow=nrow(test_set))

# Creating x and Y matrix for first time series iteration
x <- matrix(c(1, 1, 1, -2, -1, 0), ncol=2, nrow=3)
Y <- training_set[1:3, 2]
E <- diag(c(1/(lambda^2), 1/lambda, 1), 3,3)

# F and h matrices for first iteration
F <- t(x) %*% solve(E) %*% x
h <- t(x) %*% solve(E) %*% Y

# First solution
theta <- solve(F, h)              
yhat_loc[4] <- t(f(1)) %*% theta      
errors_loc[4] <- yhat_loc[4]-raw_data[4,2]
T <- 1 + lambda + lambda^2 
diagonal <- c(1/lambda^2, 1/lambda^1, 1/lambda^0)
sigma <- diag(diagonal, 3, 3)
variance <- (t(Y - x %*% theta) %*% solve(sigma) %*% (Y - x %*% theta)/(T - 2))*(1+(t(f(1)) %*% solve(F) %*% f(1)))
pred_int_loc[4,] <- c(yhat[4]-qt(p=0.95,df=4-2)*sqrt(variance),yhat[4]+qt(p=0.95,df=4-2)*sqrt(variance))

#Allocate memory
N<-length(training_set[,2])
n<-3
sigma_new <- diag(N-n)

train_idx = nrow(training_set)

# Loop for getting all solutions for training set
for (i in 4:train_idx){
  sigma_new[i-n,i-n] <- 1+t(f(1))%*%solve(F)%*%f(1)
  F <- F + lambda^(i-1) * f(-(i-1)) %*% t(f(-(i-1)))
  h <- lambda * LInv %*% h + f(0)*training_set[i,2]
  x <- rbind(c(1,-i+1), x)
  theta <- solve(F, h)
  T <- T + lambda^(i-1)
  diagonal <- c(1/lambda^(i-1), diagonal)
  sigmainv <- diag(1/diagonal, i, i)
  if (i<nrow(training_set)){
    yhat_loc[i+1] <- t(f(1)) %*% theta
    errors_loc[i+1] <- yhat_loc[i+1]-raw_data[i+1,2]
    variance <- (t(training_set[1:i,2] - x %*% theta) %*% sigmainv %*% (training_set[1:i,2] - x %*% theta)/(T - 2))*(1+(t(f(1)) %*% solve(F) %*% f(1)))
    pred_int_loc[i+1,] <- c(yhat_loc[i+1]-qt(p=0.95,df=i-2)*sqrt(variance),yhat_loc[i+1]+qt(p=0.95,df=i-2)*sqrt(variance))
  }
}

sigma_new_inv <- solve(sigma_new)

init <- which(raw_data$year==test_years[1])
for (i in 1:nrow(test_set)){
  yhat_loc[init-1+i] <- t(f(i)) %*% theta
  errors_loc[init-1+i] <- yhat_loc[init-1+i]-test_set[i,2]
  variance <- (t(training_set[1:train_idx,2] - x %*% theta) %*% sigmainv %*% (training_set[1:train_idx,2] - x %*% theta)/(T - 2))*(1+(t(f(i)) %*% solve(F) %*% f(i)))
  variance_new <- (t(training_set[4:train_idx,2] - x[4:train_idx,] %*% theta) %*% sigma_new_inv %*% (training_set[4:train_idx,2] - x[4:train_idx,] %*% theta)/(N - n))*(1+(t(f(i)) %*% solve(F) %*% f(i)))
  pred_int_loc[init-1+i,] <- c(yhat_loc[init+i-1]-qt(p=0.975,df=nrow(training_set)-2)*sqrt(variance),yhat_loc[init+i-1]+qt(p=0.975,df=nrow(training_set)-2)*sqrt(variance))
  pred_int_loc_new[i,] <- c(yhat_loc[init+i-1]-qt(p=0.975,df=nrow(training_set)-2)*sqrt(variance_new),yhat_loc[init+i-1]+qt(p=0.975,df=nrow(training_set)-2)*sqrt(variance_new)) 
}

# -------------- Question 1.3 plots --------------

# Joining all data together
raw_data$yhat_loc <- yhat_loc
raw_data$errors_loc <- errors_loc
raw_data <- cbind(raw_data, pred_int_loc)
names(raw_data)[10:11]<-c('lower_loc', 'upper_loc')

train_data <- raw_data[1:which(raw_data$year==training_years[2]),]
ggplot(train_data, aes(x=year)) + 
  geom_line(aes(y=yhat_loc, color='Estimated values'), size=2) + 
  geom_line(aes(y=lower_loc, color='Prediction interval'), linetype = "dashed") + 
  geom_line(aes(y=upper_loc, color='Prediction interval'), linetype = "dashed") + 
  geom_point(aes(y=nh, color='Original data')) +
  labs(x = "Year",
       y = "",
       color = "Legend") +
  scale_color_manual(values = colors)

ggplot(train_data,aes(x=year)) +
  geom_point(aes(y=errors), colour="green", size = 3) +
  labs(x = "Year",
       y = "",
       color = "")

test_data <- raw_data[which(raw_data$year==test_years[1]):which(raw_data$year==test_years[2]),]
ggplot(test_data, aes(x=year)) + 
  geom_point(aes(y=yhat_loc, color='Estimated values'), size=2) + 
  geom_line(aes(y=lower_loc, color='Prediction interval'), linetype = "dashed") + 
  geom_line(aes(y=upper_loc, color='Prediction interval'), linetype = "dashed") + 
  geom_point(aes(y=nh, color='Original data')) +
  labs(x = "Year",
       y = "",
       color = "Legend") +
  scale_color_manual(values = colors)

Whole_data <- rbind(train_data,test_data)
Whole_data

ggplot(Whole_data, aes(x=year,y=nh)) + 
  geom_point(aes(y=yhat, color=ifelse(Purpose== 'test','Test data','Estimated values'))) + 
  geom_line(aes(y=lower, color=ifelse(Purpose== 'test','Test data','Estimated values')), linetype = "dashed") + 
  geom_line(aes(y=upper, color=ifelse(Purpose== 'test','Test data','Estimated values')), linetype = "dashed") + 
  geom_point(aes(y=nh, color='Original data')) +
  labs(x = "Year",
       y = "nh",
       color = "Legend") +
  scale_color_manual(values = colors)

ggplot(test_data,aes(x=year)) +
  geom_point(aes(y=errors_loc), colour="green", size = 3) +
  labs(x = "Year",
       y = "",
       color = "")

test_data <- raw_data[which(raw_data$year==test_years[1]):which(raw_data$year==test_years[2]),]
ggplot(test_data, aes(x=year)) + 
  geom_point(aes(y=yhat_loc, color='Estimated values'), size=2) + 
  geom_line(aes(y=pred_int_loc_new[,1], color='Prediction interval'), linetype = "dashed") + 
  geom_line(aes(y=pred_int_loc_new[,2], color='Prediction interval'), linetype = "dashed") + 
  geom_point(aes(y=nh, color='Original data')) +
  labs(x = "Year",
       y = "",
       color = "Legend") +
  scale_color_manual(values = colors)

Whole_data <- rbind(train_data,test_data)
Whole_data

ggplot(Whole_data, aes(x=year,y=nh)) + 
  geom_point(aes(y=yhat, color=ifelse(Purpose== 'test','Test data','Estimated values'))) + 
  geom_line(aes(y=lower, color=ifelse(Purpose== 'test','Test data','Estimated values')), linetype = "dashed") + 
  geom_line(aes(y=upper, color=ifelse(Purpose== 'test','Test data','Estimated values')), linetype = "dashed") + 
  geom_point(aes(y=nh, color='Original data')) +
  labs(x = "Year",
       y = "nh",
       color = "Legend") +
  scale_color_manual(values = colors)
# -------------- Question 1.4 --------------

seq1 <- seq(from=0.05, to=0.825, by=0.025)
seq2 <- seq(from=0.83, to=0.85, by=0.005)
seq3 <- seq(from=0.875, to=0.95, by=0.025)


lambdas <- c(seq1,seq2,seq3)
lambdas
output_data <- data.frame(lambdas)
output_data$errors_sum <- matrix(NA, nrow=nrow(output_data))

for (i in 1:length(lambdas)){
  # Initiating values
  x <- matrix(c(1, 1, 1, -2, -1, 0), ncol=2, nrow=3)
  Y <- training_set[1:3, 2]
  T <- 1 + lambdas[i] + lambdas[i]^2 
  diagonal <- c(1/lambdas[i]^2, 1/lambdas[i]^1, 1/lambdas[i]^0)
  sigma <- diag(diagonal, 3, 3)
  errors_temp <- 0
  
  # F and h matrices for first iteration
  F <- t(x) %*% solve(sigma) %*% x
  h <- t(x) %*% solve(sigma) %*% Y
  
  # First solution
  theta <- solve(F, h)              
  yhat_temp <- t(f(1)) %*% theta      
  errors_temp <- errors_temp + (yhat_temp-raw_data[4,2])^2
  
  # Loop for further estimates
  for (j in 4:nrow(training_set-1)){
    F <- F + lambdas[i]^(j-1) * f(-(j-1)) %*% t(f(-(j-1)))
    h <- lambdas[i] * LInv %*% h + f(0)*training_set[j,2]
    x <- rbind(c(1,-j+1), x)
    theta <- solve(F, h)
    yhat_temp <- t(f(1)) %*% theta
    errors_temp <- errors_temp + (yhat_temp-raw_data[j+1,2])^2
  }
  output_data[i, 2] <- errors_temp
}


minimum <- output_data[which.min(output_data$errors_sum),]
minimum

min_DF = data.frame(c(1,2),c(minimum$lambdas,minimum$errors_sum))

# Plot
ggplot(output_data, aes(x=lambdas, y=errors_sum))+ geom_line()+ geom_point(x=minimum$lambdas,y=minimum$errors_sum[1,1],color="red",size=3)


