install.packages("Rmosek", type="source", INSTALL_opts="--no-multiarch",repos="http://download.mosek.com/R/8", configure.vars="PKG_MOSEKHOME=C:/Program Files/Mosek/8/tools/platform/win64x86 PKG_MOSEKLIB=mosek64")
library(Rmosek)
mosek_version()
version
rm(list=ls())
#Importing Data Set
blogData <- read.csv("D:\MIS\Spring 2018\ML\Proj 2\blogdata\train.csv", header=FALSE)
basic_features<-blogData[c(51:60,281)]
sampl<-basic_features[sample(1:nrow(basic_features), 5000,replace=FALSE),]

# Removing column 55 and 60
drop_col <- names(sampl) %in% c("V55", "V60")
blogData=sampl[!drop_col]
blogData

# Setting the seed
set.seed(11)
sizes<- (nrow(sampl) * 0.75)
train_data <- sample((1:nrow(sampl)), size = sizes)
train <- blogData[train_data, ]
test <- blogData[-train_data, ]
View(train)

#Taking Feature matrix A
blog_Train_A=train[,1:ncol(train)-1]
blog_Train_B
blog_Train_B=train[,ncol(train)]
View(blog_Train_B)
blog_Train_B=as.matrix(blog_Train_B)
blog_Train_A=as.matrix(blog_Train_A)


blog_Test_A=test[,1:ncol(test)-1]
blog_Test_A
blog_Test_B=test[,ncol(test)]
View(blog_Test_B)
blog_Test_B=as.matrix(blog_Test_B)
blog_Test_B=as.matrix(blog_Test_A)


#calling func solve.ols
objfun=solve.ols(blog_Train_A, blog_Train_B)
objfun

#Equation for the target variable
ycap=blog_Test_A[,1]0.0009095406 + blog_Test_A[,2]*0.2710035193 + blog_Test_A[,3](-0.0294062500)+ blog_Test_A[,4](0.0132145918)  + blog_Test_A[,5](-2.5847932464) +blog_Test_A[,6](1.6881868585) +blog_Test_A[,7](0.7124617630)+  blog_Test_A[,8]*3.0793374887
ycap
YMinusYcap = blog_Test_B-ycap

#definition of error function
mse <- function(error)
{
  mean(error^2)
}

#Calculating Mean Square Error
MSE_Mosek=mse(YMinusYcap)
RSS_Mosek=sum(YMinusYcap^2)
MSE_Mosek
YMinusYcap
RSS_Mosek

#definition of fucntion solve.ols
solve.ols<-function(X,y, verb=1){
  param<-dim(X)[2]  
  
  #correspondence between OLS and the Quadratic Program
  xx<-crossprod(X)
  c<--crossprod(X,y) 
  xx2<-xx
  xx2[upper.tri(xx)]<-0 
  idx <- which(xx2 != 0, arr.ind=TRUE) 
  
  #problem definition in Mosek
  qo1<-list() 
  qo1$sense<-"min" 
  qo1$c<-as.vector(c) 
  qo1$qobj<-list(i = idx[,1],
                 j = idx[,2],
                 v = xx2[idx] ) 
  qo1$A<-Matrix(rep(0,param), nrow=1,byrow=TRUE,sparse=TRUE) 
  
  qo1$bc<-rbind(blc=-Inf, buc= Inf) 
  qo1$bx<-rbind(blx=rep(-Inf,param), bux = rep(Inf,param)) 
  
  r<-mosek(qo1, opts = list(verbose = verb)) 
  return(r)
}

###################################################################################
###################################################################################

#using traing set train
colnames(train) <- c('V51','V52','V53','V54','V56','V57','V58','V59',"Target")

#Linear Regression LM
head(train)
head(blog_Test_A)
Model_LM_1 <- lm(formula = Target ~ .,data = data.frame(train))
Prediction_LM <- predict(Model_LM_1,blog_Test_A,se.fit = TRUE)
summary(Model_LM_1)
Prediction_LM$fit

#mean squared error calculation 
dim(train)
dim(test)
error_1 <- (blog_Test_B - Prediction_LM$fit)
msError=mse(error_1)
msError
error_1=sum(error_1**2)
error_1
typeof(Prediction_LM)
error_1
print(mean(error_1^2))


#Summary
summary(Model_LM_1)
