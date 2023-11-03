setwd("C:/Users/AdrienL/Desktop/MATH 439")
library(rpart)
library(adabag)
library(randomForest)
library(ROCR)
library(gbm)

data = read.csv("eqmag.csv",h=T)
head(data)
#  magnitude fault_depth fault_angle fault_length volcano
#1  5.683777    6.593868   66.543026   18.3706911       1
#2  4.595783    3.901338   19.261750    0.2988122       1
#3  4.732101    4.278531   31.322007   12.7967537       1
#4  4.548045    4.418310   30.112433   11.1263541       1
#5  4.838043    4.943698   -7.512702   18.2085859       1
#6  4.847247    3.670954   57.872159   50.3241621       1

attach(data)
x1 = data$fault_depth
x2 = data$fault_angle
x3 = data$fault_length
x4 = data$volcano	

y = data$magnitude





#Part a.)
#We'll start by using a linear model to make a prediction
model1 = lm(y~x1+x2+x3+x4)
lm(y~x1+x2+x3+x4)
#Call:
#lm(formula = y ~ x1 + x2 + x3 + x4)
#
#Coefficients:
#(Intercept)           x1           x2           x3           x4  
#   3.409179     0.342202     0.001497     0.003974    -0.073838 

summary(model1)

#Call:
#lm(formula = y ~ x1 + x2 + x3 + x4)

#Residuals:
#     Min       1Q   Median       3Q      Max 
#-0.66246 -0.20638 -0.03125  0.17992  0.83619 
#
#Coefficients:
#              Estimate Std. Error t value Pr(>|t|)    
#(Intercept)  3.4091790  0.0393043  86.738   <2e-16 ***
#x1           0.3422018  0.0085191  40.169   <2e-16 ***
#x2           0.0014973  0.0008675   1.726   0.0853 .  
#x3           0.0039744  0.0003425  11.603   <2e-16 ***
#x4          -0.0738378  0.0383003  -1.928   0.0548 .  
#---
#Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
#
#Residual standard error: 0.2915 on 311 degrees of freedom
#Multiple R-squared:  0.8958,    Adjusted R-squared:  0.8945 
#F-statistic: 668.6 on 4 and 311 DF,  p-value: < 2.2e-16
#r^2 = 0.89 for linear model

par(mfrow=c(2,2))
plot(model1)
dev.off()


#Random Forest
earthquake = data.frame(x1,x2,x3,x4,y)
model.rf = randomForest(y~x1+x2+x3+x4,data=earthquake,ntree=100,mtry=2,control=rpart.control(minsplit=10,cp=.05))
fitted.values = model.rf$predicted
residuals = y - fitted.values
plot(fitted.values,residuals)

#Centered around 0.

names(model.rf)
model.rf$votes
model.rf$err.rate
plot(model.rf)

prediction.rf = model.rf$predicted
table(prediction.rf,earthquake$y)

model.rf
#Call:
# randomForest(formula = y ~ x1 + x2 + x3 + x4, data = earthquake,      ntree = 100, mtry = 2, control = rpart.control(minsplit = 10,          cp = 0.05)) 
#               Type of random forest: regression
#                     Number of trees: 100
#No. of variables tried at each split: 2
#
#          Mean of squared residuals: 0.05220929
#                    % Var explained: 93.5
#



#Manually compute r^2 based on residuals.
SSR = sum((data$magnitude - prediction.rf)^2)
SST = var(data$magnitude)*(length(data$magnitude)-1)
r2 = (SST - SSR)/SST
r2
#[1] 0.9349619

#Hey, our r^2 value here is higher than the linear model
#This tells us that 93.5% of the variance of the dependent variable 
#being studied is explained by the variance of the independent variable.
#Hence, why a random forest is a better model than a linear one.


par(mfrow=c(2,2))
plot(model.rf)









#Gradient Boosting
library(gbm)
earthquake1 = data.frame(x1,x2,x3,x4,y)

gbm.model = gbm(y ~ x1 + x2 + x3 + x4, data = earthquake1, n.trees=1200, distribution="gaussian", shrinkage =  0.01, cv.folds = 5)
summary(gbm.model)

gbm.model$cv.error
plot(1:1200,gbm.model$cv.error,type="l")

gbm.model$train.error
plot(1:1200,gbm.model$train.error,type="l")


fit = gbm.model$cv.fitted
errors = data$magnitude - fit

# Create residual vs. fit plot.
plot(fit,errors)
loess.line = loess(errors~fit)
lines(loess.line$x[order(loess.line$x)],loess.line$fitted[order(loess.line$x)],col=2)
lines(c(0,200),c(0,0),lty=2,col="grey")

#Compute r^2 manually
r2 = (sum((data$magnitude - mean(data$magnitude))^2) - sum(errors^2))/sum((data$magnitude - mean(data$magnitude))^2)
r2
#[1] 0.9424886




#Part c.) 
#plug in variables to get answer
#For Random Forest
newdata = data.frame(x1=1.6,x2=13,x3=67,x4=1)
predict(model.rf,newdata=newdata,interval='confidence')
#       1 
#4.439287 

#With Gradient Boosting Model Summer Home
gbm.predict = predict(gbm.model, newdata = newdata)
gbm.predict
#[1] 4.292888
