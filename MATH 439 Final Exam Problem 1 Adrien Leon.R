setwd("C:/Users/AdrienL/Desktop/MATH 439")

data = read.csv("antigravity.csv",h=T)
head(data)
#         P         t
#1 6.474757 0.7307139
#2 4.799978 1.2788421
#3 7.280305 0.4716559
#4 7.170258 0.8864955
#5 7.737092 0.5573943
#6 5.328621 1.6377777
attach(data)

#P = position
#t = time 0<t<3
plot(data$t,data$P)

#Model looks like a U shape, cubed
model = lm(P~t)
par(mfrow=c(2,2))
plot(model)
summary(model)

#Resid vs. fitted: U shape
#Normal Q-Q: Tails towards left at bottom
#Scale Location: W shape
#Residuals vs leverage: Lots of Leverage and some points
#past Cook's distance

#Since it's a cubic polynomial, we can model it as
x = data$t
y = data$P
model3 = lm(y~x+I(x^2)+I(x^3))
summary(model3)

#Call:
#lm(formula = y ~ x + I(x^2) + I(x^3))
#
#Residuals:
#    Min      1Q  Median      3Q     Max 
#-4.7225 -0.9263  0.0294  0.9942  4.2321 
#
#Coefficients:
#            Estimate Std. Error t value Pr(>|t|)    
#(Intercept)   9.7729     0.4330  22.569   <2e-16 ***
#x             0.1719     1.2541   0.137    0.891    
#I(x^2)       -9.6788     0.9768  -9.908   <2e-16 ***
#I(x^3)        4.7725     0.2150  22.200   <2e-16 ***
#---
#Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
#
#Residual standard error: 1.531 on 196 degrees of freedom
#Multiple R-squared:  0.9832,    Adjusted R-squared:  0.9829 
#F-statistic:  3823 on 3 and 196 DF,  p-value: < 2.2e-16
 
par(mfrow=c(2,2))
plot(model3)


#There supposedly coefficients for their model is not valid
#z-test to see what their prediction is
#P(t) = 4.8*t^3 – 9.8*t^2 + 5 for t:0<t<3
#Confidence Interval

#(Intercept)
9.7729-1.96*0.4330
#[1] 8.92422
9.7729+1.96*0.4330
#[1] 10.62158

#x
0.1719-1.96*1.2541
#[1] -2.286136
0.1719+1.96*1.2541
#[1] 2.629936

#I(x^2)
-9.6788-1.96*0.9768
#[1] -11.59333
-9.6788+1.96*0.9768
#[1] -7.764272

#I(x^3)
4.7725-1.96*0.215
#[1] 4.3511
4.7725+1.96*0.215
#[1] 5.1939

#As we can see, their hypothesis for the intercept is off by a lot from
#our 95% confidence interval of the cubic polynomial says that the 
#intercept should be between 8.92422 to 10.62158. So their prediction
#of 5 is no good.


dev.off()
plot(x,y)
fit <- lm(y ~ x + I(x^2) + I(x^3))
pred <- predict(fit)
ix <- sort(x, index.return=T)$ix
lines(x[ix], pred[ix], col='red', lwd=2)

t1 = 0:300/100
P1 = 4.8*t1^3-9.8*t1^2+5
lines(t1,P1,col=3,lwd=2)

#We can see our orginal 200 data plots in circles and our predicted
#best fit line in red and the scientist's predicted measurement
#equation in green. As we can see visually, their intercept is a lot
#lower than what we expect.


