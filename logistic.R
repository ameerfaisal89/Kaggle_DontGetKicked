car_test <- read.csv("/sscc/home/a/amk202/Predictive/car_test.csv")
car_train <- read.csv("/sscc/home/a/amk202/Predictive/car_train.csv")

mydata = car_train

mydata=car_test <- read.csv("/sscc/home/a/amk202/Predictive/car_test.csv")
drop = c("PurchDate","VehYear","Model","Trim","SubModel","PRIMEUNIT","AUCGUART","VNST")
mydata = mydata[!(names(mydata) %in% drop)]

car_test = car_test[!(names(car_test) %in% drop)]

fit<-glm(IsBadBuy~., data = mydata)
summary(fit)

library(MASS)
#use stepwise to select predictors
fit2<-stepAIC(fit)
summary(fit2)
formula(fit2)

mydata$WheelType = relevel(mydata$WheelType,"NULL") #set "NULL" group to be base category
#drop insignificant variables from the model
f = update(formula(fit2), .~. - bachelor-Auction-Make-region- WarrantyCost -mean_income-Transmission-Size)
fit3 = glm(f,data=mydata)
summary(fit3)

#check multicollinearity among predictors
vif(fit3)

#drop predictors has vif larger than 10, whhich mean there are multicollinearity within them
f = update(formula(fit3), .~. -median_income-MMRCurrentRetailCleanPrice- total_population-MMRAcquisitionRetailAveragePrice-moved_total-MMRAcquisitionAuctionAveragePrice-MMRAcquisitonRetailCleanPrice-MMRCurrentAuctionAveragePrice)
fit4 = glm(f,data=mydata)
summary(fit4)

#exponential coefficients to get the cofficients of predictor on odds of bad-buy vs. good-buy
exp_coef = exp(cbind(OR = coef(fit4),confint(fit4)))

#predict outcome based on training set
yhat = predict(fit4,type="response")
yp = yhat
#set cut off to be 0.5
yp[yhat>0.5] = 1
yp[yhat<=0.5] = 0
y = mydata$IsBadBuy
#get the classifcation table
table = table(y,yp)
table
#compute the missclassification rate
sum(yhat != y)/length(y)

#set up gini funtion
gini = function ( actual, predicted ) {
  n = length( actual );
  gini_data = as.data.frame( cbind( actual, predicted, row = 1:n ) );
  gini_data = gini_data[ with( gini_data, order( -predicted, row ) ), ];
  sum( cumsum( gini_data[ , 1 ] ) / sum( gini_data[ , 1 ] ) - ( 1:n ) / n ) / n;
}

#predict outcome based on test set
phat = predict(fit4, car_test, type ="resp")
yhat = phat
#set cut off to be 0.5
yhat[phat>0.5] = 1
yhat[phat<0.5] = 0
y= car_test$IsBadBuy
#test set classfication table
tab2 = table(y, yhat)
tab2
#missclassfiction rate on test set
sum(yhat != y)/length(y)

y_test = car_test$IsBadBuy
y_test_hat = phat
gini(y_test,y_test_hat) # get gini on test set

library(pROC)
#plot ROC curve
roc = plot.roc(car_test$IsBadBuy,phat)
#get area under ROC curve
auc(roc)
