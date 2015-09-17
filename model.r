library( corrplot );
library( caret );
library( gbm );

setwd( "/sscc/home/a/amk202/Predictive" );

gini = function ( actual, predicted ) {
  n = length( actual );
  gini_data = as.data.frame( cbind( actual, predicted, row = 1:n ) );
  gini_data = gini_data[ with( gini_data, order( -predicted, row ) ), ];
  sum( cumsum( gini_data[ , 1 ] ) / sum( gini_data[ , 1 ] ) - ( 1:n ) / n ) / n;
}

carTrain = read.csv( "car_train.csv", header = T );
carTest = read.csv( "car_test.csv", header = T );

n = nrow( carTrain );

carTrain[ , "IsBadBuy" ] = factor( carTrain[ , "IsBadBuy" ] );
carTest[ , "IsBadBuy" ] = factor( carTest[ , "IsBadBuy" ] );

varResp = 1;
varPred = c( 3, 5:6, 10:16, 25:26, 28:30, 41:42 );

########################################## Bagging ##########################################

startTime = proc.time( );
control = bagControl( fit = ctreeBag$fit, predict = ctreeBag$pred, aggregate = ctreeBag$aggregate, downSample = F  );
fit_bag = bag( x = carTrain[ , varPred ], y = carTrain[ , varResp ], B =  10, bagControl = control );
proc.time( ) - startTime;

summary( fit_bag );

phat = predict( fit_bag, carTest[ , varPred ], type = "response" );

yhat = apply( phat, 1, function( x ) names( which.max( x ) ) );
yhat = factor( yhat, levels = levels( carData[ , "IsBadBuy" ] ) );
# yhat = factor( as.numeric( phat > 0.5 ) );

MCE = sum( yhat != carTest[ , "IsBadBuy" ] ) / nrow( carTest );

table( carTest[ , "IsBadBuy" ], yhat );

gini( as.numeric( carTest[ , "IsBadBuy" ] == "1" ), phat[ , "1" ] );

########################################## Boosted Trees ##########################################

N = 10;

interactDepth = 2:7;
lambda = seq( 0.01, 0.1, 0.02 );

CVerror = matrix( NA, length( interactDepth ), length( lambda ) );

for ( i in 1:length( interactDepth ) ) {
  for ( l in 1:length( lambda ) ) {
    CVerr = c( );
    
    for ( j in 1:N ) {
      fit_gbm = gbm( IsBadBuy ~ ., data = carTrain[ , c( varResp, varPred ) ], distribution = "bernoulli",
                     n.trees = 3000, shrinkage = lambda[ l ], interaction.depth = interactDepth[ i ],
                     bag.fraction = 0.5, train.fraction = 1, n.minobsinnode = 20, cv.folds = 10, keep.data = T );
      
      bestIter = gbm.perf( fit_gbm, method = "cv", plot.it = F );
      CVerr = c( CVerr, fit_gbm$cv.error[ bestIter ] );
    }
    
    CVerror[ i, l ] = mean( CVerr );
  }
}
  
indexMin = which.min( CVerror );
IDMin = interactDepth[ 1 + ( indexMin - 1 ) %% ( length( interactDepth ) ) ];
lambdaMin = lambda[ 1 + ( indexMin - 1 ) %/% ( length( interactDepth ) ) ];

# IDMin = 4, lambdaMin = 0.01

fit_gbm = gbm( IsBadBuy ~ ., data = carTrain[ , c( varResp, varPred ) ], distribution = "bernoulli",
               n.trees = 3000, shrinkage = lambdaMin, interaction.depth = IDMin, bag.fraction = 0.5,
               train.fraction = 1, n.minobsinnode = 20, cv.folds = 10, keep.data = T );

bestIter = gbm.perf( fit_gbm, method = "cv" );
summary( fit_gbm, n.trees = bestIter );

print( pretty.gbm.tree( fit_gbm, bestIter ) );

plot( fit_gbm, i.var = c( 7, 13 ), n.trees = bestIter );
plot( fit_gbm, i.var = c( 13, 16 ), n.trees = bestIter );

phat = predict( fit_gbm, newdata = carTest, n.trees = bestIter, type = "response" );
yhat = as.numeric( phat > 0.5 );

sum( yhat != carTest[ , "IsBadBuy" ] ) / nrow( carTest );
table( carTest[ , "IsBadBuy" ], yhat );

gini( carTest[ , "IsBadBuy" ], phat );

plot.roc( carTest[ , "IsBadBuy" ], phat )
