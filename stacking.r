library( foreach );
library( nnet );
library( gbm );

setwd( "/sscc/home/a/amk202/Predictive" );

standardize = function( x, newdata, data ) {
  ( newdata[ , x ] - mean( data[ , x ] ) ) / sd( data[ , x ] );
}

gini = function ( actual, predicted ) {
  n = length( actual );
  gini_data = as.data.frame( cbind( actual, predicted, row = 1:n ) );
  gini_data = gini_data[ with( gini_data, order( -predicted, row ) ), ];
  sum( cumsum( gini_data[ , 1 ] ) / sum( gini_data[ , 1 ] ) - ( 1:n ) / n ) / n;
}

carTrain = read.csv( "car_train.csv", header = T );
carTest = read.csv( "car_test.csv", header = T );

n = nrow( carTrain );

varResp = 1;
varPred = c( 3, 5:6, 10:16, 25:26, 28:30, 41:42 );
varScale = c( "VehicleAge", "VehOdo", "VehBCost", "WarrantyCost", "Differential" );

train = carTrain;
test = carTest;

train_scale = carTrain;
train_scale[ , varScale ] = scale( train_scale[ , varScale ] );
test_scale = carTest;
test_scale[ , varScale ] = as.data.frame( lapply( colnames( test_scale[ , varScale ] ), standardize,
                                  newdata =  test_scale, data = carTrain ) );

N = 200;

predictions_gbm = foreach( run = 1:N, .combine = cbind ) %do% {
  n = nrow( train );
  sampleboot = sample( n, n );  
  
  fitboot_gbm = gbm( IsBadBuy ~ ., data = train[ sampleboot, c( varResp, varPred ) ], distribution = "bernoulli",
                 n.trees = 600, shrinkage = 0.01, interaction.depth = 6, bag.fraction = 0.5,
                 train.fraction = 0.5, n.minobsinnode = 10, cv.folds = 0, keep.data = T );
  bestIter = gbm.perf( fitboot_gbm, method = "OOB" );
  
  print( bestIter );

  phat_gbm = predict( fitboot_gbm, test, n.trees = bestIter, type = "response" );
}

predictions_nnet = foreach( run = 1:N, .combine = cbind ) %do% {
  n = nrow( train_scale );
  sampleboot = sample( n, 0.5 * n );  
  
  fitboot_nn = nnet( IsBadBuy ~ ., train_scale[ sampleboot, c( varResp, varPred ) ], linout = F, skip = F,
               maxit = 1000, trace = F, size = 2, decay = 0.1 );
  
  print( run );
  
  phat_nn = predict( fitboot_nn, test_scale, type = "raw" );
}

phat_gbm = rowMeans( predictions_gbm );
phat_nnet = rowMeans( predictions_nnet );

phat1 = apply( cbind( phat_gbm, phat_nn ), MARGIN = 1, FUN = max );
phat2 = rowMeans( cbind( phat_gbm, phat_nn ) );

yhat1 = as.numeric( phat1 > 0.5 );

sum( yhat1 != carTest[ , "IsBadBuy" ] ) / nrow( carTest );
table( carTest[ , "IsBadBuy" ], yhat1 );

gini( carTest[ , "IsBadBuy" ], phat1 );

yhat2 = as.numeric( phat2 > 0.5 );

sum( yhat2 != carTest[ , "IsBadBuy" ] ) / nrow( carTest );
table( carTest[ , "IsBadBuy" ], yhat2 );

gini( carTest[ , "IsBadBuy" ], phat2 );

