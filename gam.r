MCE_gam = c( );

N = 10;
k = 10;

set.seed( 1 );

for ( j in 1:N ) {
  indexCV = cvIndex( n, k );
  yhat = rep( NA, n );
  
  for ( i in 1:k ) {
    fit_gam = gam( IsBadBuy ~ Auction + Transmission + TopThreeAmericanName + IsOnlineSale
                   + s( VehicleAge ) + s( Make ) + s( Color, k = 10 ) + s( WheelType, k = 3 )
                   + s( VehOdo ) + s( Nationality, k = 3 ) + s( Size ) + s( VehBCost ) + s( region )
                   + s( Differential ), data = carTrain[ -indexCV[[ i ]], ], family = binomial( ),
                   sp = c( -1, -1, -1, -1, -1, -1, -1, -1, -1, -1 ) );
    yhat[ indexCV[[ i ]] ] = predict( fit_gam, model_data[ indexCV[[ i ]], ] );
  }
  
  e = yhat - model_data[ , "cost" ];
  
  SSE = c( SSE, sum( e ^ 2 ) );
  Rsq = c( Rsq, 1 - var( e ) / var( model_data[ , "cost" ] ) );
}

sqrt( mean( SSE ) / n );

fit_gam = gam( IsBadBuy ~ Auction + Make + Color + WheelType + Transmission + TopThreeAmericanName + IsOnlineSale
               + s( VehicleAge ) + s( Make )  +  s( VehOdo ) + s( Size ) + s( VehBCost ) + s( region )
               + s( Differential ), data = carTrain[ -indexCV[[ i ]], ], family = binomial( ),
               sp = c( -1, -1, -1, -1, -1, -1, -1, -1, -1, -1 ) );

x_star = data.frame( 59, 0, 10, 0, 3, 0, 4, 300 );
names( x_star ) = names( heart_data )[ -1 ];
x_pred = as.data.frame( lapply( colnames( x_star ), standardize, newdata = x_star, data = heart_data ) );
names( x_pred ) = names( heart_data )[ -1 ];

yhat = predict( fit_gam, x_pred );