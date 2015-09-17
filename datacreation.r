library( corrplot );

setwd( "/sscc/home/a/amk202/Predictive" );

merge_levels = function ( x, levelOld, levelNew, newCat = F ) {
  if ( !newCat ) {
    levels( x )[ which( levels( x ) %in% levelOld ) ] = levels( x )[ which( levels( x ) %in% levelNew ) ];
  }
  else {
    levels( x )[ which( levels( x ) %in% levelOld ) ] = levelNew;
  }

  x;
}

carData = read.csv( "../SuddenLink/predictive/car_data.csv", header = T );
# test = read.csv( "../SuddenLink/predictive/test.csv", header = T );

varCont = c( "VehicleAge","VehOdo", "MMRAcquisitionAuctionAveragePrice", "MMRAcquisitionAuctionCleanPrice",
              "MMRAcquisitionRetailAveragePrice", "MMRAcquisitonRetailCleanPrice", "MMRCurrentAuctionAveragePrice",
              "MMRCurrentAuctionCleanPrice", "MMRCurrentRetailAveragePrice", "MMRCurrentRetailCleanPrice",
              "VehBCost", "WarrantyCost" );

varCat = c( "IsBadBuy", "VehYear", "Auction", "Make", "Model", "Trim", "SubModel", "Color", "Transmission",
             "WheelTypeID", "WheelType", "Nationality", "Size", "TopThreeAmericanName", "PRIMEUNIT", "AUCGUART",
             "BYRNO", "VNZIP1", "VNST", "IsOnlineSale" );

var_date = c( "PurchDate" );

carData = carData[ , -c( 1:2 ) ];

check = sapply( carData[ , varCont ], is.factor );
carData[ , varCont ][ check ] = sapply( carData[ , varCont ][ check ], function( x )
                                                                              as.integer( as.character( x ) ) );

check = sapply( carData[ , varCat ], function( x ) !is.factor( x ) );
carData[ , varCat ][ check ] = lapply( carData[ , varCat ][ check ], factor );

carData = carData[ complete.cases( carData ), ];

# carData[ , "PurchDate" ] = as.Date( carData[ , "PurchDate" ], format = "%m/%d/%Y" );
# carData[ , "PurchYear" ] = as.factor( format( carData[ , "PurchDate" ], "%Y" ) );

carData[ , "Color" ] = merge_levels( carData[ , "Color" ], "NULL", "NOT AVAIL" );
carData[ , "Transmission" ] = merge_levels( carData[ , "Transmission" ], "", "NULL" );
carData[ , "Transmission" ] = merge_levels( carData[ , "Transmission" ], "Manual", "MANUAL" );
carData[ , "WheelTypeID" ] = merge_levels( carData[ , "WheelTypeID" ], "0", "NULL" );

check = carData[ , "Nationality" ] == "NULL";
carData = carData[ !check, ];
carData[ , varCat ] = lapply( carData[ , varCat ], factor );

carData[ , "region" ] = carData[ , "VNST" ];
carData[ , "region" ] = merge_levels( carData[ , "region" ],
                          c( "PA", "NY", "NJ", "CT", "MA", "VT", "NH", "ME", "RI" ), "Northwest", newCat = T );
carData[ , "region" ] = merge_levels( carData[ , "region" ],
                          c( "ND", "SD", "NE", "KS","MN","IA","MO","WI","IL","IN","MI","OH" ), "Midwest", newCat = T );

check = which( carData[ , "Make" ] %in% c( "HUMMER", "PLYMOUTH", "TOYOTA SCION" ) );
carData = carData[ -check, ];

carData[ , "Differential" ] = with( carData,
                                     ( VehBCost - MMRAcquisitionRetailAveragePrice ) / MMRAcquisitionRetailAveragePrice );
check = is.infinite( carData[ , "Differential" ] );
carData[ check, "Differential" ] = 0;

varDrop = c( "WheelTypeID", "BYRNO", "VNZIP1" );
carData = carData[ , -which( colnames( carData ) %in% varDrop ) ];

x = rep( "", nrow( carData ) );
# check = carData[ , "IsBadBuy" ] == 1;
# x[ check ] = "BadBuy";
# x[ !check ] = "NotBadBuy";
# carData[ , "IsBadBuy" ] = factor( x );

check = carData[ , "IsOnlineSale" ] == 1;
x[ check ] = "Yes";
x[ !check ] = "No";
carData[ , "IsOnlineSale" ] = factor( x );

set.seed( 1 );

sampleRows = sample( nrow( carData ), 0.7 * nrow( carData ) );
carTrain = carData[ sampleRows, ];
carTest = carData[ -sampleRows, ];

rownames( carData ) = NULL;
write.csv( carData, "car_data_clean.csv", row.names = F );
write.csv( carTrain, "car_train.csv", row.names = F );
write.csv( carTest, "car_test.csv", row.names = F );
write.csv( sampleRows, "car_train_rows.csv", row.names = F );

# Correlations

corrplot.mixed( cor( carData[ , varCont ] ), lower = "number", upper = "ellipse" );

# regions
# Make
# diff cost
