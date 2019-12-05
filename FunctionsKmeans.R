# COLLABORATION NOTE: My office mate (Will Boyles), and Asmita Roy and Ananaya Roy Chowdury
# and I worked out some of the algebra/algorithms on the board our office together. 
# We did not share code or work on coding things up together besides comparing how we constructed matrices

## function to initialize M
# X - matrix of data
# K - number of clusters
create_M = function( X, K, M ) 
{
  n = nrow( X )
    
  # initialize M if it is not NULL
  if ( is.null( M ) )
  {
    # sample random numbers from 1:n, then combine the samples into a new matrix M
    M = X[sample( 1:n, K, replace = FALSE ), ]
  }

  # check dimensions even if M is originally not NULL
  if ( ncol( X ) != ncol( M ) )
  {
    stop( "Incorrect dimensions for X and K" )
  }
    
  # check if number of rows in new M = K
  if ( nrow( M ) != K )
  {
    stop( "Number of rows in M does not equal supplied K" )
  }
 
  # check if K > n, if so, warn the user
  if ( K > n )
  {
    warning( "K is bigger than n, this may causes crashes unexpected results" )
  }

  # this will return the original M if not NULL, or the created M otherwise
  return (M)
}

## function to find means
# X - matrix of data
# K - number of clusters
# M - current matrix of means
# Y - current classification 
update_M = function( X, K, Y )
{
  # loop over all possible clusters to get new new values for M
  # then use the new means to update M
  Mtemp = matrix(NA, nrow = K, ncol = ncol( X ) )
  
  for ( i in 1:K )
  {
    # TODO check this
    Mtemp[i, ] = colMeans( X[Y == i, , drop = F] )
  }
  
  # return the updated M
  return( Mtemp )
}

## function to calculate the distance between things
# X - matrix of data
# K - number of clusters
# M - current matrix of means
calculate_distance_matrix = function( X, K, M )
{
    n = nrow(X)
  # here we can use the trick of expanding the distance matrix and 
  # using that to vectorize.
  # The first element is the matrix of XX' (ignored because it's constant) 
  # the second element is XM, and the final element is MM'. This helps us 
  # speed up the calculations. 

  # source on getting diagonals without doing cross product: https://stackoverflow.com/questions/42569698/how-to-just-calculate-the-diagonal-of-a-matrix-product-in-r
    
  # get second element
  second = tcrossprod( X, M )

  # get third element - take diagonal elements of XM' and paste into matrix
  transpose_M = t( M )
  diag_M_rep = colSums( transpose_M * transpose_M )
  third = matrix( rep( diag_M_rep ), n, ncol = K, byrow = T )

  # return the approrpaite sum. Note we can exclude the square root since
  # it will have no impact on finding the minimum
  return( -2*second + third )
}

## function that implements K-means algorithm for a given number of iterations.
# X - matrix of data
# K - number of clusters
# M - OPTIONAL:  matrix of starting means
# numIter - OPTIONAL: number of times to run algorithm
MyKmeans <- function( X, K, M = NULL, numIter = 100 )
{
  # check tht M is a matrix
  if ( !is.matrix( X ) )
  {
    stop( "X is not in matrix form, pass in argument as 'as.matrix(X)'" )
  }
    
  # create initial M 
  M = create_M( X, K, M )
  
  # do this n times
  for ( i in 1:numIter )
  {
    # Implement K-means algorithm
    distance = calculate_distance_matrix( X, K, M )

    # source: https://stackoverflow.com/questions/18324053/r-return-the-index-of-the-minimum-column-for-each-row
    Y = max.col( -distance )

    # update M matrix
    M = update_M( X, K, Y )
  }

  # Return the vector of assignments Y
  return( Y )
}
