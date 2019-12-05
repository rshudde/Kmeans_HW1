# Application of K-means algorithm to ZIPCODE data

# Rand Index Calculation example
library(fossil)
cluster1 <- c(2,2,1,3)
cluster2 <- c(3,3,2,1)
rand.index(cluster1, cluster2) # clusters match

# Load the ZIPCODE data
zipcode <- read.table("ZIPCODE.txt", header = F)

# Extract the true digits
Y <- zipcode[ , 1]

# Extract the data points
X <- zipcode[ , -1]

# Try K-means algorithm nRep times with different starting points on ZIPCODE data. 
# Calculate Rand Index at each replication
nRep <- 100
accuracy = rep(0, nRep)

for (i in 1:nRep)
{
    Y_test = MyKmeans( as.matrix( X ), 10, NULL, 100 )
    accuracy[i] = rand.index( Y, Y_test )
}

# R Report mean Rand Index
paste( "The average accuracy over 100 repitions is: ", round( sum( accuracy ) / nRep*100, 2 ) )

# Report mean run time for one replication on your machine
library(microbenchmark)
microbenchmark(
    MyKmeans( as.matrix( X ), 10, NULL, 100 ),
    times = 10
)

# Try RPROF - this shows where the code is spending hte most time
# for some reason 'is.data.frame' is taking up 50% of my runtime, and I am 
# still not sure where that function is being called
Rprof()
MyKmeans( as.matrix( X ), 10, NULL, 100 )
Rprof( NULL )
summaryRprof()

