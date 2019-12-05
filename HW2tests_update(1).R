# Author: Irina Gaynanova
#############################################
# Points allocation:
# 50 pt for FunctionsKmeans correctness
# 30 pt for speed - see the end
# 10 pt for comments and code style in both .R files
# 10 pt for clarity, frequency and logic of commits (1 logical change =  1 commit, can tell what happened based on commit comment, started commits at least 2 days before the HW is due)

# Instructions for usage:
# - Make sure your working directory is set to your HW2 folder
# - Make sure that HW2tests.R and IrinasKmeans.R are within the same folder
# - You can source these .R file to run all checks at once. The last speed comparison may take time, especially if your code is slow
# - These tests can only be used to assess the correctness of the code in FunctionsKmeans.R and its speed. We will use our own assessment for comments/style (by cross-comparing your code and mine)

rm(list = ls())

# Updated, for rand.index function
library(fossil)

# Load my functions
source("IrinasKmeans.R")
# Load your functions
source("FunctionsKmeans.R")


# Load and install if necessary testing package
require(testthat)

# Does it run without error on small K or large K with M = NULL?
################################################################


# Example from built-in kmeans function in R
set.seed(923846)
x <- rbind(matrix(rnorm(100, sd = 0.3), ncol = 2),
           matrix(rnorm(100, mean = 1, sd = 0.3), ncol = 2))

# Does it run without errors on a simple example with different K?
test1_smallK = tryCatch(MyKmeans(X = x, K = 2, M = NULL, numIter = 100), error = function(e) 2)
test1_largeK = tryCatch(MyKmeans(X = x, K = 15, M = NULL, numIter = 100), error = function(e) 2)

if ((length(test1_smallK) == 100)&(length(test1_largeK) == 100)){
  test1 = 1
}else{
  test1 = 2
}


# Does it agree on the same input with given M?
########################################################
selected_id = c(21, 25)
M = x[selected_id, ]

out2 = MyKmeans(X = x, K = 2, M = M, numIter = 5)
out2_Irina = IrinasKmeans(X = x, K = 2, M = M, numIter = 5)

x <- rbind(matrix(rnorm(100, sd = 0.3), ncol = 2),
           matrix(rnorm(100, mean = 1, sd = 0.3), ncol = 2),
           matrix(rnorm(100, mean = -1, sd = 0.3), ncol = 2))

selected_id = c(21, 25, 105)
M = x[selected_id, ]

out3 = MyKmeans(X = x, K = 3, M = M, numIter = 10)
out3_Irina = IrinasKmeans(X = x, K = 3, M = M, numIter = 10)


test2 = test_that("Smaller K is correct", {
  expect_equal(as.vector(out2), out2_Irina)
  expect_equal(as.vector(out3), out3_Irina)})

test2 = tryCatch(test_that("Smaller K is correct", {
  expect_equal(rand.index(as.vector(out2), out2_Irina) > 0.99, TRUE)
  expect_equal(rand.index(as.vector(out3), out3_Irina) > 0.99, TRUE)}), error = function(e) 2)


# Does it agree on the ZIPCODE data?
########################################################
library(fossil)
# Load the ZIPCODE data
zipcode <- read.table("ZIPCODE.txt", header = F)
# Extract the true digits
Y <- zipcode[ , 1]
# Extract the data points
X <- as.matrix(zipcode[ , -1])

set.seed(29374)
M <- X[sample(nrow(X), 10), ]

out4 = MyKmeans(X = X, K = 10, M = M, numIter = 15)
out4Irina = IrinasKmeans(X = X, K = 10, M = M, numIter = 15)

# Do they agree? Make below a 3rd test
if (rand.index(out4, out4Irina) > 0.99){
  test3 = 1
}else{
  test3 = 2
}

# Total number of points below should be 45
npoints = 0
if (test1 == 1){
  npoints = npoints + 15
}
if (test2 == 1){
  npoints = npoints + 15
}
if (test3 == 1){
  npoints = npoints + 15
}


print(paste("Total number of correctness points is", npoints, "out of 45 points", sep=" "))


# Check that complains correctly when M is wrong
########################################################
x <- rbind(matrix(rnorm(100, sd = 0.3), ncol = 2),
           matrix(rnorm(100, mean = 1, sd = 0.3), ncol = 2))

print(paste("Additional 5 points if error message on execution below, and the message clearly states what is wrong"))

out1 = MyKmeans(X = x, K = 2, M = matrix(0,1,3), numIter = 100)


# Check speed
########################################################
print("Start timing comparison, may take a lot of time if your code is very slow (mine takes ~5 sec on my machine).")

# Comparison on 100 iterations
library(microbenchmark)
microbenchmark(
  MyKmeans(X, K = 10, M = NULL, numIter = 100),
  IrinasKmeans(X, K = 10, M = NULL, numIter = 100),
  kmeans(X, centers = 10, iter.max = 100),
  times = 10
)

# Comparison on 30 iterations
library(microbenchmark)
microbenchmark(
  MyKmeans(X, K = 10, M = NULL, numIter = 30),
  IrinasKmeans(X, K = 10, M = NULL, numIter = 30),
  kmeans(X, centers = 10, iter.max = 30),
  times = 10
)

print("Based on the MEDIAN timing above for 100 iterations, you get 30 (out of 30) if it's at most 5 times slower than my function, and you get 0 points if it's 35 times slower or more (you loose 1 point for each extra fold over 5 times).")

print("If (your median time)/(my median time) < 0.9 and you got all correctness points, you get extra 5 points as long as the comparison is similar on smaller numIter (i.e. you are not faster due to exiting earlier)")
